package bopdrox.server

import bopdrox.util.{Ack, Connect, FileBytes, FileHash, FLDirectory, FLFile,
                     FSDirectory, FSFile, FSInitMessage, FSListMessage,
                     FSTransferMessage, FSRemovedMessage, FSRequest, FTData,
                     FTDirectory, FTFile, Message, RejDirFile, RejDirNone, Rejection,
                     RejectUpdateMessage, RejFileDir, RejFileFile, RejFileNone, Utils}
import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.Queue
import scala.concurrent.Lock

/* A ClientHandler is a Runnable object designed to handle all communications
 * with a Client */
class ClientHandler (server: Server) (private var client: Socket) (debug: Boolean) extends Runnable {

  val home = server.home

  private[server] val sendQueue = new Queue[Message]

  private var messenger: CHMessenger = null
  private[server] val messengerLock = new Lock

  private var brokenConn = false

  private var in = Utils.getObjectInputStream(client)
  private val out = Utils.getObjectOutputStream(client) // not needed on reconnection

  val getRelPath = Utils.getRelativePath(home) _

  private def disconRead: Option[Object] = Utils.checkedRead(disconnect)(in)
  private val reconRead = Utils.checkedRead(reconnectHandler)_
  private val disconWrite = Utils.checkedWrite(disconnect)(out)_

  // continue running this handler
  private var continue = true

  // TODO(jacob) move this into run()
  val id = disconRead match {
    case Some(Connect(clientId)) => clientId
    case _ => throw new IOException("Something went wrong during Client connection.")
  }


  private def conflictDir (dir: FSDirectory) : FSDirectory = {

    val conflictPath = Utils.conflictedPath(home)(dir.path)
    val fsConflictDir = FSDirectory(conflictPath)

    Utils.ensureDir(home, conflictPath)
    val conflictDir = Utils.newDir(home, fsConflictDir)
    server.hashes(fsConflictDir) = DirData(conflictDir.lastModified)

    fsConflictDir
  }


  private def conflictFile (fsFile: FSFile, contents: FileBytes, hash: Array[FileHash]) : FSFile = {

    val conflictPath = Utils.conflictedPath(home)(fsFile.path)
    val fsConflictFile = FSFile(conflictPath)

    Utils.ensureDir(home, conflictPath)
    val conflictedFile = Utils.newFile(home, fsConflictFile)

    Utils.writeFile(conflictedFile)(contents)
    server.hashes(fsConflictFile) = FileData(conflictedFile.lastModified, hash, Nil)

    fsConflictFile
  }


  // disconnect client and mark handler to terminate
  private def disconnect (ioe: IOException) : Unit = {
    server.dropClient(this)
    continue = false
  }


  /* Handle message matching and file operations. Note that due the variable
   * nature of network communications it does not make sense to enforce FIFO
   * ordering of message handling, only that the file operation + hash map
   * update is atomic. */
  private def matchMessage (msg: Message) : Unit = msg match {

    case FSTransferMessage(ftData) => {

      trait TransferResult
      case object TransNothing extends TransferResult
      case class TransConflict (reject: Rejection, original: Option[FTData], conflict: FTData)
          extends TransferResult
      case class TransSuccess (good: FTData) extends TransferResult

      val result = ftData match {

        case FTDirectory(dir, oldFSObj) => server.hashes.synchronized {
          (server.hashes.lookupPath(dir.path), oldFSObj) match {

            case (None, None) => {
              val emptyDir = Utils.newDir(home, dir)
              server.hashes(dir) = DirData(emptyDir.lastModified)
              TransSuccess(ftData)
            }

            case (Some(DirData(_)), Some(FLDirectory(_))) => TransNothing
              // nothing to do here, transfer redundant

            case (Some(fData: FileData), Some(flFile: FLFile)) => {

              if (Utils.verifyHash(fData.hash)(flFile.hash)) {
                val emptyDir = Utils.newDir(home, dir)
                server.hashes(dir) = DirData(emptyDir.lastModified)
                TransSuccess(ftData)
              }

              else {
                val fsConflictDir = conflictDir(dir)
                val fsOrigFile = FSFile(dir.path)

                TransConflict(
                  RejDirFile(dir, fsOrigFile, fData.hash),
                  Some(FTFile(fsOrigFile, null, fData.hash, Some(FLDirectory(dir)))),
                  FTDirectory(fsConflictDir, None)
                )
              }
            }

            case (None, Some(_)) => {
              val fsConflictDir = conflictDir(dir)
              TransConflict(RejDirNone(dir), None, FTDirectory(fsConflictDir, None))
            }

            case (Some(DirData(_)), Some(FLFile(_, _))) => TransNothing
              // nothing to do here, self-correcting error

            case (Some(fData: FileData), Some(flDir: FLDirectory)) => {

              val fsConflictDir = conflictDir(dir)
              val fsOrigFile = FSFile(dir.path)

              TransConflict(
                RejDirFile(dir, fsOrigFile, fData.hash),
                Some(FTFile(fsOrigFile, null, fData.hash, Some(FLDirectory(dir)))),
                FTDirectory(fsConflictDir, None)
              )
            }

            case (Some(DirData(_)), None) => TransNothing
              // nothing to do here, self-correcting error

            case (Some(fData: FileData), None) => {

              val fsConflictDir = conflictDir(dir)
              val fsOrigFile = FSFile(dir.path)

              TransConflict(
                RejDirFile(dir, fsOrigFile, fData.hash),
                Some(FTFile(fsOrigFile, null, fData.hash, Some(FLDirectory(dir)))),
                FTDirectory(fsConflictDir, None)
              )
            }

          }

        }

        case ftFile @ FTFile(fsFile, contents, position, hash, oldFSObj) => server.hashes.synchronized {
          (server.hashes.lookupPath(fsFile.path), oldFSObj) match {

            case (None, None) => {

              Utils.ensureDir(home, fsFile.path)
              Utils.writeChunk(home, ftFile)
              server.hashes(fsFile) = FileData(file.lastModified, hash, Nil)

              TransSuccess(ftData)
            }

            case (Some(dData: DirData), Some(flDir: FLDirectory)) => {

              val file = Utils.newFile(home, fsFile)
              file.delete

              Utils.writeFile(file)(contents)
              server.hashes(fsFile) = FileData(file.lastModified, hash, Nil)

              TransSuccess(ftData)
            }

            case (Some(fData: FileData), Some(flFile: FLFile)) => {

              if (Utils.verifyHash(fData.hash)(flFile.hash)) {
                val file = Utils.newFile(home, fsFile)
                val chain = (fData.time, fData.hash)::fData.chain

                Utils.writeFile(file)(contents)
                server.hashes(fsFile) = FileData(file.lastModified, hash, chain)

                TransSuccess(ftData)
              }

              else {
                val fsConflictFile = conflictFile(fsFile, contents, hash)
                val origContents = Utils.readFile(home, fsFile.path)

                TransConflict(
                  RejFileFile(fsFile, hash, fData.hash),
                  Some(FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))),
                  FTFile(fsConflictFile, contents, hash, None)
                )
              }
            }

            case (None, Some(_)) => {
              val fsConflictFile = conflictFile(fsFile, contents, hash)

              TransConflict(
                RejFileNone(fsFile, hash),
                None,
                FTFile(fsConflictFile, contents, hash, None)
              )
            }

            case (Some(dData: DirData), Some(flFile: FLFile)) => {

              val fsConflictFile = conflictFile(fsFile, contents, hash)
              val origDir = FSDirectory(fsFile.path)

              TransConflict(
                RejFileDir(fsFile, hash, origDir),
                Some(FTDirectory(origDir, Some(FLFile(fsFile, hash)))),
                FTFile(fsConflictFile, contents, hash, None)
              )
            }

            case (Some(fData: FileData), Some(flDir: FLDirectory)) => {

              val fsConflictFile = conflictFile(fsFile, contents, hash)
              val origContents = Utils.readFile(home, fsFile.path)

              TransConflict(
                RejFileFile(fsFile, hash, fData.hash),
                Some(FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))),
                FTFile(fsConflictFile, contents, hash, None)
              )
            }

            case (Some(dData: DirData), None) => {

              val fsConflictFile = conflictFile(fsFile, contents, hash)
              val origDir = FSDirectory(fsFile.path)

              TransConflict(
                RejFileDir(fsFile, hash, origDir),
                Some(FTDirectory(origDir, Some(FLFile(fsFile, hash)))),
                FTFile(fsConflictFile, contents, hash, None)
              )
            }

            case (Some(fData: FileData), None) => {

              val fsConflictFile = conflictFile(fsFile, contents, hash)
              val origContents = Utils.readFile(home, fsFile.path)

              TransConflict(
                RejFileFile(fsFile, hash, fData.hash),
                Some(FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))),
                FTFile(fsConflictFile, contents, hash, None)
              )
            }
          }
        }
      }

      result match {
        case TransNothing => ()

        case TransSuccess(good) => server.clients.synchronized {
          server.clients.foreach { kv =>
            if (kv._1 != id) {
              kv._2.message(FSTransferMessage(good))
            }
          }
        }

        case TransConflict(reject, original, conflict) => {
          message(RejectUpdateMessage(reject))
          original.foreach(orig => message(FSTransferMessage(orig)))
          message(FSTransferMessage(conflict))

          server.clients.synchronized {
            server.clients.foreach { kv =>
              if (kv._1 != id) {
                kv._2.message(FSTransferMessage(conflict))
              }
            }
          }
        }
      }

    }

    case FSRemovedMessage(removed) => {

      removed.foreach {
        _ match {
          case flDir: FLDirectory => server.hashes.synchronized {
            server.hashes.remove(flDir.dir)
            val rem = Utils.newFile(home, FSFile(flDir.dir.path))
            Utils.dirDelete(rem)
          }

          // TODO(jacob) should check hashes here
          case flFile: FLFile => server.hashes.synchronized {
            server.hashes.remove(flFile.file)
            val rem = Utils.newFile(home, flFile.file)
            Utils.dirDelete(rem)
          }
        }
      }

      // forward message
      server.clients.synchronized {
        server.clients.foreach { kv =>
          if (kv._1 != id) {
            kv._2.message(msg)
          }
        }
      }

    }

    case _ => throw new IOException("Unknown or incorrect message received")
  }


  def message (msg: Message) : Unit = sendQueue.synchronized {
    sendQueue.enqueue(msg)
  }


  def reconnect (newClient: Socket) : Unit = {

    client = newClient
    in = Utils.getObjectInputStream(client)

    messenger = new CHMessenger(this)(out)(debug)
    new Thread(messenger).start

    brokenConn = false

  }


  private def reconnectHandler (ioe: IOException) : Unit = {
    println("Something went horribly wrong. Waiting for Client to reconnect...")

    messenger.continue = false
    brokenConn = true
  }


  def run : Unit = {

    println("client connected: " + Utils.printSocket(client))

    // send client a list of file names and hashes
    val listMsg = FSListMessage(server.hashes.flList)
    disconWrite(listMsg)

    disconRead match {

      case None => () // wait for termination

      case Some(FSRequest(files)) => {

        val fileList = files.map {
          _ match {
            // TODO(jacob) just send None for oldFSObj? doesn't fit well
            case fsDir: FSDirectory => FTDirectory(fsDir, None)
            case fsFile: FSFile => {
              val contents = Utils.readFile(home, fsFile.path)
              val hash = server.hashes.applyFile(fsFile).hash
              FTFile(fsFile, contents, hash, None)
            }
          }
        }

        val msg = FSInitMessage(fileList)
        disconWrite(msg)

        disconRead match {
          case None => () // wait for termination
          case Some(Ack) => ()
          case Some(_) => throw new IOException("Unknown or incorrect message received")
        }
      }

      case Some(_) => throw new IOException("Unknown or incorrect message received")
    }

    messenger = new CHMessenger(this)(out)(debug)
    new Thread(messenger).start

    // main loop to listen for updated files
    while (continue) {

      // if some error arises we must wait for the Client to reconnect
      while (brokenConn) {
        Thread.sleep(100)
      }

      reconRead(in) match {
        case None => () // wait for termination

        case Some(msg: Message) => {
          if (debug) {
            println("DEBUG - Server received Message from " + Utils.printSocket(client) + ":\n\t" + msg)
          }

          matchMessage(msg)
        }

        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

    messenger.continue = false

  }
}
