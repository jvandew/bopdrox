package bopdrox.server

import bopdrox.util.{Ack, FileBytes, FileHash, FLDirectory, FLFile, FSDirectory,
                     FSFile, FSListMessage, FSTransferMessage, FSRemovedMessage,
                     FSRequest, FTData, FTDirectory, FTFile, Message, RejDirFile,
                     RejDirNone, Rejection, RejectUpdateMessage, RejFileDir,
                     RejFileFile, RejFileNone, Utils}
import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.Queue
import scala.concurrent.Lock

/* A ClientHandler is a Runnable object designed to handle all communications
 * with a Client */
class ClientHandler (server: Server) (private var client: Socket) extends Runnable {

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


  private def conflictDir (dir: FSDirectory) : FSDirectory = {

    val conflictPath = Utils.conflictedPath(home)(dir.path)
    val fsConflictDir = FSDirectory(conflictPath)

    Utils.ensureDir(home, conflictPath)
    val conflictDir = Utils.newDir(home, fsConflictDir)
    server.hashes(fsConflictDir) = DirData(conflictDir.lastModified)

    fsConflictDir
  }


  private def conflictFile (fsFile: FSFile, contents: FileBytes, hash: FileHash) : FSFile = {

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

    case FSTransferMessage(ftList) => {

      var rejections = List[Rejection]()
      var originals = List[FTData]()
      var conflictCopies = List[FTData]()
      var good = List[FTData]()

      ftList.foreach {
        _ match {
          case FTDirectory(dir, oldFSObj) => server.hashes.synchronized {
            (server.hashes.lookupPath(dir.path), oldFSObj) match {

              case (None, None) => {

                val emptyDir = Utils.newDir(home, dir)
                server.hashes(dir) = DirData(emptyDir.lastModified)

                good ::= FTDirectory(dir, oldFSObj)
              }

              case (Some(DirData(_)), Some(FLDirectory(_))) => ()
                // nothing to do here, transfer redundant

              case (Some(fData: FileData), Some(flFile: FLFile)) => {

                if (Utils.verifyHash(fData.hash)(flFile.hash)) {
                  val emptyDir = Utils.newDir(home, dir)
                  server.hashes(dir) = DirData(emptyDir.lastModified)

                  good ::= FTDirectory(dir, oldFSObj)
                }

                else {
                  val fsConflictDir = conflictDir(dir)
                  val fsOrigFile = FSFile(dir.path)
                  val contents = Utils.readFile(home, dir.path)

                  conflictCopies ::= FTDirectory(fsConflictDir, None)
                  originals ::= FTFile(fsOrigFile, contents, fData.hash, Some(FLDirectory(dir)))
                  rejections ::= RejDirFile(dir, fsOrigFile, fData.hash)
                }
              }

              case (None, Some(_)) => {

                val fsConflictDir = conflictDir(dir)

                conflictCopies ::= FTDirectory(fsConflictDir, None)
                rejections ::= RejDirNone(dir)
              }

              case (Some(DirData(_)), Some(FLFile(_, _))) => ()
                // nothing to do here, self-correcting error

              case (Some(fData: FileData), Some(flDir: FLDirectory)) => {

                val fsConflictDir = conflictDir(dir)
                val fsOrigFile = FSFile(dir.path)
                val contents = Utils.readFile(home, dir.path)

                conflictCopies ::= FTDirectory(fsConflictDir, None)
                originals ::= FTFile(fsOrigFile, contents, fData.hash, Some(FLDirectory(dir)))
                rejections ::= RejDirFile(dir, fsOrigFile, fData.hash)
              }

              case (Some(DirData(_)), None) => ()
                // nothing to do here, self-correcting error

              case (Some(fData: FileData), None) => {

                val fsConflictDir = conflictDir(dir)
                val fsOrigFile = FSFile(dir.path)
                val contents = Utils.readFile(home, dir.path)

                conflictCopies ::= FTDirectory(fsConflictDir, None)
                originals ::= FTFile(fsOrigFile, contents, fData.hash, Some(FLDirectory(dir)))
                rejections ::= RejDirFile(dir, fsOrigFile, fData.hash)
              }

            }

          }

          case FTFile(fsFile, contents, hash, oldFSObj) => server.hashes.synchronized {
            (server.hashes.lookupPath(fsFile.path), oldFSObj) match {

              case (None, None) => {

                Utils.ensureDir(home, fsFile.path)
                val file = Utils.newFile(home, fsFile)

                Utils.writeFile(file)(contents)
                server.hashes(fsFile) = FileData(file.lastModified, hash, Nil)

                good ::= FTFile(fsFile, contents, hash, oldFSObj)
              }

              case (Some(dData: DirData), Some(flDir: FLDirectory)) => {

                val file = Utils.newFile(home, fsFile)
                file.delete

                Utils.writeFile(file)(contents)
                server.hashes(fsFile) = FileData(file.lastModified, hash, Nil)

                good ::= FTFile(fsFile, contents, hash, oldFSObj)
              }

              case (Some(fData: FileData), Some(flFile: FLFile)) => {

                if (Utils.verifyHash(fData.hash)(flFile.hash)) {
                  val file = Utils.newFile(home, fsFile)
                  val chain = (fData.time, fData.hash)::fData.chain

                  Utils.writeFile(file)(contents)
                  server.hashes(fsFile) = FileData(file.lastModified, hash, chain)

                  good ::= FTFile(fsFile, contents, hash, oldFSObj)
                }

                else {
                  val fsConflictFile = conflictFile(fsFile, contents, hash)
                  val origContents = Utils.readFile(home, fsFile.path)

                  conflictCopies ::= FTFile(fsConflictFile, contents, hash, None)
                  originals ::= FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))
                  rejections ::= RejFileFile(fsFile, hash, fData.hash)
                }
              }

              case (None, Some(_)) => {

                val fsConflictFile = conflictFile(fsFile, contents, hash)

                conflictCopies ::= FTFile(fsConflictFile, contents, hash, None)
                rejections ::= RejFileNone(fsFile, hash)
              }

              case (Some(dData: DirData), Some(flFile: FLFile)) => {

                val fsConflictFile = conflictFile(fsFile, contents, hash)
                val origDir = FSDirectory(fsFile.path)

                conflictCopies ::= FTFile(fsConflictFile, contents, hash, None)
                originals ::= FTDirectory(origDir, Some(FLFile(fsFile, hash)))
                rejections ::= RejFileDir(fsFile, hash, origDir)
              }

              case (Some(fData: FileData), Some(flDir: FLDirectory)) => {

                val fsConflictFile = conflictFile(fsFile, contents, hash)
                val origContents = Utils.readFile(home, fsFile.path)

                conflictCopies ::= FTFile(fsConflictFile, contents, hash, None)
                originals ::= FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))
                rejections ::= RejFileFile(fsFile, hash, fData.hash)
              }

              case (Some(dData: DirData), None) => {

                val fsConflictFile = conflictFile(fsFile, contents, hash)
                val origDir = FSDirectory(fsFile.path)

                conflictCopies ::= FTFile(fsConflictFile, contents, hash, None)
                originals ::= FTDirectory(origDir, Some(FLFile(fsFile, hash)))
                rejections ::= RejFileDir(fsFile, hash, origDir)
              }

              case (Some(fData: FileData), None) => {

                val fsConflictFile = conflictFile(fsFile, contents, hash)
                val origContents = Utils.readFile(home, fsFile.path)

                conflictCopies ::= FTFile(fsConflictFile, contents, hash, None)
                originals ::= FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))
                rejections ::= RejFileFile(fsFile, hash, fData.hash)
              }

            }

          }

        }
      }

      rejections match {
        case Nil => server.synchronized {
          server.clients.foreach { c =>
            if (!c.equals(this))
              c.message(msg)
          }
        }
        case _ => {
          message(RejectUpdateMessage(rejections))
          message(FSTransferMessage(originals ++ conflictCopies))
          server.synchronized {
            server.clients.foreach { c =>
              if (!c.equals(this))
                c.message(FSTransferMessage(good ++ conflictCopies))
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
      server.synchronized {
        server.clients.foreach { c =>
          if (!c.equals(this))
            c.message(msg)
        }
      }

    }

    case _ => throw new IOException("Unknown or incorrect message received")
  }


  def message (msg: Message) : Unit = disconWrite(msg)


  def reconnect (newClient: Socket) : Unit = {

    client = newClient
    in = Utils.getObjectInputStream(client)

    messenger = new CHMessenger(this)(out)
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

        val msg = FSTransferMessage(fileList)
        disconWrite(msg)

        disconRead match {
          case None => () // wait for termination
          case Some(Ack) => ()
          case Some(_) => throw new IOException("Unknown or incorrect message received")
        }
      }
      case Some(_) => throw new IOException("Unknown or incorrect message received")
    }

    messenger = new CHMessenger(this)(out)
    new Thread(messenger).start

    // main loop to listen for updated files
    while (continue) {

      // if some error arises we must wait for the Client to reconnect
      while (brokenConn) {
        Thread.sleep(100)
      }

      reconRead(in) match {
        case None => () // wait for termination
        case Some(msg: Message) => matchMessage(msg)
        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}
