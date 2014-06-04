package bopdrox.server

import bopdrox.util.{Ack, FSListMessage, FSTransferMessage, FSRemovedMessage,
                     FSRequest, FTData, Rejection, RejectUpdateMessage, Utils}
import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket

/* A ClientHandler is a Runnable object designed to handle all communications
 * with a Client */
class ClientHandler (server: Server) (client: Socket) extends Runnable {

  val home = server.home

  private val in = new ObjectInputStream(client.getInputStream)
  private val out = new ObjectOutputStream(client.getOutputStream)

  val getRelPath = Utils.getRelativePath(home) _

  // continue running this handler
  private var continue = true

  // disconnect client and mark handler to terminate
  private def disconnect (ioe: IOException) : Unit = {
    server.dropClient(this)
    continue = false
  }

  private def readObject: Option[Object] = Utils.checkedRead(disconnect)(in)
  private val writeObject = Utils.checkedWrite(disconnect)(out)_

  /* Handle message matching and file operations. Note that due the variable
   * nature of network communications it does not make sense to enforce FIFO
   * ordering of message handling, only that the file operation + hash map
   * update is atomic. */
  private def matchMessage (msg: Message) : Unit = msg match {

    case FSTransferMessage(fsList) => {

      var rejections = List[Rejection]()
      var originals = List[FTData]()
      var conflictCopies = List[FTData]()
      var good = List[FTData]()

      fsList.foreach {
        _ match {
          case FTDirectory(dir, oldFSObj) => server.hashes.synchronized {
            (server.hashes.lookupPath(dir.path), oldFSObj) match {

              case (None, None) => {
                val emptyDir = Utils.newDir(home, dir.path)
                server.hashes(dir) = DirData(emptyDir.lastModified)
                good = FTDirectory(dir, oldFSObj)::good
              }

              case (Some(dData: DirData), Some(flDir: FLDirectory)) => ()
                // nothing to do here, transfer redundant

              case (Some(fData: FileData), Some(flFile: FLFile)) => {

                if (Utils.verifyHash(fData.hash, flFile.hash)) {
                  val emptyDir = Utils.newDir(home, dir.path)
                  server.hashes(dir) = emptyDir.lastModified
                  good = FTDirectory(dir, oldFSObj)::good
                }

                else {
                  var conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc")
                  var conflictCount = 0
                  while (Utils.newFile(home, conflictPath).exists) {
                    conflictCount += 1
                    conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc" + conflictCount)
                  }

                  val fsConflictDir = FSDirectory(conflictPath)
                  val conflictDir = Utils.newDir(home, conflictPath)
                  server.hashes(fsConflictDir) = DirData(conflictDir.lastModified)

                  val fsOrigFile = FSFile(dir.path)
                  val contents = Utils.readFile(home, dir.path)

                  val conflict = FTDirectory(fsConflictDir, None)
                  val original = FTFile(fsOrigFile, contents, fData.hash, Some(FLDirectory(dir)))
                  val reject = RejDirFile(dir, fsOrigFile, fData.hash)

                  conflictCopies = conflict::conflictCopies
                  originals = original::originals
                  rejections = reject::rejections
                }

              }

              case (None, Some(_)) => {

                var conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc" + conflictCount)
                }

                val fsConflictDir = FSDirectory(conflictPath)
                val conflictDir = Utils.newDir(home, conflictPath)
                server.hashes(fsConflictDir) = DirData(conflictDir.lastModified)

                val conflict = FTDirectory(fsConflictDir, None)
                val reject = RejDirNone(dir)

                conflictCopies = conflict::conflictCopies
                rejections = reject::rejections
              }

              case (Some(dData: DirData), Some(flFile: FLFile)) => ()
                // nothing to do here, self-correcting error

              case (Some(fData: FileData), Some(flDir: FLDirectory)) => {

                var conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc" + conflictCount)
                }

                val fsConflictDir = FSDirectory(conflictPath)
                val conflictDir = Utils.newDir(home, conflictPath)
                server.hashes(fsConflictDir) = DirData(conflictDir.lastModified)

                val fsOrigFile = FSFile(dir.path)
                val contents = Utils.readFile(home, dir.path)

                val conflict = FTDirectory(fsConflictDir, None)
                val original = FTFile(fsOrigFile, contents, fData.hash, Some(FLDirectory(dir)))
                val reject = RejDirFile(dir, fsOrigFile, fData.hash)

                conflictCopies = conflict::conflictCopies
                originals = original::originals
                rejections = reject::rejections
              }

              case (Some(dData: DirData), None) => ()
                // nothing to do here, self-correcting error

              case (Some(fData: FileData), None) => {

                var conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = dir.path.updated(dir.path.size - 1, dir.path.last + "-cc" + conflictCount)
                }

                val fsConflictDir = FSDirectory(conflictPath)
                val conflictDir = Utils.newDir(home, conflictPath)
                server.hashes(fsConflictDir) = DirData(conflictDir.lastModified)

                val fsOrigFile = FSFile(dir.path)
                val contents = Utils.readFile(home, dir.path)

                val conflict = FTDirectory(fsConflictDir, None)
                val original = FTFile(fsOrigFile, contents, fData.hash, Some(FLDirectory(dir)))
                val reject = RejDirFile(dir, fsOrigFile, fData.hash)

                conflictCopies = conflict::conflictCopies
                originals = original::originals
                rejections = reject::rejections
              }

            }

          }

          case FTFile(fsFile, contents, hash, oldFSObj) => server.hashes.synchronized {
            (server.hashes.lookupPath(file.path), oldFSObj) match {

              case (None, None) => {
                Utils.ensureDir(home, fsFile.path)
                val file = Utils.newFile(home, fsFile.path)

                Utils.writeFile(file)(contents)
                server.hashes(fsFile) = FileData(file.lastModified, hash, Nil)
                good = FTFile(file, contents, hash, oldFSObj)::good
              }

              case (Some(dData: DirData), Some(flDir: FLDirectory)) => {
                val file = Utils.newFile(home, fsFile.path)
                file.delete

                Utils.writeFile(file)(contents)
                server.hashes(fsFile) = FileData(file.lastModified, hash, Nil)
                good = FTFile(file, contents, hash, oldFSObj)::good
              }

              case (Some(fData: FileData), Some(flFile: FLFile)) => {

                if (Utils.verifyHash(fData.hash, flFile.hash)) {
                  val file = Utils.newFile(home, fsFile.path)
                  val chain = (fData.time, fData.hash)::fData.chain

                  Utils.writeFile(file)(contents)
                  server.hashes(fsFile) = FileData(file.lastModified, hash, chain)
                  good = FTFile(file, contents, hash, oldFSObj)::good
                }

                else {
                  var conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc")
                  var conflictCount = 0
                  while (Utils.newFile(home, conflictPath).exists) {
                    conflictCount += 1
                    conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc" + conflictCount)
                  }

                  val fsConflictFile = FSFile(conflictPath)
                  val conflictFile = Utils.newFile(home, conflictPath)
                  Utils.writeFile(conflictFile)(contents)
                  server.hashes(fsConflictFile) = FileData(conflictFile.lastModified, hash, Nil)

                  val origContents = Utils.readFile(home, fsFile.path)

                  val conflict = FTFile(fsConflictFile, contents, hash, None)
                  val original = FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))
                  val reject = RejFileFile(fsFile, hash, fData.hash)

                  conflictCopies = conflict::conflictCopies
                  originals = original::originals
                  rejections = reject::rejections
                }

              }

              case (None, Some(_)) => {

                var conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc" + conflictCount)
                }

                val fsConflictFile = FSFile(conflictPath)
                Utils.ensureDir(home, conflictPath)
                val conflictFile = Utils.newFile(home, conflictPath)
                Utils.writeFile(conflictFile)(contents)
                server.hashes(fsConflictFile) = FileData(conflictFile.lastModified, hash, Nil)

                val conflict = FTFile(fsConflictFile, contents, hash, None)
                val reject = RejDirNone(dir)

                conflictCopies = conflict::conflictCopies
                rejections = reject::rejections
              }

              case (Some(dData: DirData), Some(flFile: FLFile)) => {

                var conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc" + conflictCount)
                }

                val fsConflictFile = FSFile(conflictPath)
                val conflictFile = Utils.newFile(home, conflictPath)
                Utils.writeFile(conflictFile)(contents)
                server.hashes(fsConflictFile) = FileData(conflictFile.lastModified, hash, Nil)

                val origDir = FSDirectory(fsFile.path)

                val conflict = FTFile(fsConflictFile, contents, hash, None)
                val original = FTDirectory(origDir, Some(FLFile(fsFile, hash)))
                val reject = RejFileDir(fsFile, hash, origDir)

                conflictCopies = conflict::conflictCopies
                originals = original::originals
                rejections = reject::rejections
              }

              case (Some(fData: FileData), Some(flDir: FLDirectory)) => {

                var conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc" + conflictCount)
                }

                val fsConflictFile = FSFile(conflictPath)
                val conflictFile = Utils.newFile(home, conflictPath)
                Utils.writeFile(conflictFile)(contents)
                server.hashes(fsConflictFile) = FileData(conflictFile.lastModified, hash, Nil)

                val origContents = Utils.readFile(home, dir.path)

                val conflict = FTFile(fsConflictFile, contents, hash, None)
                val original = FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))
                val reject = RejFileFile(fsFile, hash, fData.hash)

                conflictCopies = conflict::conflictCopies
                originals = original::originals
                rejections = reject::rejections
              }

              case (Some(dData: DirData), None) => {

                var conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc" + conflictCount)
                }

                val fsConflictFile = FSFile(conflictPath)
                val conflictFile = Utils.newFile(home, conflictPath)
                Utils.writeFile(conflictFile)(contents)
                server.hashes(fsConflictFile) = FileData(conflictFile.lastModified, hash, Nil)

                val origDir = FSDirectory(fsFile.path)

                val conflict = FTFile(fsConflictFile, contents, hash, None)
                val original = FTDirectory(origDir, Some(FLFile(fsFile, hash)))
                val reject = RejFileDir(fsFile, hash, origDir)

                conflictCopies = conflict::conflictCopies
                originals = original::originals
                rejections = reject::rejections
              }

              case (Some(fData: FileData), None) => {

                var conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc")
                var conflictCount = 0
                while (Utils.newFile(home, conflictPath).exists) {
                  conflictCount += 1
                  conflictPath = fsFile.path.updated(fsFile.path.size - 1, fsFile.path.last + "-cc" + conflictCount)
                }

                val fsConflictFile = FSFile(conflictPath)
                val conflictFile = Utils.newFile(home, conflictPath)
                Utils.writeFile(conflictFile)(contents)
                server.hashes(fsConflictFile) = FileData(conflictFile.lastModified, hash, Nil)

                val origContents = Utils.readFile(home, fsFile.path)

                val conflict = FTFile(fsConflictFile, contents, hash, None)
                val original = FTFile(fsFile, origContents, fData.hash, Some(FLFile(fsFile, hash)))
                val reject = RejFileFile(fsFile, hash, fData.hash)

                conflictCopies = conflict::conflictCopies
                originals = original::originals
                rejections = reject::rejections
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
        case _ => server.synchronized {
          message(RejectUpdateMessage(rejections))
          message(FSTransferMessage(originals ++ conflictCopies))
          server.clients.foreach { c =>
            if (!c.equals(this))
              c.message(FSTransferMessage(good ++ conflictCopies))
          }
        }
      }

    }

    case FSRemovedMessage(removed) => {

      removed.foreach {
        _ match {
          case flDir: FLDirectory => server.hashes.synchronized {
            server.hashes.remove(flDir.dir)
            val rem = Utils.newFile(home, flDir.dir.path)
            Utils.dirDelete(rem)
          }

          // TODO(jacob) should check hashes here
          case flFile: FLFile => server.hashes.synchronized {
            server.hashes.remove(flFile.file)
            val rem = Utils.newFile(home, flFile.file.path)
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

  def message (msg: Message) : Unit = writeObject(msg)

  def run : Unit = {

    println("client connected: " + Utils.printSocket(client))

    // send client a list of file names and hashes
    val listMsg = FSListMessage(server.hashes.flList)
    writeObject(listMsg)

    readObject match {
      case None => () // wait for termination
      case Some(FSRequest(files)) => {

        val fileList = files.map {
          _ match {
            // TODO(jacob) just send None for oldFSObj? doesn't fit well
            case fsDir: FSDirectory => FTDirectory(fsDir, None)
            case fsFile: FSFile => {
              val contents = Utils.readFile(home, fsFile.path)
              val hash = server.hashes(fsFile).hash
              FTFile(fsFile, contents, hash)
            }
          }
        }

        val msg = FSTransferMessage(fileList)
        writeObject(msg)

        readObject match {
          case None => () // wait for termination
          case Some(Ack) => ()
          case Some(_) => throw new IOException("Unknown or incorrect message received")
        }
      }
      case Some(_) => throw new IOException("Unknown or incorrect message received")
    }

    // main loop to listen for updated files
    while (continue) {
      readObject match {
        case None => () // wait for termination
        case Some(msg: Message) => matchMessage(msg)
        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}