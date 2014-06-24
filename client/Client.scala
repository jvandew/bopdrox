package bopdrox.client

import bopdrox.util.{Ack, FLData, FLDirectory, FLFile, FSDirectory, FSFile,
                     FSListMessage, FSRemovedMessage, FSRequest,
                     FSTransferMessage, FTData, FTDirectory, FTFile, Message,
                     RejectUpdateMessage, Utils}
import java.io.{File, FileInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.{HashMap, Queue}

// companion object for running a Client
object Client {

  /* Takes a home folder and the address of a running Server */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val Array(host, port) = args(1).split(':')

    val client = new Client(home)(host)(port.toInt)
    client.run
  }
}

class Client (val home: File) (host: String) (port: Int) extends Runnable {

  val hashes = new ClientMap

  val getRelPath = Utils.getRelativePath(home)_

  // store received Messages for later consumption
  val messageQueue = new Queue[Message]

  private var open = false


  // disconnect handler
  private[client] def disconnect (ioe: IOException) : Unit = {
    println("IOException received! Initiating hara-kiri...")
    ioe.printStackTrace
    sys.exit
  }


  def isOpen : Boolean = open


  // note this method is not called asnchronously
  private def matchMessage (msg: Message) : Unit = msg match {

    case FSTransferMessage(ftList) => {

      ftList.foreach {
        _ match {

          // TODO(jacob) should probably check oldFSObj eventually
          case FTDirectory(dir, _) => {
            val emptyDir = Utils.newDir(home, dir)
            hashes(dir) = DirData(emptyDir.lastModified)
          }

          // TODO(jacob) should probably check oldFSObj eventually
          case FTFile(fsFile, contents, hash, _) => {
            val file = Utils.newFile(home, fsFile)

            hashes.lookupPath(fsFile.path) match {
              case None => Utils.ensureDir(home, fsFile.path)
              case Some(FileData(_, _)) => ()
              case Some(DirData(_)) => file.delete
            }

            Utils.writeFile(file)(contents)
            hashes(fsFile) = FileData(file.lastModified, hash)
          }
        }

      }
    }

    case RejectUpdateMessage(rejections) => ()  // will receive corrections from Server momentarily

    case FSRemovedMessage(removed) => {

      removed.foreach { rem =>
        val (fsObj, file) = rem match {
          case FLFile(fsFile, _) => (fsFile, Utils.newFile(home, fsFile))
          case FLDirectory(fsDir) => (fsDir, Utils.newDir(home, fsDir))
        }

        hashes.remove(fsObj)
        Utils.dirDelete(file)
      }
    }

    case _ => throw new IOException("Unknown or incorrect message received")
  }


  def run : Unit = {

    val serv = new Socket(host, port)
    val out = new ObjectOutputStream(serv.getOutputStream)
    val in = new ObjectInputStream(serv.getInputStream)

    def readObject: Option[Object] = Utils.checkedRead(disconnect)(in)
    val writeObject = Utils.checkedWrite(disconnect)(out)_

    // get list of files and hashes from server
    // TODO(jacob) currently assumes Client files are a subset of Server files
    readObject match {
      case None => () // wait for termination
      case Some(FSListMessage(fsList)) => {

        // generate local file list and hashes
        Utils.dirForeach(home) { file =>
          val hash = Utils.hashFile(file)
          val time = file.lastModified
          val fsFile = FSFile(getRelPath(file))

          hashes(fsFile) = FileData(time, hash)
        }
        { dir =>
          val time = dir.lastModified
          val fsDir = FSDirectory(getRelPath(dir))

          hashes(fsDir) = DirData(time)
        }

        val filtered = fsList.filter { flData =>
          (flData, hashes.lookupPath(flData.fsObj.path)) match {
            case (_, None) => true
            case (FLDirectory(_), Some(DirData(_))) => false
            case (FLDirectory(_), Some(FileData(_, _))) => true
            case (FLFile(_, _), Some(DirData(_))) => true
            case (FLFile(_, hash1), Some(FileData(_, hash2))) => !Utils.verifyHash(hash1)(hash2)
          }
        }

        val msg = FSRequest(filtered.map(_.fsObj))
        writeObject(msg)

        readObject match {
          case None => () // wait for termination
          case Some(FSTransferMessage(ftList)) => {

            // Process list of new files
            ftList.foreach {
              _ match {

                case FTDirectory(dir, _) => {
                  val emptyDir = Utils.newDir(home, dir)
                  hashes(dir) = DirData(emptyDir.lastModified)
                }

                case FTFile(fsFile, contents, hash, _) => {

                  Utils.ensureDir(home, fsFile.path)
                  val file = Utils.newFile(home, fsFile)

                  Utils.writeFile(file)(contents)
                  hashes(fsFile) = FileData(file.lastModified, hash)
                }
              }
            }

            writeObject(Ack)
          }
          case Some(_) => throw new IOException("Unknown or incorrect message received")
        }
      }
      case Some(_) => throw new IOException("Unknown or incorrect message received")
    }

    // begin listener thread
    new Thread(new ClientListener(this)(in)).start
    open = true

    // main loop to check for updated files
    while (true) {

      // process any received Messages
      // TODO(jacob) this could be an inconvenient way of handling Messages if we have a
      // large Queue (unlikely with a single user) or a significantly large directory tree
      while (!messageQueue.isEmpty)
        matchMessage(messageQueue.dequeue)

      var keySet = hashes.keySet
      var updates = List[FTData]()

      Utils.dirForeach(home) { file =>

        val path = getRelPath(file)
        val fsFile = FSFile(path)

        hashes.lookupPath(path) match {

          case None => {
            val (bytes, hash) = Utils.contentsAndHash(file)

            hashes(fsFile) = FileData(file.lastModified, hash)
            updates ::= FTFile(fsFile, bytes, hash, None)
          }

          case Some(fileData: FileData) => {

            // TODO(jacob) it's extremely unlikely that changing the system
            //             clock could break this check
            if (fileData.time != file.lastModified) {

              val oldFSObj = FLFile(fsFile, fileData.hash)
              val (bytes, hash) = Utils.contentsAndHash(file)

              hashes(fsFile) = FileData(file.lastModified, hash)
              updates ::= FTFile(fsFile, bytes, hash, Some(oldFSObj))
              keySet -= fsFile
            }
            else {
              keySet -= fsFile
            }
          }

          case Some(dirData) => {

            val fsDir = FSDirectory(path)
            val oldFSObj = FLDirectory(fsDir)
            val (bytes, hash) = Utils.contentsAndHash(file)

            hashes.remove(fsDir)
            hashes(fsFile) = FileData(file.lastModified, hash)
            updates ::= FTFile(fsFile, bytes, hash, Some(oldFSObj))
            keySet -= fsDir
          }
        }
      }
      { dir =>

        val path = getRelPath(dir)
        val fsDir = FSDirectory(path)

        hashes.lookupPath(path) match {

          case None => {
            hashes(fsDir) = DirData(dir.lastModified)
            updates ::= FTDirectory(fsDir, None)
          }

          case Some(fileData: FileData) => {

            val fsFile = FSFile(path)
            val oldFSObj = FLFile(fsFile, fileData.hash)

            hashes.remove(fsFile)
            hashes(fsDir) = DirData(dir.lastModified)
            updates ::= FTDirectory(fsDir, Some(oldFSObj))
            keySet -= fsFile
          }

          case Some(dirData) => {
            keySet -= fsDir
          }
        }
      }

      var removed = List[FLData]()
      keySet.foreach { fsObj =>

        hashes.remove(fsObj) match {

          case None => () // SHOULD never happen

          case Some(dirData: DirData) =>
            removed ::= FLDirectory(fsObj.asInstanceOf[FSDirectory])

          case Some(fileData: FileData) =>
            removed ::= FLFile(fsObj.asInstanceOf[FSFile], fileData.hash)
        }

        // determine if we deleted a parent folder or just a file or empty folder
        val path = fsObj.path
        Utils.getDeleted(home, path) match {
          case `path` => ()
          case deletedPath =>
            removed ::= FLDirectory(FSDirectory(fsObj.path))
        }
      }

      updates match {
        case Nil => ()  // no updates
        case _ => {
          val msg = FSTransferMessage(updates)
          writeObject(msg)
        }
      }

      removed match {
        case Nil => ()  // nothing removed
        case _ => {
          val msg = FSRemovedMessage(removed)
          writeObject(msg)
        }
      }

      Thread.sleep(500)
    }

  }
}