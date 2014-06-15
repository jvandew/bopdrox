package bopdrox.client

import bopdrox.msg.{Ack, FileListMessage, FileMessage, FileMsgData, FileRequest,
                    Message, RejectUpdateMessage, RemovedMessage}
import bopdrox.util.Utils
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
  private[client] def disconnect (ioe: IOException) : Unit = sys.exit


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
              case Some(FileData) => ()
              case Some(DirData) => file.delete
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
            case (FLDirectory, Some(DirData)) => false
            case (FLDirectory, Some(FileData)) => true
            case (FLFile, Some(DirData)) => true
            case (FLFile(_, hash1), FileData(_, hash2)) => !Utils.verifyHash(hash1)(hash2)
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

                  Utils.ensureDir(home, subpath)
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
      var updates = List[(List[String], Option[FileMsgData])]()

      // TODO(jacob) doing this asynchronously might be worthwhile. sleep on it
      Utils.dirForeach(home) { file =>
        val path = getRelPath(file)
        keySet = keySet - path

        val (update, oldHash) = {
          if (!hashes.contains(path))
            (true, None)
          else
            hashes(path) match {
              case None => (true, None)   // formerly a directory
              case Some(fileData) =>
                if (fileData.time != file.lastModified)
                  (true, Some(fileData.hash))
                else
                  (false, None)
            }
        }

        if (update) {
          // TODO(jacob) this call is not thread-safe and will very rarely crash the client
          val (bytes, hash) = Utils.contentsAndHash(file)
          hashes.update(path, Some(ClientData(file.lastModified, hash)))
          updates = (path, Some(FileMsgData(bytes, oldHash, hash)))::updates
        }

      }
      { dir =>
        val path = getRelPath(dir)
        keySet = keySet - path

        val update = {
          if (!hashes.contains(path))
            true
          else
            hashes(path) match {
              case None => false
              case Some(fileData) => true   // formerly a file
            }
        }

        if (update) {
          hashes.update(path, None)
          updates = (path, None)::updates
        }
      }

      // add our removed keys and their file hashes to a transfer map
      val keyHashes = new HashMap[List[String], Option[Array[Byte]]]()
      keySet.foreach { key =>

        val oldData = hashes.remove(key)

        // determine if we deleted a parent folder or just a file or empty folder
        Utils.getDeleted(home, key) match {
          case `key` => oldData match {
            case None => () // not in hashmap
            case Some(None) => keyHashes.update(key, None)  // directory
            case Some(Some(fileData)) => keyHashes.update(key, Some(fileData.hash)) // file
          }
          case deleted => keyHashes.update(deleted, None) // parent folder
        }
      }

      updates match {
        case Nil => ()  // no updates
        case _ => {
          val msg = FileMessage(updates)
          writeObject(msg)
        }
      }

      if (!keyHashes.isEmpty) {
        val msg = RemovedMessage(keyHashes)
        writeObject(msg)
      }

      Thread.sleep(500)
    }

  }
}