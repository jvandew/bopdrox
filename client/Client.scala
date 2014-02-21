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

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[List[String], Option[ClientData]]

  val getRelPath = Utils.getRelativePath(home)_

  // store received Messages for later consumption
  val messageQueue = new Queue[Message]

  private var open = false

  // disconnect handler
  private[client] def disconnect (ioe: IOException) : Unit = sys.exit

  var sock: Socket = null

  def isOpen : Boolean = open

  // note this method is not called asnchronously
  private def matchMessage (msg: Message) : Unit = msg match {

    case FileMessage(fileContents) => {

      // TODO(jacob) this code is mostly copy-pasted from ClientHandler. fix
      fileContents.foreach {
        _ match {

          // directory
          case (subpath, None) => {
            val emptyDir = Utils.newFile(home, subpath)
            if (emptyDir.exists) emptyDir.delete  // delete if this was previously a file
            emptyDir.mkdirs
            hashes.update(subpath, None)
          }

          // file
          case (subpath, Some(msgData)) => {
            val file = Utils.newFile(home, subpath)

            hashes.get(subpath) match {
              case None => Utils.ensureDir(home, subpath)
              case Some(None) => file.delete // directory is now a file
              case Some(Some(_)) => () // updated file
            }

            Utils.writeFile(file)(msgData.bytes)
            hashes.update(subpath, Some(ClientData(file.lastModified, msgData.newHash)))
          }
        }

      }
    }

    case RejectUpdateMessage(rejections) => ()  // will receive corrections from Server momentarily

    case RemovedMessage(fileMap) => {
      fileMap.foreach { nameHash =>
        hashes.remove(nameHash._1)
        val file = Utils.newFile(home, nameHash._1)
        Utils.dirDelete(file)
      }
    }

    case _ => throw new IOException("Unknown or incorrect message received")
  }

  def run : Unit = {

    // generate file list and hashes
    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), Some(ClientData(file.lastModified, hash)))
    }
    { dir =>
      hashes.update(getRelPath(dir), None)
    }

    val serv = new Socket(host, port)
    sock = serv
    val out = new ObjectOutputStream(serv.getOutputStream)
    val in = new ObjectInputStream(serv.getInputStream)

    def readObject: Option[Object] = Utils.checkedRead(disconnect)(in)
    val writeObject = Utils.checkedWrite(disconnect)(out)_

    // get list of files and hashes from server
    // TODO(jacob) currently assume Client files are a subset of Server files
    readObject match {
      case None => () // wait for termination
      case Some(FileListMessage(fileList)) => {

        val filtered = fileList.filter(nh =>
          (hashes.get(nh._1), nh._2) match {
            case (None, _) => true  // file or folder not present on client
            case (Some(None), None) => false  // folders match
            case (Some(None), Some(_)) => true  // folder on client is now file on server
            case (Some(Some(_)), None) => true  // file on client is now folder on server
            case (Some(Some(ClientData(_, hash1))), Some(hash2)) => !Utils.verifyBytes(hash1)(hash2)
          }
        )
        val msg = FileRequest(filtered.map(_._1))
        writeObject(msg)

        readObject match {
          case None => () // wait for termination
          case Some(FileMessage(fileContents)) => {

            // Process list of new files
            // TODO(jacob) also mostly copy-pasted
            fileContents.foreach {
              _ match {
                // empty directory
                case (subpath, None) => {
                  val emptyDir = Utils.newFile(home, subpath)
                  emptyDir.mkdirs
                  hashes.update(subpath, None)
                }
                // normal file
                case (subpath, Some(fileData)) => {
                  Utils.ensureDir(home, subpath)
                  val file = Utils.newFile(home, subpath)
                  Utils.writeFile(file)(fileData.bytes)
                  hashes.update(subpath, Some(ClientData(file.lastModified, fileData.newHash)))
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