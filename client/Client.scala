package bopdrox.client

import bopdrox.msg.{Ack, FileListMessage, FileMessage, FileMsgData, FileRequest, Message, RemovedMessage}
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

class Client (home: File) (host: String) (port: Int) extends Runnable {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[List[String], Option[ClientData]]

  val getRelPath = Utils.getRelativePath(home)_

  // store received Messages for later consumption
  val messageQueue = new Queue[Message]

  private var open = false

  // disconnect handler
  private[client] def disconnect (ioe: IOException) : Unit = {
    println("disconnected from Server; exiting...")
    sys.exit
  }

  def isOpen : Boolean = open

  // note this method is not called asnchronously
  private def matchMessage (msg: Message) : Unit = {

    msg match {
      case FileMessage(fileContents) => {
        println("handling FileMessage... ")

        // TODO(jacob) this code is mostly copy-pasted from ClientHandler. fix
        fileContents.foreach {
          _ match {
            // empty directory
            case (subpath, None) => {
              println(subpath)
              val emptyDir = Utils.newFile(home, subpath)
              if (emptyDir.exists) emptyDir.delete
              emptyDir.mkdirs
              println("added to map: " + subpath)
              hashes.update(subpath, None)
            }

            // normal file
            case (subpath, Some(msgData)) => {
              println(subpath)
              val file = Utils.newFile(home, subpath)

              hashes.get(subpath) match {
                case None => {  // new file
                  Utils.ensureDir(home, subpath)
                  Utils.findParent(home)(subpath)(f => hashes.contains(getRelPath(f))) match {
                    case `subpath` => ()  // no previously empty folder to remove
                    case parent => hashes.remove(parent)
                  }
                }

                case Some(None) => file.delete // empty directory is now a file
                case Some(Some(_)) => () // updated file
              }

              Utils.writeFile(file)(msgData.bytes)
              hashes.update(subpath, Some(ClientData(file.lastModified, msgData.newHash)))
            }
          }
        }

        println("done")
      }

      case RemovedMessage(fileMap) => {
        println("handling RemovedMessage... ")

        fileMap.foreach { nameHash =>
          println(nameHash._1)

          hashes.remove(nameHash._1)
          val file = Utils.newFile(home, nameHash._1)

          nameHash._2 match {
            case None => {

              file.list match {
                case Array() => file.delete // empty folder. nothing to see here
                case _ => Utils.dirForeach(file) { delFile =>
                  hashes.remove(getRelPath(delFile))
                  delFile.delete // would happen in dirDelete, but more efficient here
                }
                { delDir =>
                  hashes.remove(getRelPath(delDir))
                  delDir.delete // would happen in dirDelete, but more efficient here
                }
              }

              Utils.dirDelete(file)
            }

            case Some(hash) => file.delete
          }

          val parent = file.getParentFile
          if (Utils.dirEmpty(parent))
            hashes.update(getRelPath(parent), None)
        }

        println("done")
      }

      case _ => throw new IOException("Unknown or incorrect message received")
    }
  }

  def run : Unit = {

    // generate file list and hashes
    print("hashing files... ")

    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), Some(ClientData(file.lastModified, hash)))
    }
    { dir => hashes.update(getRelPath(dir), None)}

    println("done")

    val serv = new Socket(host, port)
    val out = new ObjectOutputStream(serv.getOutputStream)
    val in = new ObjectInputStream(serv.getInputStream)

    def readObject: Option[Object] = Utils.checkedRead(disconnect)(in)
    val writeObject = Utils.checkedWrite(disconnect)(out)_

    println("connected to server")

    // get list of files and hashes from server
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

        val update = {
          if (!hashes.contains(path))
            true
          else
            hashes(path) match {
              case None => true   // formerly an empty directory
              case Some(fileData) =>
                if (fileData.time != file.lastModified)
                  true
                else
                  false
            }
        }

        if (update) {
          val (bytes, hash) = Utils.contentsAndHash(file)
          hashes.update(path, Some(ClientData(file.lastModified, hash)))
          updates = (path, Some(FileMsgData(bytes, None, hash)))::updates
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
            case Some(None) => {
              Utils.newFile(home, key).list match {
                case Array() => keyHashes.update(key, None) // empty file, actually deleted
                case _ => ()  // new file/folder created in previously empty folder
              }
            }
            case Some(Some(fileData)) => keyHashes.update(key, Some(fileData.hash)) // file
          }
          case deleted => {
            println("non-key deleted: " + deleted)
            keyHashes.update(deleted, None)
          }
        }
      }

      updates match {
        case Nil => ()  // no updates
        case _ => {
          println("update(s) detected. notifying Server... ")
          println(updates)
          val msg = FileMessage(updates)
          writeObject(msg)
          println("done")
        }
      }

      if (!keyHashes.isEmpty) {
        println("removed files detected. notifying Server... ")
        println(keyHashes)
        val msg = RemovedMessage(keyHashes)
        writeObject(msg)
        println("done")
      }

      Thread.sleep(1000)
    }

  }
}