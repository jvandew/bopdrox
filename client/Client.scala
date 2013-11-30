package bopdrox.client

import bopdrox.msg.{Ack, FileListMessage, FileMessage, FileMsgData, FileRequest, Message, RemovedMessage}
import bopdrox.util.Utils
import java.io.{File, FileInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.{HashMap, Queue}

object Client {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[List[String], Option[ClientData]]

  // store received Messages for later consumption
  val messageQueue = new Queue[Message]

  // disconnect handler
  // TODO(jacob) Client should try to reconnect, rather than simply terminate
  // TODO(jacob) this should be visible only to Client and ClientListener
  def disconnect (ioe: IOException) : Unit = {
    println("disconnected from Server; exiting...")
    sys.exit
  }

  private def matchMessage (home: File) (msg: Message) : Unit = {
    msg match {
      case FileMessage(fileContents) => {
        print("handling FileMessage... ")

        // TODO(jacob) this code is mostly copy-pasted from ClientHandler. fix
        fileContents.foreach {
          _ match {
            // empty directory
            case (subpath, None) => {
              val emptyDir = Utils.newFile(home, subpath)
              emptyDir.mkdirs
              hashes.update(subpath, None)
            }
            // normal file
            case (subpath, Some(data)) => {
              Utils.ensureDir(home, subpath)
              val file = Utils.newFile(home, subpath)
              Utils.writeFile(file)(data.bytes)
              hashes.update(subpath, Some(ClientData(file.lastModified, data.newHash)))
            }
          }
        }

        println("done")
      }

      case RemovedMessage(fileSet) => {
        print("handling RemovedMessage... ")

        fileSet.foreach { nameHash =>
          hashes.remove(nameHash._1)
          val file = Utils.newFile(home, nameHash._1)
          file.delete
        }

      println("done")
      }

      case _ => throw new IOException("Unknown or incorrect message received")
    }
  }

  /* Takes a home folder, a binding port, and the address of a running Server */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val Array(host, port) = args(1).split(':')

    val getRelPath = Utils.getRelativePath(home)_
    val matchMsg = matchMessage(home)_

    // generate file list and hashes
    print("hashing files... ")

    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), Some(ClientData(file.lastModified, hash)))
    }
    { dir => hashes.update(getRelPath(dir), None)}

    println("done")

    val serv = new Socket(host, port.toInt)
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
            case (Some(Some(ClientData(_, hash1))), Some(hash2)) => !Utils.verifyHash(hash1)(hash2)
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
    new Thread(new ClientListener(in)(home)).start

    // main loop to check for updated files
    while (true) {

      // process any received Messages
      // TODO(jacob) this could be an inconvenient way of handling Messages if we have a
      // large Queue (unlikely with a single user) or a significantly large directory tree
      while (!messageQueue.isEmpty)
        matchMsg(messageQueue.dequeue)

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
        hashes.remove(key) match {
          case None => ()
          case Some(None) => keyHashes.update(key, None)
          case Some(Some(fileData)) => keyHashes.update(key, Some(fileData.hash))
        }
      }

      updates match {
        case Nil => ()  // no updates
        case _ => {
          print("update(s) detected. notifying Server... ")
          val msg = FileMessage(updates)
          writeObject(msg)
          println("done")
        }
      }

      if (!keyHashes.isEmpty) {
        print("removed files detected. notifying Server... ")
        val msg = RemovedMessage(keyHashes)
        writeObject(msg)
        println("done")
      }

      Thread.sleep(1000)
    }

  }
}