import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.{HashMap, HashSet}

/* A ClientHandler is a Runnable object designed to handle all communications
 * with a Client */
class ClientHandler (client: Socket) (home: File) extends Runnable {

  private val in = new ObjectInputStream(client.getInputStream)
  private val out = new ObjectOutputStream(client.getOutputStream)

  // continue running this handler
  private var continue = true

  // disconnect client and mark handler to terminate
  private def disconnect (ioe: IOException) : Unit = {
    println("disconnecting client: " + Utils.printSocket(client))

    Server.synchronized {
      Server.clients = Server.clients.diff(List(this))
    }

    continue = false
  }

  private def readObject: Option[Object] = Utils.checkedRead(disconnect)(in)
  private val writeObject = Utils.checkedWrite(disconnect)(out)_

  /* Handle message matching and file operations. Note that due the variable
   * nature of network communications it does not make sense to enforce FIFO
   * ordering of message handling, only that the file operation + hash map
   * update is atomic. */
  private def matchMessage (msg: Message) : Unit = {
    msg match {
      case FileMessage(fileContents) => {
        print("handling FileMessage... ")

        fileContents.foreach {
          _ match {
            // empty directory
            case (subpath, None) => {
              Server.hashes.synchronized {
                val emptyDir = Utils.newFile(home, subpath)
                emptyDir.mkdirs
                Server.hashes.update(subpath, None)
              }
            }

            // normal file
            case (subpath, Some((bytes, hash))) => {
              Server.hashes.synchronized {
                Utils.ensureDir(home, subpath)
                val file = Utils.newFile(home, subpath)
                Utils.writeFile(file)(bytes)
                Server.hashes.update(subpath, Some(MapData(file.lastModified, hash)))
              }
            }

          }
        }

        println("done")
      }

      case RemovedMessage(fileSet) => {
        print("handling RemovedMessage... ")

        fileSet.foreach { filename =>
          Server.hashes.synchronized {
            Server.hashes.remove(filename)
            val file = Utils.newFile(home, filename)
            file.delete
          }
        }

        println("done")
      }
    }
  }

  def message (msg: Message) : Unit = writeObject(msg)

  def run : Unit = {

    println("client connected: " + Utils.printSocket(client))

    // send client a list of file names and hashes; do not synchronize on read
    val fhList = Server.hashes.toList.map {
      _ match {
        case (subpath, None) => (subpath, None)
        case (subpath, Some(fileData)) => (subpath, Some(fileData.hash))
      }
    }

    val listMsg = FileListMessage(fhList)

    writeObject(listMsg)

    readObject match {
      case None => () // wait for termination
      case Some(FileRequest(files)) => {

        val fileList = files.map { filename =>
          Server.hashes(filename) match {
            case None => (filename, None)
            case Some(fileData) => {
              val contents = Utils.readFile(home, filename)
              (filename, Some(contents, fileData.hash))
            }
          }
        }

        val msg = FileMessage(fileList)
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
        case Some(msg: Message) => {

          // forward message
          Server.clients.foreach { c =>
            if (!c.equals(this))
              c.message(msg)
          }

          matchMessage(msg)
        }

        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}