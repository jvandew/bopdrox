import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.{HashMap, HashSet}

/* A ClientHandler is a Runnable object designed to handle all communications
 * with a Client */
class ClientHandler (client: Socket) (home: File) extends Runnable {

  private val in = new ObjectInputStream(client.getInputStream)
  private val out = new ObjectOutputStream(client.getOutputStream)

  private def matchMessage (msg: Message) : Unit = {
    msg match {
      case FileMessage(fileContents) => {
        print("handling FileMessage... ")

        fileContents.foreach {
          _ match {
            // empty directory
            case (subpath, None) => {
              val emptyDir = Utils.newFile(home, subpath)
              emptyDir.mkdirs
              Server.hashes.update(subpath, None)
            }
            // normal file
            case (subpath, Some((bytes, hash))) => {
              Utils.ensureDir(home, subpath)
              val file = Utils.newFile(home, subpath)
              Utils.writeFile(file)(bytes)
              Server.hashes.update(subpath, Some(MapData(file.lastModified, hash)))
            }
          }
        }

        println("done")
      }

      case RemovedMessage(fileSet) => {
        print("handling RemovedMessage... ")

        fileSet.foreach { filename =>
          Server.hashes.remove(filename)
          val file = Utils.newFile(home, filename)
          file.delete
        }

        println("done")
      }
    }
  }

  def message (msg: Message) : Unit = out.writeObject(msg)

  def run : Unit = {
    println("client connected")

    // send client a list of file names and hashes
    val fhList = Server.hashes.toList.map {
      _ match {
        case (subpath, None) => (subpath, None)
        case (subpath, Some(fileData)) => (subpath, Some(fileData.hash))
      }
    }

    val listMsg = FileListMessage(fhList)
    out.writeObject(listMsg)

    in.readObject match {
      case FileRequest(files) => {

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
        out.writeObject(msg)

        in.readObject match {
          case Ack => ()
          case _ => throw new IOException("Unknown or incorrect message received")
        }
      }
      case _ => throw new IOException("Unknown or incorrect message received")
    }

    // main loop to listen for updated files
    while (true) {
      in.readObject match {
        case msg: Message => {

          // forward message
          Server.clients.foreach { c =>
            if (!c.equals(this))
              c.message(msg)
          }

          matchMessage(msg)
        }

        case _ => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}