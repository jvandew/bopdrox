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

        fileContents.foreach { pbh =>
          val path = home + File.separator + pbh._1
          Utils.ensureDir(path)

          val file = new File(path)
          Utils.writeFile(file)(pbh._2)
          // TODO(jacob) this is not safe with multiple clients
          // TODO(jacob) verify correct hash
          Server.hashes.update(pbh._1, MapData(file.lastModified, pbh._3))
        }

        println("done")
      }

      case RemovedMessage(fileSet) => {
        print("handling RemovedMessage... ")

        fileSet.foreach { filename =>
          Server.hashes.remove(filename)
          val file = new File(home + File.separator + filename)
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
    val fhList = Server.hashes.toList.map(kv => (kv._1, kv._2.hash))
    val listMsg = FileListMessage(fhList)
    out.writeObject(listMsg)

    in.readObject match {
      case FileRequest(files) => {

        val fileList =
          files.map { filename =>
            val contents = Utils.readFile(new File(home, filename))
            (filename, contents, Server.hashes(filename).hash)
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