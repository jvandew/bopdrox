import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.HashMap

/* A ClientHandler is a Runnable object designed to handle all communications
 * with a Client */
class ClientHandler (client: Socket) (val hashes: HashMap[String, MapData]) extends Runnable {

  val in = new ObjectInputStream(client.getInputStream)
  val out = new ObjectOutputStream(client.getOutputStream)

  def run : Unit = {
    println("client connected")

    // send client a list of file names and hashes
    val fhList = hashes.toList.map(kv => (kv._1, kv._2.hash))
    val listMsg = FileListMessage(fhList)
    out.writeObject(listMsg)

    in.readObject match {
      case FileRequest(files) => {

        val fileList =
          files.map { filename =>
            val contents = Utils.readFile(new File(filename))
            (filename, contents, hashes(filename).hash)
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
        case FileMessage(updates) => {

          updates.foreach { pbh =>
            Utils.ensureDir(pbh._1)
            val file = new File(pbh._1)
            Utils.writeFile(file)(pbh._2)
            // TODO(jacob) this is not safe with multiple clients
            // TODO(jacob) verify correct hash
            hashes.update(pbh._1, MapData(file.lastModified, pbh._3))
          }
        }

        case _ => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}