import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.{ServerSocket, Socket}
import scala.collection.mutable.HashMap

object Server {

  /* A ClientHandler is a Runnable object designed to handle all communications
   * with a Client */
  class ClientHandler(val client: Socket) extends Runnable {

    val in = new ObjectInputStream(client.getInputStream)
    val out = new ObjectOutputStream(client.getOutputStream)

    def run : Unit = {
      println("client connected")

      // send client a list of file names and hashes
      val fhList = hashes.toList.map(kv => (kv._1, kv._2._2))
      val listMsg = FileListMessage(fhList)
      out.writeObject(listMsg)

      in.readObject match {
        case FileRequest(files) => {

          val fileList =
            files.map { filename =>
              val contents = Utils.readFile(new File(filename))
              (filename, contents, hashes(filename)._2)
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

      while (true) {
        in.readObject match {
          case FileMessage(updates) => {

            updates.foreach { pbh =>
              Utils.ensureDir(pbh._1)
              val file = new File(pbh._1)
              Utils.writeFile(file)(pbh._2)
              // TODO(jacob) this is not safe with multiple clients
              // TODO(jacob) verify correct hash
              hashes.update(pbh._1, (file.lastModified, pbh._3))
            }
          }

          case _ => throw new IOException("Unknown or incorrect message received")
        }
      }

    }
  }

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[String, (Long, Array[Byte])]

  /* Takes a home folder and a binding port */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val port = args(1).toInt

    val getRelPath = Utils.getRelativePath(home) _

    // generate file list and hashes
    print("hashing files... ")

    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), (file.lastModified, hash))
    }

    println("done")

    val serv = new ServerSocket(port)

    // main listen loop
    println("listening for connections")
    while (true) {
      val client = serv.accept
      val handler = new Thread(new ClientHandler(client))
      handler.start
    }

  }
}