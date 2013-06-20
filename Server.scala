import java.io.{File, FileInputStream, FileOutputStream, IOException,
                ObjectInputStream, ObjectOutputStream}
import java.net.{ServerSocket, Socket}
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Server {

  /* A ClientHandler is a Runnable object designed to handle all communications
   * with a Client */
  class ClientHandler(val client: Socket) extends Runnable {

    val in = new ObjectInputStream(client.getInputStream)
    val out = new ObjectOutputStream(client.getOutputStream)

    def run {
      // send client a list of file names and hashes
      val listMsg = FileListMessage(hashes.toList)
      out.writeObject(listMsg)

      in.readObject match {
        case Ack => println("Client up to date")
        case FileRequest(files) => {
          val fileList =
            files.map { filename =>
              val file = new File(filename)
              val fileIn = new FileInputStream(file)
              val bytes = new Array[Byte](file.length.toInt)
              fileIn.read(bytes)

              (filename, bytes)
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
        println("Hey there sexy")
        Thread.sleep(10 * 1000)
      }
    }
  }

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[String, Array[Byte]]

  val hash_algo = "SHA-512"

  /* Takes a home folder and a binding port */
  def main (args: Array[String]) {
    val home = new File(args(0))
    val port = args(1).toInt
    val hasher = MessageDigest.getInstance(hash_algo)

    // generate file list and hashes
    val filenames = home.list
    filenames.foreach { filename =>
      val file = new File(filename)
      val fileIn = new FileInputStream(file)
      val bytes = new Array[Byte](file.length.toInt)
      fileIn.read(bytes)

      val hash = hasher.digest(bytes)
      hashes.update(filename, hash)
    }

    val serv = new ServerSocket(port)

    // main listen loop
    while (true) {
      val client = serv.accept
      val handler = new Thread(new ClientHandler(client))
      handler.start
    }

  }
}