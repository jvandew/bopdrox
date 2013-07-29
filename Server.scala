import java.io.File
import java.net.ServerSocket
import scala.collection.mutable.HashMap

object Server {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[String, MapData]

  /* Takes a home folder and a binding port */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val port = args(1).toInt

    val getRelPath = Utils.getRelativePath(home) _

    // generate file list and hashes
    print("hashing files... ")

    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), MapData(file.lastModified, hash))
    }

    println("done")

    val serv = new ServerSocket(port)

    // main listen loop
    println("listening for connections")
    while (true) {
      val client = serv.accept
      val handler = new Thread(new ClientHandler(client)(home)(hashes))
      handler.start
    }

  }
}