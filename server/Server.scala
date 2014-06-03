package bopdrox.server

import bopdrox.util.{FSDirectory, FSFile, Utils}
import java.io.File
import java.net.ServerSocket
import scala.collection.mutable.HashMap

object Server {

  /* Takes a home folder and a binding port */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val port = args(1).toInt

    val server = new Server(home)(port)
    server.run
  }
}

class Server (val home: File) (port: Int) extends Runnable {

  private var open = false

  val getRelPath = Utils.getRelativePath(home)_

  // store a list of all connected clients
  // note since this is a var we sychronize on Server instead of clients directly
  var clients = List[ClientHandler]()

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new ServerMap

  def dropClient (client: ClientHandler): Unit = this.synchronized {
    clients = clients.diff(List(client))
  }

  def isOpen : Boolean = open

  def run : Unit = {

    // generate file list and hashes
    print("hashing files... ")

    // no handlers yet -> no synchronization
    Utils.dirForeach(home) { file =>
      val fsFile = FSFile(getRelPath(file))
      val data = FileData(file.lastModified, Utils.hashFile(file), Nil)
      hashes.update(fsFile, data)
    }
    { dir =>
      val fsDir = FSDirectory(getRelPath(dir))
      val data = DirData(dir.lastModified)
      hashes.update(fsDir, data)
    }

    println("done")

    val serv = new ServerSocket(port)
    open = true

    // main listen loop
    println("listening for connections")
    while (true) {
      val client = serv.accept
      val handler = new ClientHandler(this)(client)

      this.synchronized {
        clients = handler::clients
      }

      new Thread(handler).start
    }
  }

}
