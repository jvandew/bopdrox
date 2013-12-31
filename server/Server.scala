package bopdrox.server

import bopdrox.util.Utils
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

  val getRelPath = Utils.getRelativePath(home)_

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[List[String], Option[ServerData]]

  // store a list of all connected clients
  // note since this is a var we sychronize on Server instead of clients directly
  var clients = List[ClientHandler]()

  def dropClient (client: ClientHandler): Unit = this.synchronized {
    clients = clients.diff(List(client))
  }

  def run : Unit = {

    // generate file list and hashes
    print("hashing files... ")

    // no handlers yet -> no synchronization
    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), Some(ServerData(file.lastModified, hash, Nil)))
    }
    { dir => hashes.update(getRelPath(dir), None)}

    println("done")

    val serv = new ServerSocket(port)

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
