package server

import java.io.File
import java.net.ServerSocket
import scala.collection.mutable.HashMap

import util.{MapData, Utils}

object Server {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[List[String], Option[MapData]]

  // store a list of all connected clients
  // note since this is a var we sychronize on Server instead of clients directly
  var clients = List[ClientHandler]()

  /* Takes a home folder and a binding port */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val port = args(1).toInt

    val getRelPath = Utils.getRelativePath(home) _

    // generate file list and hashes
    print("hashing files... ")

    // no handlers yet -> no synchronization
    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), Some(MapData(file.lastModified, hash)))
    }
    { dir => hashes.update(getRelPath(dir), None)}

    println("done")

    val serv = new ServerSocket(port)

    // main listen loop
    println("listening for connections")
    while (true) {
      val client = serv.accept
      val handler = new ClientHandler(client)(home)

      Server.synchronized {
        clients = handler::clients 
      }

      new Thread(handler).start
    }

  }
}