package bopdrox.server

import bopdrox.util.{FSDirectory, FSFile, Utils}
import java.io.{File, IOException}
import java.net.ServerSocket
import scala.collection.mutable.HashMap

object Server {

  /* Takes a home folder and a binding port */
  def main (args: Array[String]) : Unit = {

    val home = new File(args(0))
    val port = args(1).toInt
    val debug = if (args.length >= 3 && args(2) == "-debug") true else false

    val server = new Server(home)(port)(debug)
    server.run
  }
}

class Server (val home: File) (port: Int) (debug: Boolean) extends Runnable {

  private var open = false

  val getRelPath = Utils.getRelativePath(home)_

  // store a table of all connected Clients and their handlers
  val clients = new HashMap[String, ClientHandler]

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new ServerMap

  def dropClient (client: ClientHandler): Unit = clients.synchronized {
    clients.remove(client.id)
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

      try {
        val handler = new ClientHandler(this)(client)(debug)

        clients.synchronized {
          clients.get(handler.id) match {

            case None => {
              clients(handler.id) = handler
              new Thread(handler).start
            }

            case Some(ch) => ch.reconnect(client)
          }
        }
      } catch {
        case ioe: IOException => () // bad client connection
      }
    }
  }

}
