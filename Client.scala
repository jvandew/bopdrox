import java.io.{File, FileInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.HashMap

object Client {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[String, (Long, Array[Byte])]

  /* Takes a home folder, a binding port, and the address of a running Server */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val localport = args(1).toInt
    val Array(host, port) = args(2).split(':')

    val getRelPath = Utils.getRelativePath(home) _

    // generate file list and hashes
    print("hashing files... ")

    Utils.dirForeach(home) { file =>
      val hash = Utils.hashFile(file)
      hashes.update(getRelPath(file), (file.lastModified, hash))
    }

    println("done")

    val serv = new Socket(host, port.toInt)
    val out = new ObjectOutputStream(serv.getOutputStream)
    val in = new ObjectInputStream(serv.getInputStream)

    println("connected to server")

    // get list of files and hashes from server
    in.readObject match {
      case FileListMessage(fileList) => {

        // TODO(jacob) eventually this should only request new/modified files
        val files = fileList.map(_._1)
        val msg = FileRequest(files)
        out.writeObject(msg)

        in.readObject match {
          case FileMessage(files) => {

            // Process list of new files
            files.foreach { name_data =>
              val path = home + File.separator + name_data._1
              Utils.ensureDir(name_data._1)

              val file = new File(path)
              Utils.writeFile(file)(name_data._2)

              // TODO(jacob) check hash value matches
              val hash = Utils.hashBytes(name_data._2)
              hashes.update(name_data._1, (file.lastModified, hash))
            }

            out.writeObject(Ack)
          }
          case _ => throw new IOException("Unknown or incorrect message received")
        }
      }
      case _ => throw new IOException("Unknown or incorrect message received")
    }

    // main loop to check for updated files
    while (true) {
      var updates = List[(String, Array[Byte], Array[Byte])]()

      Utils.dirForeach(home) { file =>
        val path = getRelPath(file)

        if (!hashes.contains(path) || hashes(path)._1 != file.lastModified) {
          val (bytes, hash) = Utils.contentsAndHash(file)
          updates = (path, bytes, hash)::updates
        }
      }

      updates match {
        case Nil => ()  // no updates
        case _ => {
          val msg = new FileMessage(updates)
          out.writeObject(msg)

          // TODO(jacob) currently we don't make sure the message is received...
        }
      }

      Thread.sleep(1000)
    }

  }
}