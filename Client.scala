import java.io.{File, FileInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.HashMap

object Client {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[String, MapData]

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
      hashes.update(getRelPath(file), MapData(file.lastModified, hash))
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
              Utils.ensureDir(path)

              val file = new File(path)
              Utils.writeFile(file)(name_data._2)

              // TODO(jacob) check hash value matches
              val hash = Utils.hashBytes(name_data._2)
              hashes.update(name_data._1, MapData(file.lastModified, hash))
            }

            out.writeObject(Ack)
          }
          case _ => throw new IOException("Unknown or incorrect message received")
        }
      }
      case _ => throw new IOException("Unknown or incorrect message received")
    }

    // begin listener thread
    new Thread(new ClientListener(in)(home)).start

    // main loop to check for updated files
    while (true) {
      var keySet = hashes.keySet
      var updates = List[(String, Array[Byte], Array[Byte])]()

      Utils.dirForeach(home) { file =>
        val path = getRelPath(file)
        keySet = keySet - path

        if (!hashes.contains(path) || hashes(path).time != file.lastModified) {
          val (bytes, hash) = Utils.contentsAndHash(file)
          hashes.update(path, MapData(file.lastModified, hash))
          updates = (path, bytes, hash)::updates
        }
      }

      // remove any deleted files from our hashmap
      keySet = keySet.filter { key =>
        hashes.remove(key) match {
          case None => false
          case Some(_) => true
        }
      }

      updates match {
        case Nil => ()  // no updates
        case _ => {
          print("update(s) detected. notifying Server... ")
          val msg = FileMessage(updates)
          out.writeObject(msg)
          println("done")
        }
      }

      if (!keySet.isEmpty) {
        print("removed files detected. notifying Server... ")
        val msg = RemovedMessage(keySet)
        out.writeObject(msg)
        println("done")
      }

      // TODO(jacob) currently we don't make sure messages are received...

      Thread.sleep(1000)
    }

  }
}