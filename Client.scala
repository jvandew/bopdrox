import java.io.{File, FileInputStream, FileOutputStream, IOException,
                ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Client {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[String, (Long, Array[Byte])]

  val hash_algo = "SHA-512"

  val hasher = MessageDigest.getInstance(hash_algo)

  // get the contents of a file and its hash value in that order
  def contentsAndHash (file: File) : (Array[Byte], Array[Byte]) = {
    val fileIn = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    fileIn.read(bytes)

    (bytes, hasher.digest(bytes))
  }

  // recursively walk a directory tree and apply the given function to each file
  // calling this function on a file is an error
  def dirForeach (dir: File) (proc: File => Unit) : Unit = {
    Option(dir.listFiles) match {
      case None =>
        throw new IOException("Error while processing directory")
      
      case Some(files) => {
        files.foreach { file =>
          if (file.isFile)
            proc(file)
          else
            dirForeach(file)(proc)
        }
      }
    }
  }

  // ensure that the directory containing the given file path exists
  // and create it if not
  def ensureDir (path: String) : Unit = {
    path.lastIndexOf(File.separatorChar) match {
      case -1 => ()   // file; nothing to do here
      case split => {
        // directory; create it if necessary
        val dir = new File(path.substring(0, split))
        if (!dir.exists)
          dir.mkdirs
      }
    }
  }

  // helper function to generate a relative path for a file
  def getRelativePath (home: File, file: File) : String =
    file.getCanonicalPath.stripPrefix(home.getCanonicalPath)

  // helper function to hash the contents of a file
  def hashFile (file: File) : Array[Byte] = contentsAndHash(file)._2

  /* Takes a home folder, a binding port, and the address of a running Server */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val localport = args(1).toInt
    val Array(host, port) = args(2).split(':')

    // generate file list and hashes
    print("hashing files... ")

    dirForeach(home) { file =>
      val hash = hashFile(file)
      hashes.update(getRelativePath(home, file), (file.lastModified, hash))
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
              ensureDir(name_data._1)

              val file = new File(path)
              val fileOut = new FileOutputStream(file)
              fileOut.write(name_data._2)
              fileOut.close

              // TODO(jacob) check hash value matches
              val hash = hasher.digest(name_data._2)
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

      dirForeach(home) { file =>
        val path = getRelativePath(home, file)

        if (!hashes.contains(path) || hashes(path)._1 != file.lastModified) {
          val (bytes, hash) = contentsAndHash(file)
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