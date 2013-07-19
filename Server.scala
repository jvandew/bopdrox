import java.io.{File, FileInputStream, FileOutputStream, IOException,
                ObjectInputStream, ObjectOutputStream}
import java.net.{ServerSocket, Socket}
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Server {

  // get the contents of a file and its hash value in that order
  def contentsAndHash (file: File) : (Array[Byte], Array[Byte]) = {
    val bytes = readFile(file)
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

  // helper function to read the contents of a file
  def readFile (file: File) : Array[Byte] = {
    val fileIn = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    fileIn.read(bytes)
    bytes
  }

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
              val contents = readFile(new File(filename))
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
              ensureDir(pbh._1)
              val file = new File(pbh._1)
              val fileOut = new FileOutputStream(file)
              fileOut.write(pbh._2)
              fileOut.close
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

  val hash_algo = "SHA-512"

  val hasher = MessageDigest.getInstance(hash_algo)

  /* Takes a home folder and a binding port */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val port = args(1).toInt

    // generate file list and hashes
    print("hashing files... ")

    dirForeach(home) { file =>
      val hash = hashFile(file)
      hashes.update(getRelativePath(home, file), (file.lastModified, hash))
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