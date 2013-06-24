import java.io.{File, FileInputStream, FileOutputStream, IOException,
                ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Client {

  // store file hashes of the most recent version
  // TODO(jacob) include version vectors at some point
  val hashes = new HashMap[String, Array[Byte]]

  val hash_algo = "SHA-512"

  /* Takes a home folder, a binding port, and the address of a running Server */
  def main (args: Array[String]) : Unit = {
    val home = new File(args(0))
    val localport = args(1).toInt
    val Array(host, port) = args(2).split(':')
    val hasher = MessageDigest.getInstance(hash_algo)

    // // generate file list and hashes
    // val filenames = home.list
    // filenames.foreach { filename =>
    //   val file = new File(filename)
    //   val fileIn = new FileInputStream(file)
    //   val bytes = new Array[Byte](file.length.toInt)
    //   fileIn.read(bytes)

    //   val hash = hasher.digest(bytes)
    //   hashes.update(filename, hash)
    // }

    val serv = new Socket(host, port.toInt)
    val out = new ObjectOutputStream(serv.getOutputStream)
    val in = new ObjectInputStream(serv.getInputStream)

    println("connected to server")

    // get list of files and hashes from server
    in.readObject match {
      case FileListMessage(fileList) => {
        val files = fileList.map(_._1)
        val msg = FileRequest(files)
        out.writeObject(msg)

        in.readObject match {
          case FileMessage(files) => {
            files.foreach { name_data =>
              val path = home + File.separator + name_data._1

              // check if we must create a directory
              name_data._1.lastIndexOf(File.separatorChar) match {
                case -1 => ()   // file; nothing to do here
                case split => {
                  // directory; create it if necessary
                  val dirPath = path.substring(0, split)
                  val dir = new File(dirPath)
                  if (!dir.exists)
                    dir.mkdirs
                }
              }

              val fileOut = new FileOutputStream(path)
              fileOut.write(name_data._2)
              fileOut.close

              val hash = hasher.digest(name_data._2)
              hashes.update(name_data._1, hash)
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
      println("Hey good lookin")
      Thread.sleep(10 * 1000)
    }

  }
}