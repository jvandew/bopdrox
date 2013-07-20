import java.io.{File, FileInputStream, FileOutputStream, IOException}
import java.security.MessageDigest

/* This object is for common utility and helper functions that are shared
 * between Server and Client */
object Utils {

  val hash_algo = "SHA-512"

  /* static hasher; note we hold invariant that the current digest
   * is empty by only calling hasher.digest(data) */
  // TODO(jacob) this could be enforced staticly if we so desire
  val hasher = MessageDigest.getInstance(hash_algo)

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

  // find the relative path for a file
  def getRelativePath (home: File) (file: File) : String =
    file.getCanonicalPath.stripPrefix(home.getCanonicalPath)

  // wrapper around hasher.digest(bytes)
  def hashBytes (bytes: Array[Byte]) : Array[Byte] = hasher.digest(bytes)

  // helper function to hash the contents of a file
  def hashFile (file: File) : Array[Byte] = contentsAndHash(file)._2

  // read the contents of a file
  def readFile (file: File) : Array[Byte] = {
    val fileIn = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    fileIn.read(bytes)
    fileIn.close
    bytes
  }

  // write data to a file
  def writeFile (file: File) (bytes: Array[Byte]) : Unit = {
    val fileOut = new FileOutputStream(file)
    fileOut.write(bytes)
    fileOut.close
  }

}