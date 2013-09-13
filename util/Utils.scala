package bopdrox.util

import bopdrox.msg.Message
import java.io.{File, FileInputStream, FileOutputStream, IOException,
                ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import java.security.MessageDigest
import java.util.regex.Pattern

/* This object is for common utility and helper functions that are shared
 * between Server and Client */
object Utils {

  val hash_algo = "SHA-512"

  /* static hasher; note we hold invariant that the current digest
   * is empty by only calling hasher.digest(data) */
  // TODO(jacob) this could be enforced staticly if we so desire
  val hasher = MessageDigest.getInstance(hash_algo)

  /* Performs a read on some ObjectInputStream, checking that the operation
   * succeeds while handling failure */
  def checkedRead (handler: IOException => Unit) (in: ObjectInputStream) : Option[Object] = {
    try {
      Some(in.readObject)
    } catch {
      case ioe: IOException => {
        handler(ioe)
        None
      }
    }
  }

  /* Perfroms a write on some ObjectOutputStream, checking that the operation
   * succeeds while handling failure */
  def checkedWrite (handler: IOException => Unit) (out: ObjectOutputStream) (msg: Message) : Unit = {
    try { 
      out.writeObject(msg)
    } catch {
      case ioe: IOException => handler(ioe)
    }
  }

  // get the contents of a file and its hash value in that order
  def contentsAndHash (file: File) : (Array[Byte], Array[Byte]) = {
    val bytes = readFile(file)
    (bytes, hasher.digest(bytes))
  }

  // recursively walk a directory tree and apply the given function to each file
  // calling this function on a file is an error
  // TODO(jacob) if dir is deleted during this call bad things can happen
  def dirForeach (dir: File) (proc: File => Unit) (empty: File => Unit) : Unit = {
    Option(dir.listFiles) match {
      case None =>
        throw new IOException("Error while processing directory")
      
      case Some(Array()) => empty(dir)

      case Some(files) => {
        files.foreach { file =>
          if (file.isFile)
            proc(file)
          else
            dirForeach(file)(proc)(empty)
        }
      }
    }
  }

  def ensureDir (home: File, subpath: List[String]) : Unit =
    ensureDir(home, joinPath(subpath))

  def ensureDir (home: File, subpath: String) : Unit =
    ensureDir(home + File.separator + subpath)

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
  def getRelativePath (home: File) (file: File) : List[String] = {
    val filePath = file.getCanonicalPath
    val homePrefix = home.getCanonicalPath + File.separator
    val relPathString = filePath.stripPrefix(homePrefix)
    splitPath(relPathString)
  }

  // wrapper around hasher.digest(bytes)
  def hashBytes (bytes: Array[Byte]) : Array[Byte] = hasher.digest(bytes)

  // helper function to hash the contents of a file
  def hashFile (file: File) : Array[Byte] = contentsAndHash(file)._2

  // join together a list of strings representing a file path
  def joinPath (path: List[String]) : String = path.reduce(_ + File.separator + _)

  // shortcut method to create a File object using our List subpath format
  def newFile (home: File, subpath: List[String]) : File =
    new File(home, joinPath(subpath))

  // a nice way to print out the standard host:port from a socket
  def printSocket (sock: Socket) : String =
    sock.getInetAddress.getHostName + ":" + sock.getPort

  def readFile (home: File, subpath: List[String]) : Array[Byte] =
    readFile(home, joinPath(subpath))

  def readFile (home: File, subpath: String) : Array[Byte] =
    readFile(new File(home, subpath))

  // read the contents of a file
  def readFile (file: File) : Array[Byte] = {
    val fileIn = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    fileIn.read(bytes)
    fileIn.close
    bytes
  }

  // split a file path string into a list of strings
  def splitPath (path: String) : List[String] =
    path.split(Pattern.quote(File.separator)).toList

  // write data to a file
  def writeFile (file: File) (bytes: Array[Byte]) : Unit = {
    val fileOut = new FileOutputStream(file)
    fileOut.write(bytes)
    fileOut.close
  }

}