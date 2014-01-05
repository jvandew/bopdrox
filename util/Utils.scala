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

  // Delete the given directory and all files contained within. If a file is
  // given as the argument it too will meet its demise.
  def dirDelete (dir: File) : Boolean = {
    Option(dir.listFiles) match {
      case None => dir.delete
      case Some(Array()) => dir.delete
      case Some(files) => {
        val deletions = files.map { file =>
          if (file.isFile)
            file.delete
          else
            dirDelete(file)
        }

        dir.delete && deletions.reduce(_ && _)

      }
    }
  }

  // Detect if a directory is empty. Calling this function with on a file instead
  // of a directory is an error.
  def dirEmpty (dir: File) : Boolean = dir.list match {
    case Array() => true
    case _ => false
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

  /* Search for the highest parent folder of the given file/folder that satisfies
   * the given predicate. If no parent is found the given subpath will be returned.
   * TODO(jacob) are we guranteed processing order with takeWhile? */
  def findParent (home: File) (subpath: List[String]) (pred: File => Boolean) : List[String] = {
    var sub = List[String]()
    subpath.takeWhile { p =>
      sub = sub :+ p
      !pred(newFile(home, sub))
    }

    sub
  }

  /* Find and return the highest deleted folder subpath in the given subpath.
   * If no parent folder has been deleted the subpath itself is returned.
   * This function is not guranteed correct results if folders along the
   * subpath are deleted during execution. If the home folder has been deleted
   * this function will return the top folder contained within home. In other
   * words, you can't go home again, but you can sure be in denial about it. */
  def getDeleted (home: File, subpath: List[String]) : List[String] =
    findParent(home)(subpath)(!_.exists)

  // find the relative path for a file. requires file to be contained within home
  def getRelativePath (home: File) (file: File) : List[String] = {
    val filePath = file.getCanonicalPath
    val homePrefix = home.getCanonicalPath + File.separator
    val relPathString = filePath.stripPrefix(homePrefix)

    if (relPathString == filePath)
      Nil
    else
      splitPath(relPathString)
  }

  // wrapper around hasher.digest(bytes)
  def hashBytes (bytes: Array[Byte]) : Array[Byte] = hasher.digest(bytes)

  // helper function to hash the contents of a file
  def hashFile (file: File) : Array[Byte] = contentsAndHash(file)._2

  // check if the given File object is a file and is empty
  def isEmptyFile (file: File) : Boolean = file.isFile && (file.length == 0)

  // join together a list of strings representing a file path
  def joinPath (path: List[String]) : String = path.reduce(_ + File.separator + _)

  // is this a prefix of the given list?
  def listStartsWith[T] (list: List[T]) (prefix: List[T]) : Boolean = (list, prefix) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (e1::list2, e2::prefix2) => (e1 equals e2) && listStartsWith(list2)(prefix2)
  }

  // shortcut method to create a File object using our List subpath format
  def newFile (home: File, subpath: List[String]) : File = {
    subpath match {
      case Nil => home
      case path => new File(home, joinPath(path))
    }
  }

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

  // check whether or not two byte arrays match
  def verifyBytes (hash1: Array[Byte])(hash2: Array[Byte]) =
    hash1.length == hash2.length && hash1.zip(hash2).forall(hs => hs._1 == hs._2)

  // write data to a file
  def writeFile (file: File) (bytes: Array[Byte]) : Unit = {
    val fileOut = new FileOutputStream(file)
    fileOut.write(bytes)
    fileOut.close
  }

}