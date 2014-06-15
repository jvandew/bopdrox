package bopdrox.util

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
      out.reset
    } catch {
      case ioe: IOException => handler(ioe)
    }
  }

  // Find an unused path for a conflicted copy of a file or directory.
  // Not thread-safe
  def conflictedPath (home: File) (path: FSPath) : FSPath = {
    var conflictPath = path.updated(path.size - 1, path.last + "-cc")
    var conflictCount = 0
    while (Utils.newFile(home, conflictPath).exists) {
      conflictCount += 1
      conflictPath = path.updated(path.size - 1, path.last + "-cc" + conflictCount)
    }

    conflictPath
  }

  // get the contents of a file and its hash value in that order
  def contentsAndHash (file: File) : (FileBytes, FileHash) = {
    val bytes = readFile(file)
    (bytes, hasher.digest(bytes))
  }

  // Delete the given directory and all files contained within. If a file is
  // given as the argument it too will meet its demise.
  def dirDelete (dir: File) : Boolean = {
    Option(dir.listFiles) match {
      case None => dir.delete
      case Some(files) => {
        val deletions = files.map { file =>
          if (file.isFile)
            file.delete
          else
            dirDelete(file)
        }

        dir.delete && deletions.fold(true)(_ && _)

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
  // and directory. calling this function on a file is an error
  // TODO(jacob) if dir is deleted during this call bad things can happen
  // currently any resulting IOExceptions are just swallowed
  def dirForeach (dir: File) (procFile: File => Unit) (procDir: File => Unit) : Unit = {
    try {
      Option(dir.listFiles) match {
        case None =>
          throw new IOException("Error while processing directory")

        case Some(files) => {
          files.foreach { file =>
            if (file.isFile)
              procFile(file)
            else {
              procDir(file)
              dirForeach(file)(procFile)(procDir)
            }
          }
        }

      }
    }
    catch {
      case ioe: IOException => ()  // nothing to see here...
    }
  }

  def ensureDir (home: File, subpath: FSPath) : Unit =
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
  def findParent (home: File) (subpath: FSPath) (pred: File => Boolean) : FSPath = {
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
  def getDeleted (home: File, subpath: FSPath) : FSPath =
    findParent(home)(subpath)(!_.exists)

  // find the relative path for a file. requires file to be contained within home
  def getRelativePath (home: File) (file: File) : FSPath = {
    val filePath = file.getCanonicalPath
    val homePrefix = home.getCanonicalPath + File.separator
    val relPathString = filePath.stripPrefix(homePrefix)

    if (relPathString == filePath)
      Nil
    else
      splitPath(relPathString)
  }

  // wrapper around hasher.digest(bytes)
  def hashBytes (bytes: FileBytes) : FileHash = hasher.digest(bytes)

  // helper function to hash the contents of a file
  def hashFile (file: File) : FileHash = contentsAndHash(file)._2

  // check if the given File object is a file and is empty
  def isEmptyFile (file: File) : Boolean = file.isFile && (file.length == 0)

  // join together a list of strings representing a file path
  def joinPath (path: FSPath) : String = path.reduce(_ + File.separator + _)

  // is this a prefix of the given list?
  def listStartsWith[T] (list: List[T]) (prefix: List[T]) : Boolean = (list, prefix) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (e1::list2, e2::prefix2) => (e1 equals e2) && listStartsWith(list2)(prefix2)
  }

  // create a new directory, replacing any existing file
  def newDir (home: File, dir: FSDirectory) : File = {
    val emptyDir = Utils.newFile(home, dir.path)
    if (emptyDir.exists) emptyDir.delete
    emptyDir.mkdirs

    emptyDir
  }

  // create a new file
  def newFile (home: File, fsFile: FSFile) : File = newFile(home, fsFile.path)

  // shortcut method to create a File object using our List subpath format
  private def newFile (home: File, subpath: FSPath) : File = {
    subpath match {
      case Nil => home
      case path => new File(home, joinPath(path))
    }
  }

  // obtain a list of all subpaths to parent folders in the given subpath
  def pathParents (path: FSPath) : List[FSPath] =
    path.scanLeft(List[String]())(_ :+ _).diff(List(Nil))

  // a nice way to print out the local host:port from a socket
  def printLocalSocket (sock: Socket) : String =
    sock.getLocalAddress.getHostName + ":" + sock.getLocalPort

  // a nice way to print out the standard host:port from a socket
  def printSocket (sock: Socket) : String =
    sock.getInetAddress.getHostName + ":" + sock.getPort

  def readFile (home: File, subpath: FSPath) : FileBytes =
    readFile(home, joinPath(subpath))

  def readFile (home: File, subpath: String) : FileBytes =
    readFile(new File(home, subpath))

  // read the contents of a file
  def readFile (file: File) : FileBytes = {
    val fileIn = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    fileIn.read(bytes)
    fileIn.close
    bytes
  }

  // split a file path string into a list of strings
  def splitPath (path: String) : FSPath =
    path.split(Pattern.quote(File.separator)).toList

  // check whether or not two hashes match
  def verifyHash (hash1: FileHash)(hash2: FileHash) : Boolean =
    hash1.length == hash2.length && hash1.zip(hash2).forall(hs => hs._1 == hs._2)

  // write data to a file
  def writeFile (file: File) (bytes: FileBytes) : Unit = {
    val fileOut = new FileOutputStream(file)
    fileOut.write(bytes)
    fileOut.close
  }

}