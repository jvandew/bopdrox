package bopdrox.server

import bopdrox.util.{FileHash, FSDirectory, FSFile, FSObject, FSPath, Timestamp}
import scala.collection.mutable.HashMap

/** A Server stores a timestamp for all objects, with an additional file hash
  * and timestamp-hash chain for files.
  */
sealed trait ServerData {
  val time: Timestamp
}

case class FileData (val time: Timestamp,
                     val hash: FileHash,
                     val chain: List[(Timestamp, FileHash)])

case class DirData (val time: Timestamp)


/** A ServerMap is essentially a pair of HashMaps, one for storing file information
  * and one for storing directory information. This data structure serves to manage
  * this pair effectively. Note that we could use a regular HashMap, but the lack
  * of true dependent types in Scala makes creating a dependent HashMap that can
  * enforce the constraint of FSFile => FileData and FSDirectory => DirData a bit
  * awkward. AFAIK, this is the simplest solution.
  */
class ServerMap {


  private val dirMap = new HashMap[FSDirectory, DirData]
  private val fileMap = new HashMap[FSFile, FileData]


  def apply (fsObj: FSObject) : ServerData = fsObj match {
    case file: FSFile => fileMap(file)
    case dir: FSDirectory => dirMap(dir)
  }


  def applyDir (dir: FSDirectory) : DirData = dirMap(dir)


  def applyFile (file: FSFile) : FileData = fileMap(file)


  def lookupPath (path: FSPath) : Option[ServerData] = {
    dirMap.get(FSDirectory(path)) match {
      case None => {
        fileMap.get(FSFile(path)) match {
          case None => None
          case some => some
        }
      }
      case some => some
    }
  }


  def update (fsObj: FSObject, data: ServerData) = {
    (fsObj, data) match {
      case (file: FSFile, fileData: FileData) => {
        dirMap.remove(FSDirectory(file.path))
        fileMap(file) = fileData
      }

      case (dir: FSDirectory, dirData: DirData) => {
        fileMap.remove(FSFile(dir.path))
        dirMap(dir) = dirData
      }
    }
  }

}
