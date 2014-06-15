package bopdrox.client

import bopdrox.util.{FileHash, FLData, FLDirectory, FLFile, FSDirectory, FSFile, FSObject, FSPath, Timestamp}
import scala.collection.mutable.HashMap

/** A Client stores only timestamps and hashes, no chains. */
sealed trait ClientData {
  val time: Timestamp
}

case class FileData (val time: Timestamp,
                     val hash: FileHash)
    extends ClientData

case class DirData (val time: Timestamp) extends ClientData


/** A ClientMap is essentially a pair of HashMaps, one for storing file information
  * and one for storing directory information. This data structure serves to manage
  * this pair effectively. Note that we could use a regular HashMap, but the lack
  * of true dependent types in Scala makes creating a dependent HashMap that can
  * enforce the constraint of FSFile => FileData and FSDirectory => DirData a bit
  * awkward. AFAIK, this is the simplest solution.
  *
  * TODO(jacob): Create a single new bopdrox.util.FSMap which takes type parameters
  *              and use this map instead for both Client and Server.
  */
class ClientMap {

  private val dirMap = new HashMap[FSDirectory, DirData]
  private val fileMap = new HashMap[FSFile, FileData]


  def apply (fsObj: FSObject) : ClientData = fsObj match {
    case file: FSFile => fileMap(file)
    case dir: FSDirectory => dirMap(dir)
  }


  def applyDir (dir: FSDirectory) : DirData = dirMap(dir)


  def applyFile (file: FSFile) : FileData = fileMap(file)


  def lookupPath (path: FSPath) : Option[ClientData] = {
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


  def remove (fsObj: FSObject) : Unit = {
    fsObj match {
      case fsDir: FSDirectory => dirMap.remove(fsDir)
      case fsFile: FSFile => fileMap.remove(fsFile)
    }
  }


  def flList: List[FLData] =
    dirMap.toList.map(kv => FLDirectory(kv._1)) ++
    fileMap.toList.map(kv => FLFile(kv._1, kv._2.hash))


  def update (fsObj: FSObject, data: ClientData) : Unit = {
    (fsObj, data) match {
      case (file: FSFile, fileData: FileData) => {
        dirMap.remove(FSDirectory(file.path))
        fileMap(file) = fileData
      }

      case (dir: FSDirectory, dirData: DirData) => {
        fileMap.remove(FSFile(dir.path))
        dirMap(dir) = dirData
      }

      case _ => throw new IllegalArgumentException("Data type and FSObject type must match.")
    }
  }

}
