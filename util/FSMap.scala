package bopdrox.util

import scala.collection.mutable.HashMap
import scala.collection.Set

/** Basic data to store in an FSMap */
trait FSData {
  val time: Timestamp
}

trait FSDirData extends FSData {
  val time: Timestamp
}

trait FSFileData extends FSData {
  val time: Timestamp
  val hash: FileHash
}


/** A FSMap is essentially a pair of HashMaps, one for storing file information
  * and one for storing directory information. This data structure serves to manage
  * this pair effectively. Note that we could use a regular HashMap, but the lack
  * of true dependent types in Scala makes creating a dependent HashMap that can
  * enforce the constraint of FSFile => FileData and FSDirectory => DirData a bit
  * awkward. AFAIK, this is the simplest solution.
  */
class FSMap[Data <: FSData, DirData <: Data with FSDirData, FileData <: Data with FSFileData] {

  private val dirMap = new HashMap[FSDirectory, DirData]
  private val fileMap = new HashMap[FSFile, FileData]


  def apply (fsObj: FSObject) : Data = fsObj match {
    case file: FSFile => fileMap(file)
    case dir: FSDirectory => dirMap(dir)
  }


  def applyDir (dir: FSDirectory) : DirData = dirMap(dir)


  def applyFile (file: FSFile) : FileData = fileMap(file)


  def lookupPath (path: FSPath) : Option[Data] = {
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


  def keySet () : Set[FSObject] = dirMap.keySet ++ fileMap.keySet


  def remove (fsObj: FSObject) : Option[Data] = {
    fsObj match {
      case fsDir: FSDirectory => dirMap.remove(fsDir)
      case fsFile: FSFile => fileMap.remove(fsFile)
    }
  }


  def flList: List[FLData] =
    dirMap.toList.map(kv => FLDirectory(kv._1)) ++
    fileMap.toList.map(kv => FLFile(kv._1, kv._2.hash))


  def update (file: FSFile, fileData: FileData) : Unit = {
    dirMap.remove(FSDirectory(file.path))
    fileMap(file) = fileData
  }


  def update (dir: FSDirectory, dirData: DirData) : Unit = {
    fileMap.remove(FSFile(dir.path))
    dirMap(dir) = dirData
  }

}
