package bopdrox

import bopdrox.util.{FileHash, FSData, FSDirData, FSFileData, FSMap, Timestamp}

package object client {

  /** A Client stores only timestamps and hashes, no chains. */

  sealed trait ClientData extends FSData

  case class DirData (val time: Timestamp) extends ClientData with FSDirData

  case class FileData (val time: Timestamp,
                       val hash: FileHash)
      extends ClientData with FSFileData

  type ClientMap = FSMap[ClientData, DirData, FileData]

}