package bopdrox.server

import bopdrox.util.{FileHash, FSData, FSDirData, FSFileData, Timestamp}

/** A Server stores a timestamp for all objects, with an additional file hash
  * and timestamp-hash chain for files.
  */

sealed trait ServerData extends FSData

case class DirData (val time: Timestamp) extends ServerData with FSDirData

case class FileData (val time: Timestamp,
                     val hash: FileHash,
                     val chain: List[(Timestamp, FileHash)])
    extends ServerData with FSFileData
