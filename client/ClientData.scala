package bopdrox.client

import bopdrox.util.{FileHash, FSData, FSDirData, FSFileData, Timestamp}

/** A Client stores only timestamps and hashes, no chains. */

sealed trait ClientData extends FSData

case class DirData (val time: Timestamp) extends ClientData with FSDirData

case class FileData (val time: Timestamp,
                     val hash: Array[FileHash])
    extends ClientData with FSFileData
