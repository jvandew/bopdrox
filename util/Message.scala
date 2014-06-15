package bopdrox.util

/** Algeraic datatype for Messages **/
sealed trait Message


/** A message to be sent to acknowledge a received message when no other reply
  * is needed */
// TODO(jacob) this is likely not necessary
case object Ack extends Message


sealed trait FLData {
  val fsObj: FSObject
}
case class FLDirectory (val dir: FSDirectory) extends FLData {
  val fsObj = dir
}
case class FLFile (val file: FSFile, val hash: FileHash) extends FLData {
  val fsObj = file
}

/** Send a list of FSObjects with file hashes over the wire */
case class FSListMessage (val fsList: List[FLData]) extends Message {
  private val serialVersionUID = 20000024601L
}


sealed trait FTData
case class FTDirectory (val dir: FSDirectory, val oldFSObj: Option[FLData])
    extends FTData
case class FTFile (val file: FSFile,
                   val contents: FileBytes,
                   val hash: FileHash,
                   val oldFSObj: Option[FLData])
    extends FTData

/** Transfer file contents over the wire */
case class FSTransferMessage (val ftList: List[FTData])
    extends Message {
  private val serialVersionUID = 10000024601L
}


/** Send a request for a list of FSObjects */
case class FSRequest (val files: List[FSObject]) extends Message {
  private val serialVersionUID = 30000024601L
}


sealed abstract class Rejection (val fsObj: FSObject)
case class RejDirFile (val dir: FSDirectory,
                       val serverFile: FSFile,
                       val serverHash: FileHash)
    extends Rejection(dir)
case class RejFileDir (val file: FSFile, val hash: FileHash, val dir: FSDirectory)
    extends Rejection(file)
case class RejFileFile (val file: FSFile,
                        val hash: FileHash,
                        val serverHash: FileHash)
    extends Rejection(file)
case class RejDirNone (val dir: FSDirectory)
    extends Rejection(dir)
case class RejFileNone (val file: FSFile,
                        val hash: FileHash)
    extends Rejection(file)

/** Notify a Client of rejected file updates */
case class RejectUpdateMessage (val rejections: List[Rejection])
    extends Message {
  private val serialVersionUID = 50000024601L
}


/** Transfer a list of files that have been deleted */
case class FSRemovedMessage (val removed: List[FLData]) extends Message {
  private val serialVersionUID = 40000024601L
}
