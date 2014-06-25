package bopdrox.util

/** Algeraic datatype for Messages **/
sealed trait Message extends Serializable


/** A message to be sent to acknowledge a received message when no other reply
  * is needed */
// TODO(jacob) this is likely not necessary
@SerialVersionUID(24601L)
case object Ack extends Message


sealed trait FLData extends Serializable {
  val fsObj: FSObject
}

@SerialVersionUID(10L)
case class FLDirectory (val dir: FSDirectory) extends FLData {
  override val fsObj = dir
  private val serialVersionUID = 10L
}

@SerialVersionUID(11L)
case class FLFile (val file: FSFile, val hash: FileHash) extends FLData {
  override val fsObj = file
  private val serialVersionUID = 11L
}

/** Send a list of FSObjects with file hashes over the wire */
@SerialVersionUID(20000024601L)
case class FSListMessage (val fsList: List[FLData]) extends Message


sealed trait FTData extends Serializable {
  val fsObj: FSObject
  val oldFSObj: Option[FLData]
}

@SerialVersionUID(20L)
case class FTDirectory (val dir: FSDirectory, override val oldFSObj: Option[FLData])
    extends FTData {
  override val fsObj = dir
  private val serialVersionUID = 20L
}

@SerialVersionUID(21L)
case class FTFile (val file: FSFile,
                   val contents: FileBytes,
                   val hash: FileHash,
                   override val oldFSObj: Option[FLData])
    extends FTData {
  override val fsObj = file
  private val serialVersionUID = 21L
}

/** Transfer file contents over the wire */
@SerialVersionUID(10000024601L)
case class FSTransferMessage (val ftList: List[FTData]) extends Message


/** Send a request for a list of FSObjects */
@SerialVersionUID(30000024601L)
case class FSRequest (val files: List[FSObject]) extends Message


sealed trait Rejection extends Serializable {
  val fsObj: FSObject
}

@SerialVersionUID(30L)
case class RejDirFile (val dir: FSDirectory,
                       val serverFile: FSFile,
                       val serverHash: FileHash)
    extends Rejection {
  override val fsObj = dir
  private val serialVersionUID = 30L
}

@SerialVersionUID(31L)
case class RejFileDir (val file: FSFile, val hash: FileHash, val dir: FSDirectory)
    extends Rejection {
  override val fsObj = file
  private val serialVersionUID = 31L
}

@SerialVersionUID(32L)
case class RejFileFile (val file: FSFile,
                        val hash: FileHash,
                        val serverHash: FileHash)
    extends Rejection {
  override val fsObj = file
  private val serialVersionUID = 32L
}

@SerialVersionUID(33L)
case class RejDirNone (val dir: FSDirectory)
    extends Rejection {
  override val fsObj = dir
  private val serialVersionUID = 33L
}

@SerialVersionUID(34L)
case class RejFileNone (val file: FSFile,
                        val hash: FileHash)
    extends Rejection {
  override val fsObj = file
  private val serialVersionUID = 34L
}

/** Notify a Client of rejected file updates */
@SerialVersionUID(50000024601L)
case class RejectUpdateMessage (val rejections: List[Rejection]) extends Message


/** Transfer a list of files that have been deleted */
@SerialVersionUID(40000024601L)
case class FSRemovedMessage (val removed: List[FLData]) extends Message
