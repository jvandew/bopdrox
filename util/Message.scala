package bopdrox.util

import scala.util.Random

/** Algeraic datatype for Messages **/
sealed trait Message extends Serializable {
  val id = new String(Random.alphanumeric.take(8).toArray)
}


/** A message to be sent to acknowledge a received message when no other reply
  * is needed */
// TODO(jacob) this is likely not necessary
@SerialVersionUID(24601L)
case object Ack extends Message {

  override def toString () : String = id + " - Ack"
}


/** Connect to the Server. */
@SerialVersionUID(60000024601L)
case class Connect(val clientId: String) extends Message {

  override def toString () : String = id + " - Connect: " + clientId
}


sealed trait FLData extends Serializable {
  val fsObj: FSObject
}

@SerialVersionUID(10L)
case class FLDirectory (val dir: FSDirectory) extends FLData {
  val fsObj = dir
}

@SerialVersionUID(11L)
case class FLFile (val file: FSFile, val hash: Array[FileHash]) extends FLData {
  val fsObj = file
}

/** Send a list of FSObjects with file hashes over the wire */
@SerialVersionUID(20000024601L)
case class FSListMessage (val fsList: List[FLData]) extends Message {

  override def toString () : String = id + " - FSListMessage"
}


sealed trait FTData extends Serializable {
  val fsObj: FSObject
  val oldFSObj: Option[FLData]
}

@SerialVersionUID(20L)
case class FTDirectory (val dir: FSDirectory, val oldFSObj: Option[FLData])
    extends FTData {
  val fsObj = dir
}

@SerialVersionUID(21L)
case class FTFile (val file: FSFile,
                   val contents: FileBytes,
                   val hash: Array[FileHash],
                   val oldFSObj: Option[FLData])
    extends FTData {
  val fsObj = file
}

/** Transfer file contents over the wire */
@SerialVersionUID(10000024601L)
case class FSTransferMessage (val ftList: List[FTData]) extends Message {

  override def toString () : String =
    id + " - FSTransferMessage: " + ftList.map(_.fsObj).mkString(", ")
}


/** Send a request for a list of FSObjects */
@SerialVersionUID(30000024601L)
case class FSRequest (val files: List[FSObject]) extends Message {

  override def toString () : String = id + " - FSRequest"
}


sealed trait Rejection extends Serializable {
  val fsObj: FSObject
}


@SerialVersionUID(30L)
case class RejDirFile (val dir: FSDirectory,
                       val serverFile: FSFile,
                       val serverHash: Array[FileHash])
    extends Rejection {
  val fsObj = dir
}

@SerialVersionUID(31L)
case class RejFileDir (val file: FSFile, val hash: Array[FileHash], val dir: FSDirectory)
    extends Rejection {
  val fsObj = file
}

@SerialVersionUID(32L)
case class RejFileFile (val file: FSFile,
                        val hash: Array[FileHash],
                        val serverHash: Array[FileHash])
    extends Rejection {
  val fsObj = file
}

@SerialVersionUID(33L)
case class RejDirNone (val dir: FSDirectory)
    extends Rejection {
  val fsObj = dir
}

@SerialVersionUID(34L)
case class RejFileNone (val file: FSFile,
                        val hash: Array[FileHash])
    extends Rejection {
  val fsObj = file
}

/** Notify a Client of rejected file updates */
@SerialVersionUID(50000024601L)
case class RejectUpdateMessage (val rejections: List[Rejection]) extends Message {

  override def toString () : String =
    id + " - RejectUpdateMessage: " + rejections.map(_.fsObj).mkString(", ")
}


/** Transfer a list of files that have been deleted */
@SerialVersionUID(40000024601L)
case class FSRemovedMessage (val removed: List[FLData]) extends Message {

  override def toString () : String =
    id + " - FSRemovedMessage: " + removed.map(_.fsObj).mkString(", ")
}
