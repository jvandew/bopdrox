package bopdrox.msg

// TODO(jacob) modify this somehow to allow folders replacing files to transfer
// an oldHash. This may require defining a new three-case type to allow distinguishing
// folders from non-existence (which are currently both represented by None)

// Let's make our lives easier
case class FileMsgData(val bytes: Array[Byte], val oldHash: Option[Array[Byte]], val newHash: Array[Byte])

// A convenient capsule for transferring file names and contents over the wire
case class FileMessage (val fileContents: List[(List[String], Option[FileMsgData])])
    extends Message {
  private val serialVersionUID = 10000024601L
}