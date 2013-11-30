package bopdrox.msg

// Let's make our lives easier
case class FileMsgData(val bytes: Array[Byte], val oldHash: Option[Array[Byte]], val newHash: Array[Byte])

// A convenient capsule for transferring file names and contents over the wire
case class FileMessage (val fileContents: List[(List[String], Option[FileMsgData])])
    extends Message {
  private val serialVersionUID = 10000024601L
}