// A convenient capsule for transferring a file name and contents over the wire
case class FileMessage(val files: List[(String, Array[Byte])])