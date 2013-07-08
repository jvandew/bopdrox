// A convenient capsule for transferring file names and contents over the wire
// tuple in order is file path, contents, and hash
case class FileMessage(val fileContents: List[(String, Array[Byte], Array[Byte])])