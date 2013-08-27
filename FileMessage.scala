// A convenient capsule for transferring file names and contents over the wire
// tuple in order is file path, contents, and hash
case class FileMessage (val fileContents: List[(List[String], Option[(Array[Byte], Array[Byte])])]) extends Message