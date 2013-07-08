// A convenient capsule for sending file names and hashes over the wire
case class FileListMessage(val fileList: List[(String, Array[Byte])])