// A convenient capsule for sending a file name and hash list over the wire
case class FileListMessage(val fileList: List[(String, Array[Byte])])