package bopdrox.msg

// A convenient capsule for sending a request for a list of files
case class FileRequest (val files: List[List[String]]) extends Message {
  private val serialVersionUID = 30000024601L
}