package bopdrox.msg

// simplify our lives
case class RejectMsgData(val givenHash: Option[Array[Byte]], val serverHash: Option[Array[Byte]])

// A convenient capsule for notifying a Client of rejected file updates
case class RejectUpdateMessage (val fileHashes: List[(List[String], RejectMsgData)])
    extends Message {
  private val serialVersionUID = 50000024601L
}