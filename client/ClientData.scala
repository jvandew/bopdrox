package bopdrox.client

// A Client only needs to store the current timestamp and hash
case class ClientData (val time: Long, val hash: Array[Byte])