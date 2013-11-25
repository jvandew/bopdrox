package bopdrox.server

// A Server ideally keeps track of the timestamp-hash chain for each file
case class ServerData (val time: Long, val hash: Array[Byte], val chain: List[(Long, Array[Byte])])