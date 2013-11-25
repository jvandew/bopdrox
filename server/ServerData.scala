package bopdrox.server

import bopdrox.util.MapData

// A Server ideally keeps track of the timestamp-hash chain for each file
case class ServerData (val time: Long, val hash: Array[Byte], val chain: List[(Long, Array[Byte])])