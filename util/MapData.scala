package util

case class MapData (val time: Long, val hash: Array[Byte]) extends Product2[Long, Array[Byte]] {
  val _1 = time
  val _2 = hash
}