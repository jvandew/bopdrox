package bopdrox.util

/** A filesystem object is either a file or a directory */
sealed trait FSObject {
  val path: FSPath
}

case class FSFile (val path: FSPath) extends FSObject
case class FSDirectory (val path: FSPath) extends FSObject