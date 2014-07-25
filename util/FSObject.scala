package bopdrox.util

/** A filesystem object is either a file or a directory */
sealed trait FSObject extends Serializable {

  val path: FSPath
  override def toString () : String = Utils.joinPath(path)

}

@SerialVersionUID(0L)
case class FSDirectory (val path: FSPath) extends FSObject

@SerialVersionUID(1L)
case class FSFile (val path: FSPath) extends FSObject
