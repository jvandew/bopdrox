package bopdrox

package object util {

  /** The contents of a file are stored in Byte Arrays. **/
  type FileBytes = Array[Byte]

  /** A file hash is stored as a Byte Array. **/
  type FileHash = Array[Byte]

  /** A filesystem path is represented by a List of Strings, where each String
  * represents one segment (parent directory or object name) in the path.
  */
  type FSPath = List[String]

}