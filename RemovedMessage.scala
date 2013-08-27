import scala.collection.Set

// A convenient capsule for transferring a set of files that have been deleted over the wire
case class RemovedMessage (val fileSet: Set[List[String]]) extends Message