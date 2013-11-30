package bopdrox.msg

import scala.collection.mutable.HashMap

/* A convenient capsule for transferring a set of files that have been deleted over the wire
 * Option stores file hash to be verified. The expectation is that if the hash does not match
 * the message can be ignored. This would mean the file has been updated and will be
 * re-propogating through the network.
 */

case class RemovedMessage (val fileSet: HashMap[List[String], Option[Array[Byte]]]) extends Message {
  private val serialVersionUID = 40000024601L
}