package client

import java.io.{File, IOException, ObjectInputStream}

import messages.Message
import util.Utils

/* A ClientListener is a Runnable object designed to listen for file updates
 * from the server. */
class ClientListener (in: ObjectInputStream) (home: File) extends Runnable {

  private def readObject: Option[Object] = Utils.checkedRead(Client.disconnect)(in)

  def run : Unit = {
    while (true) {
      readObject match {
        case None => () // wait for termination
        case Some(msg: Message) => Client.messageQueue.enqueue(msg)
        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}