package bopdrox.client

import bopdrox.util.{Message, Utils}
import java.io.{File, IOException, ObjectInputStream}

/* A ClientListener is a Runnable object designed to listen for file updates
 * from the server. */
class ClientListener (client: Client) (in: ObjectInputStream) extends Runnable {

  private def readObject: Option[Object] = Utils.checkedRead(client.disconnect)(in)

  def run : Unit = {
    while (true) {
      readObject match {
        case None => () // wait for termination
        case Some(msg: Message) => client.messageQueue.enqueue(msg)
        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}