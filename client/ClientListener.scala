package bopdrox.client

import bopdrox.util.{Message, Utils}
import java.io.{IOException, ObjectInputStream}

/* A ClientListener is a Runnable object designed to listen for file updates
 * from the server. */
class ClientListener (client: Client)
                     (in: ObjectInputStream)
                     (handle: IOException => Unit)
                     (debug: Boolean)
    extends Runnable {

  private[client] var continue = true


  private def handler (ioe: IOException) : Unit = client.synchronized {

    if (continue) {
      // Messenger hasn't shut us down yet; commit murder-suicide
      continue = false
      handle(ioe)
    }
  }

  private def readObject: Option[Object] = Utils.checkedRead(handler)(in)

  def run : Unit = {

    while (continue) {
      readObject match {
        case None => ()

        case Some(msg: Message) => {
          if (debug) {
            println("DEBUG - Client " + client.id + " received Message:\n\t" + msg)
          }

          client.messageQueue.synchronized {
            client.messageQueue.enqueue(msg)
          }
        }

        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}
