package bopdrox.client

import bopdrox.util.{Message, Utils}
import java.io.{IOException, ObjectOutputStream}

/* A ClientMessenger is a Runnable object responsible for funneling fs updates
 * to the Server. */
class ClientMessenger (client: Client)
                      (out: ObjectOutputStream)
                      (handle: IOException => Unit)
                      (debug: Boolean)
    extends Runnable {

  private[client] var continue = true
  private var msg: Message = null

  private def handler (ioe: IOException) : Unit = {

    /* TODO(jacob) Absolutely ESSENTIAL that we add a Messenger lock to prevent
     * a new Messenger from sending anything before this broken Message is
     * re-queued at the front. Synchronization is not sufficient.
     */
    client.sendQueue.synchronized {
      msg +=: client.sendQueue
    }

    client.synchronized {
      if (continue) {
        // Listener hasn't shut us down yet; commit murder-suicide
        continue = false
        handle(ioe)
      }
    }
  }

  private val writeObject = Utils.checkedWrite(handler)(out)_

  def run : Unit = {

    // must retrieve lock from previous Messenger to operate Queue safely
    client.messengerLock.acquire

    while (continue) {
      while (!client.sendQueue.isEmpty) {

        client.sendQueue.synchronized {
          msg = client.sendQueue.dequeue
        }

        if (debug) {
          println("DEBUG - Client sending Message:\n\t" + msg)
        }

        writeObject(msg)
      }

      Thread.sleep(100)
    }

    client.messengerLock.release
  }
}
