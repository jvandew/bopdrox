package bopdrox.client

import bopdrox.util.{Message, Utils}
import java.io.{IOException, ObjectOutputStream}

/* A ClientMessenger is a Runnable object responsible for funneling fs updates
 * to the Server. */
class ClientMessenger (client: Client)
                      (out: ObjectOutputStream)
                      (handle: IOException => Unit)
    extends Runnable {

  private var curMsg: Option[Message] = None

  private def handler (ioe: IOException) : Unit = curMsg match {
    case None => handle(ioe)
    case Some(msg) => {
      client.synchronized {
        msg +=: client.sendQueue
        handle(ioe)
      }
    }
  }

  private val writeObject = Utils.checkedWrite(handler)(out)_

  def run : Unit = {

    while (true) {
      while (!client.sendQueue.isEmpty) {

        val msg = client.sendQueue.dequeue
        curMsg = Some(msg)
        writeObject(msg)
      }

      Thread.sleep(50)
    }

  }
}
