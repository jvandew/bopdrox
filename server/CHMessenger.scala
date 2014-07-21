package bopdrox.server

import bopdrox.util.{Message, Utils}
import java.io.{IOException, ObjectOutputStream}

/* A CHMessenger is a Runnable object responsible for passing updates from
 * Server to Client. */
class CHMessenger (ch: ClientHandler) (out: ObjectOutputStream) extends Runnable {

  private[server] var continue = true
  private var msg: Message = null

  private def handler (ioe: IOException) : Unit = {

    ch.sendQueue.synchronized {
      msg +=: ch.sendQueue
    }

    continue = false
  }

  private val writeObject = Utils.checkedWrite(handler)(out)_

  def run : Unit = {

    // must retrieve lock from previous Messenger to operate Queue safely
    ch.messengerLock.acquire

    while (continue) {
      while (!ch.sendQueue.isEmpty) {

        ch.sendQueue.synchronized {
          msg = ch.sendQueue.dequeue
        }

        writeObject(msg)
      }

      Thread.sleep(100)
    }

    ch.messengerLock.release
  }
}
