package bopdrox.client

import bopdrox.util.{Ack, Connect, FLData, FLDirectory, FLFile, FSDirectory,
                     FSFile, FSInitMessage, FSListMessage, FSRemovedMessage,
                     FSRequest, FSTransferMessage, FTData, FTDirectory,
                     FTFile, Message, RejectUpdateMessage, Utils}
import java.io.{File, FileInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.{Socket, UnknownHostException}
import scala.collection.mutable.{HashMap, Queue}
import scala.concurrent.Lock
import scala.util.Random

// companion object for running a Client
object Client {

  /* Takes a home folder and the address of a running Server */
  def main (args: Array[String]) : Unit = {

    val home = new File(args(0))
    val Array(host, port) = args(1).split(':')
    val debug = if (args.length >= 3 && args(2) == "-debug") true else false

    val client = new Client(home)(host)(port.toInt)(debug)
    client.run
  }
}

class Client (val home: File) (val host: String) (val port: Int) (debug: Boolean) extends Runnable {

  val hashes = new ClientMap

  val getRelPath = Utils.getRelativePath(home)_

  // store received Messages for later consumption
  private[client] val messageQueue = new Queue[Message]
  private[client] val sendQueue = new Queue[Message]

  private var listener: ClientListener = null
  private var messenger: ClientMessenger = null
  private[client] val messengerLock = new Lock

  val id = new String(Random.alphanumeric.take(16).toArray)

  private var open = false


  def connect () : Unit = {

    val serv = new Socket(host, port)
    val out = Utils.getObjectOutputStream(serv)
    val in = Utils.getObjectInputStream(serv)

    def readObject: Option[Object] = Utils.checkedRead(disconnect)(in)
    val writeObject = Utils.checkedWrite(disconnect)(out)_

    writeObject(Connect(id))

    // get list of files and hashes from server
    // TODO(jacob) currently assumes Client files are a subset of Server files
    readObject match {
      case None => () // wait for error handler
      case Some(FSListMessage(fsList)) => {

        // generate local file list and hashes
        Utils.dirForeach(home) { file =>
          val hash = Utils.hashFile(file)
          val time = file.lastModified
          val length = file.length
          val fsFile = FSFile(getRelPath(file))

          hashes(fsFile) = FileData(time, length, hash)
        }
        { dir =>
          val time = dir.lastModified
          val fsDir = FSDirectory(getRelPath(dir))

          hashes(fsDir) = DirData(time)
        }

        val filtered = fsList.filter { flData =>
          (flData, hashes.lookupPath(flData.fsObj.path)) match {
            case (_, None) => true
            case (_: FLDirectory, Some(_: DirData)) => false
            case (_: FLDirectory, Some(_: FileData)) => true
            case (_: FLFile, Some(_: DirData)) => true
            case (FLFile(_, hash1), Some(FileData(_, _, hash2))) => !Utils.verifyHash(hash1)(hash2)
          }
        }

        val msg = FSRequest(filtered.map(_.fsObj))
        writeObject(msg)

        readObject match {
          case None => () // wait for termination
          case Some(FSInitMessage(ftList)) => {

            // Process list of new files
            ftList.foreach {
              _ match {

                case FTDirectory(dir, _) => {
                  val emptyDir = Utils.newDir(home, dir)
                  hashes(dir) = DirData(emptyDir.lastModified)
                }

                case FTFile(fsFile, contents, hash, _) => {

                  Utils.ensureDir(home, fsFile.path)
                  val file = Utils.newFile(home, fsFile)

                  Utils.writeFile(file)(contents)
                  hashes(fsFile) = FileData(file.lastModified, file.length, hash)
                }
              }
            }

            writeObject(Ack)
          }
          case Some(_) => throw new IOException("Unknown or incorrect message received")
        }
      }
      case Some(_) => throw new IOException("Unknown or incorrect message received")
    }

    // begin networking Threads
    listener = new ClientListener(this)(in)(reconnectHandler)(debug)
    messenger = new ClientMessenger(this)(out)(reconnectHandler)(debug)

    new Thread(listener).start
    new Thread(messenger).start

    open = true

    println("Ready for action!")
  }


  // disconnect handler
  private def disconnect (ioe: IOException) : Unit = {
    println("IOException received! Initiating hara-kiri...")
    ioe.printStackTrace
    sys.exit
  }


  // included for testing purposes
  def isOpen : Boolean = open


  // note this method is not called asnchronously
  private def matchMessage (msg: Message) : Unit = {

    msg match {

      case FSTransferMessage(FTDirectory(dir, _)) => {
        val emptyDir = Utils.newDir(home, dir)
        hashes(dir) = DirData(emptyDir.lastModified)
      }

      case FSTransferMessage(FTFile(fsFile, contents, hash, _)) => {
        val file = Utils.newFile(home, fsFile)

        hashes.lookupPath(fsFile.path) match {
          case None => Utils.ensureDir(home, fsFile.path)
          case Some(_: FileData) => ()
          case Some(_: DirData) => file.delete
        }

        Utils.writeFile(file)(contents)
        hashes(fsFile) = FileData(file.lastModified, file.length, hash)
      }

      case RejectUpdateMessage(rejections) => ()  // will receive corrections from Server momentarily

      case FSRemovedMessage(removed) => {
        removed.foreach { rem =>
          val (fsObj, file) = rem match {
            case FLFile(fsFile, _) => (fsFile, Utils.newFile(home, fsFile))
            case FLDirectory(fsDir) => (fsDir, Utils.newDir(home, fsDir))
          }

          hashes.remove(fsObj)
          Utils.dirDelete(file)
        }
      }

      case _ => throw new IOException("Unknown or incorrect message received")
    }

    if (debug) {
      println("DEBUG - Client " + id + " processed Message:\n\t" + msg)
    }
  }


  def pollFileSystem () : Unit = {

    // main loop to check for updated files
    while (true) {

      var keySet = hashes.keySet
      var updates = List[FTData]()

      Utils.dirForeach(home) { file =>

        val time = file.lastModified
        val length = file.length
        val path = getRelPath(file)
        val fsFile = FSFile(path)

        hashes.lookupPath(path) match {

          case None => {
            val (bytes, hash) = Utils.contentsAndHash(file)

            hashes(fsFile) = FileData(time, length, hash)
            updates ::= FTFile(fsFile, bytes, hash, None)
          }

          case Some(fileData: FileData) => {
            // TODO(jacob) it's extremely unlikely that changing the system
            //             clock could break this check
            if (time != fileData.time && length != fileData.length) {
              val (bytes, hash) = Utils.contentsAndHash(file)
              val oldFSObj = FLFile(fsFile, fileData.hash)

              hashes(fsFile) = FileData(time, length, hash)
              updates ::= FTFile(fsFile, bytes, hash, Some(oldFSObj))
              keySet -= fsFile
            }
            else {
              keySet -= fsFile
            }
          }

          case Some(dirData) => {
            val fsDir = FSDirectory(path)
            val oldFSObj = FLDirectory(fsDir)
            val (bytes, hash) = Utils.contentsAndHash(file)

            hashes.remove(fsDir)
            hashes(fsFile) = FileData(time, length, hash)
            updates ::= FTFile(fsFile, bytes, hash, Some(oldFSObj))
            keySet -= fsDir
          }
        }
      }
      { dir =>

        val path = getRelPath(dir)
        val fsDir = FSDirectory(path)

        hashes.lookupPath(path) match {

          case None => {
            hashes(fsDir) = DirData(dir.lastModified)
            updates ::= FTDirectory(fsDir, None)
          }

          case Some(fileData: FileData) => {

            val fsFile = FSFile(path)
            val oldFSObj = FLFile(fsFile, fileData.hash)

            hashes.remove(fsFile)
            hashes(fsDir) = DirData(dir.lastModified)
            updates ::= FTDirectory(fsDir, Some(oldFSObj))
            keySet -= fsFile
          }

          case Some(dirData) => {
            keySet -= fsDir
          }
        }
      }

      var removed = List[FLData]()
      keySet.foreach { fsObj =>

        hashes.remove(fsObj) match {

          case None => () // SHOULD never happen

          case Some(dirData: DirData) =>
            removed ::= FLDirectory(fsObj.asInstanceOf[FSDirectory])

          case Some(fileData: FileData) =>
            removed ::= FLFile(fsObj.asInstanceOf[FSFile], fileData.hash)
        }

        // determine if we deleted a parent folder or just a file or empty folder
        val path = fsObj.path
        Utils.getDeleted(home, path) match {
          case `path` => ()
          case deletedPath =>
            removed ::= FLDirectory(FSDirectory(fsObj.path))
        }
      }

      updates.foreach { update =>
        sendQueue.synchronized {
          sendQueue.enqueue(FSTransferMessage(update))
        }
      }

      removed match {
        case Nil => ()  // nothing removed
        case _ => {
          val msg = FSRemovedMessage(removed)
          sendQueue.synchronized {
            sendQueue.enqueue(msg)
          }
        }
      }

      // process any received Messages
      // TODO(jacob) this could be an inconvenient way of handling Messages if we have a
      // large Queue (unlikely with a single user) or a significantly large directory tree
      while (!messageQueue.isEmpty) {

        val msg = messageQueue.synchronized {
          messageQueue.dequeue
        }

        matchMessage(msg)
      }

      Thread.sleep(250)
    }
  }


  /* TODO(jacob) the recursive nature of this method may lead to a stack overflow
   * if we repeatedly fail to reconnect. I don't believe the compiler is smart
   * enough to optimize this tail call.
   *
   * Note that this method is synchronized on Client within Listener and Messenger.
   */
  private def reconnectHandler (ioe: IOException) : Unit = {

    println("Something has gone terribly wrong. Reconnecting...")

    try {
      val serv = new Socket(host, port)
      val out = Utils.getObjectOutputStream(serv)
      val in = Utils.getObjectInputStream(serv)

      Utils.checkedWrite(e => throw e)(out)(Connect(id))

      listener.continue = false
      messenger.continue = false

      listener = new ClientListener(this)(in)(reconnectHandler)(debug)
      messenger = new ClientMessenger(this)(out)(reconnectHandler)(debug)

      new Thread(listener).start
      new Thread(messenger).start

    } catch {
      case _: IOException => reconnectHandler(ioe)
      case _: UnknownHostException => reconnectHandler(ioe)
    }

    println("...And we're back!")

  }


  def run : Unit = {
    // connect and manage files
    connect
    pollFileSystem
  }

}
