package bopdrox.server

import bopdrox.msg.{Ack, FileListMessage, FileMessage, FileMsgData, FileRequest, Message, RemovedMessage}
import bopdrox.util.Utils
import java.io.{File, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.Socket
import scala.collection.mutable.{HashMap, HashSet}

/* A ClientHandler is a Runnable object designed to handle all communications
 * with a Client */
class ClientHandler (client: Socket) (home: File) extends Runnable {

  private val in = new ObjectInputStream(client.getInputStream)
  private val out = new ObjectOutputStream(client.getOutputStream)

  val getRelPath = Utils.getRelativePath(home) _

  // continue running this handler
  private var continue = true

  // disconnect client and mark handler to terminate
  private def disconnect (ioe: IOException) : Unit = {
    println("disconnecting client: " + Utils.printSocket(client))

    Server.synchronized {
      Server.clients = Server.clients.diff(List(this))
    }

    continue = false
  }

  private def readObject: Option[Object] = Utils.checkedRead(disconnect)(in)
  private val writeObject = Utils.checkedWrite(disconnect)(out)_

  /* Handle message matching and file operations. Note that due the variable
   * nature of network communications it does not make sense to enforce FIFO
   * ordering of message handling, only that the file operation + hash map
   * update is atomic. */
  private def matchMessage (msg: Message) : Unit = msg match {
    case FileMessage(fileContents) => {
      println("handling FileMessage... ")

      fileContents.foreach {
        _ match {
          // empty directory
          case (subpath, None) => {
            println(subpath)
            Server.hashes.synchronized {
              val emptyDir = Utils.newFile(home, subpath)
              if (emptyDir.exists) emptyDir.delete
              emptyDir.mkdirs
              Server.hashes.update(subpath, None)
            }
          }

          // normal file
          case (subpath, Some(msgData)) => {
            println(subpath)
            val file = Utils.newFile(home, subpath)

            Server.hashes.synchronized {

              val chain = Server.hashes.get(subpath) match {
                case None => {  // new file
                  Utils.ensureDir(home, subpath)
                  Utils.findParent(home)(subpath)(f => Server.hashes.contains(getRelPath(f))) match {
                    case `subpath` => ()  // no previously empty folder to remove
                    case parent => Server.hashes.remove(parent)
                  }
                  Nil
                }

                case Some(None) => {  // empty directory is now a file
                  file.delete
                  Nil
                }

                case Some(Some(ServerData(pTime, pHash, pChain))) => (pTime, pHash)::pChain // updated file
              }

              Utils.writeFile(file)(msgData.bytes)
              Server.hashes.update(subpath, Some(ServerData(file.lastModified, msgData.newHash, chain)))
            }
          }

        }
      }

      println("done")
    }

    case RemovedMessage(fileMap) => {
      println("handling RemovedMessage... ")

      fileMap.foreach { nameHash =>
        println(nameHash._1)

        // TODO(jacob) this synchronization is expensive...
        Server.hashes.synchronized {

          Server.hashes.remove(nameHash._1)
          val file = Utils.newFile(home, nameHash._1)
          
          nameHash._2 match {
            case None => {

              file.list match {
                case Array() => file.delete // empty folder. nothing to see here
                case _ => Utils.dirForeach(file) { delFile =>
                  Server.hashes.remove(getRelPath(delFile))
                  delFile.delete // would happen in dirDelete, but more efficient here
                }
                { delDir =>
                  Server.hashes.remove(getRelPath(delDir))
                  delDir.delete // would happen in dirDelete, but more efficient here
                }
              }

              Utils.dirDelete(file)
            }

            case Some(hash) => file.delete
          }
        }
      }

      println("done")
    }
  }

  def message (msg: Message) : Unit = writeObject(msg)

  def run : Unit = {

    println("client connected: " + Utils.printSocket(client))

    // send client a list of file names and hashes; do not synchronize on read
    val fhList = Server.hashes.toList.map {
      _ match {
        case (subpath, None) => (subpath, None)
        case (subpath, Some(fileData)) => (subpath, Some(fileData.hash))
      }
    }

    val listMsg = FileListMessage(fhList)

    writeObject(listMsg)

    readObject match {
      case None => () // wait for termination
      case Some(FileRequest(files)) => {

        val fileList = files.map { filename =>
          Server.hashes(filename) match {
            case None => (filename, None)
            case Some(fileData) => {
              val bytes = Utils.readFile(home, filename)
              (filename, Some(FileMsgData(bytes, None, fileData.hash)))
            }
          }
        }

        val msg = FileMessage(fileList)
        writeObject(msg)

        readObject match {
          case None => () // wait for termination
          case Some(Ack) => ()
          case Some(_) => throw new IOException("Unknown or incorrect message received")
        }
      }
      case Some(_) => throw new IOException("Unknown or incorrect message received")
    }

    // main loop to listen for updated files
    while (continue) {
      readObject match {
        case None => () // wait for termination
        case Some(msg: Message) => {

          // forward message
          Server.clients.foreach { c =>
            if (!c.equals(this))
              c.message(msg)
          }

          matchMessage(msg)
        }

        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}