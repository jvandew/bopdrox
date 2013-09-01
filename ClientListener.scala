import java.io.{File, IOException, ObjectInputStream}

/* A ClientListener is a Runnable object designed to listen for file updates
 * from the server. */
class ClientListener (in: ObjectInputStream) (home: File) extends Runnable {

  private def readObject: Option[Object] = Utils.checkedRead(Client.disconnect)(in)

  // TODO(jacob) this likely causes race conditions
  def run : Unit = {
    while (true) {
      readObject match {
        case None => () // wait for termination
        case Some(FileMessage(fileContents)) => {
          print("handling FileMessage... ")

          // TODO(jacob) this code is mostly copy-pasted from ClientHandler. fix
          fileContents.foreach {
            _ match {
              // empty directory
              case (subpath, None) => {
                val emptyDir = Utils.newFile(home, subpath)
                emptyDir.mkdirs
                Client.hashes.update(subpath, None)
              }
              // normal file
              case (subpath, Some((bytes, hash))) => {
                Utils.ensureDir(home, subpath)
                val file = Utils.newFile(home, subpath)
                Utils.writeFile(file)(bytes)
                Client.hashes.update(subpath, Some(MapData(file.lastModified, hash)))
              }
            }
          }

          println("done")
        }

        case Some(RemovedMessage(fileSet)) => {
          print("handling RemovedMessage... ")

          fileSet.foreach { filename =>
            Client.hashes.remove(filename)
            val file = Utils.newFile(home, filename)
            file.delete
          }

        println("done")
        }

        case Some(_) => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}