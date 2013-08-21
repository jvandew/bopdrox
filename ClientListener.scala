import java.io.{File, IOException, ObjectInputStream}

/* A ClientListener is a Runnable object designed to listen for file updates
 * from the server. */
class ClientListener (in: ObjectInputStream) (home: File) extends Runnable {

  // TODO(jacob) this likely causes race conditions
  def run : Unit = {
    while (true) {
      in.readObject match {
        case FileMessage(fileContents) => {
          print("handling FileMessage... ")

          fileContents.foreach { pbh =>
            val path = home + File.separator + pbh._1
            Utils.ensureDir(path)

            val file = new File(path)
            Utils.writeFile(file)(pbh._2)
            // TODO(jacob) this is not safe with multiple clients
            // TODO(jacob) verify correct hash
            Client.hashes.update(pbh._1, MapData(file.lastModified, pbh._3))
          }

          println("done")
        }

        case RemovedMessage(fileSet) => {
          print("handling RemovedMessage... ")

          fileSet.foreach { filename =>
            Client.hashes.remove(filename)
            val file = new File(home + File.separator + filename)
            file.delete
          }

        println("done")
        }

        case _ => throw new IOException("Unknown or incorrect message received")
      }
    }

  }
}