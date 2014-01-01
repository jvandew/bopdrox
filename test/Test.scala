package bopdrox.test

import bopdrox.client.{Client, ClientData}
import bopdrox.server.{Server, ServerData}
import bopdrox.util.Utils
import java.io.{File, FileOutputStream}
import java.util.Date
import scala.collection.mutable.HashMap

/* A simple test framework for Bopdrox. Creates several test directories and spawns
 * a Server and several Clients to test some basic operations. The results of each
 * test are printed to the console.
 * NOTE THAT THESE TESTS ARE NOT COMPREHENSIVE! In particular they do not test
 * performance over an actual network. */
object Test {

  private def buildEmptyDirs (serverDir: File) : Unit = {
    val emptyDir1 = new File(serverDir, "empty_dir")
    emptyDir1.mkdir

    val emptyDir2Path = Utils.joinPath(List("nested", "nested", "nested", "empty_dir"))
    val emptyDir2 = new File(serverDir, emptyDir2Path)
    emptyDir2.mkdir
  }

  private def buildEmptyFiles (serverDir: File) : Unit = {
    val emptyFile1 = new File(serverDir, "empty.test")
    emptyFile1.createNewFile
    if (!Utils.isEmptyFile(emptyFile1)) Utils.writeFileString(emptyFile1)("")

    val emptyFile2Path = Utils.joinPath(List("nested", "empty.test"))
    val emptyFile2 = new File(serverDir, emptyFile2Path)
    emptyFile2.createNewFile
    if (!Utils.isEmptyFile(emptyFile2)) Utils.writeFileString(emptyFile2)("")

    val emptyFile3Path = Utils.joinPath(List("nested", "nested", "nested", "empty"))
    val emptyFile3 = new File(serverDir, emptyFile3Path)
    emptyFile3.createNewFile
    if (!Utils.isEmptyFile(emptyFile3)) Utils.writeFileString(emptyFile3)("")
  }

  private def buildNestedDirs (serverDir: File) : Unit = {
    val nestedsPath = Utils.joinPath(List("nested", "nested", "nested"))
    val nesteds = new File(serverDir, nestedsPath)
    nesteds.mkdirs
  }

  private def buildNonEmptyFiles (serverDir: File) : Unit = {
    val file1 = new File(serverDir, "test")
    file1.createNewFile
    Utils.writeFileString(file1)("this is a test\n\n")

    val file2 = new File(serverDir, "test2")
    file2.createNewFile
    Utils.writeFileString(file2)("this is a test\n\n")

    val file3Path = Utils.joinPath(List("nested", "testFile"))
    val file3 = new File(serverDir, file3Path)
    file3.createNewFile
    Utils.writeFileString(file3)("\nI'm\n\talso\n a \n test!\n\n")

    val file4Path = Utils.joinPath(List("nested", "nested", "nested", "test"))
    val file4 = new File(serverDir, file4Path)
    file4.createNewFile

    val file4out = new FileOutputStream(file4)
    file4out.write("I'm a looooooooooooooooooooooooooooooooooooooooooo\n".getBytes)
    var lineCount = 0

    // TODO(jacob) this could probably be further optimized in some way
    while (lineCount < 100000) {
      file4out.write("oooooooooooooooooooooooooooooooooooooooooooooooooo\noooooooooooooooooooooooooooooooooooooooooooooooooo\n".getBytes)
      lineCount += 2
    }
    file4out.write("oooooooooooooooooooooooooooooooooooooooooong test\n".getBytes)
  }

  private def buildServerDir (timeStr: String) : File = {
    val serverDir = new File("server_test_dir_" + timeStr)
    serverDir.mkdir
    serverDir
  }

  def buildTestDir (timeStr: String) : File = {
    val serverDir = buildServerDir(timeStr)

    buildNestedDirs(serverDir)
    buildEmptyDirs(serverDir)
    buildEmptyFiles(serverDir)
    buildNonEmptyFiles(serverDir)

    serverDir
  }

  // TODO(jacob) clean up is still very messy...
  def cleanUp (serverDir: File) (clientDirs: List[File]) : Unit = {
    clientDirs.foreach(Utils.dirDelete(_))
    Utils.dirDelete(serverDir)

    sys.exit
  }

  def hashmapEquals (servMap: HashMap[List[String], Option[ServerData]])
                    (clntMap: HashMap[List[String], Option[ClientData]]) : Boolean = {

    // if keys do not match we can simply return false
    if (!servMap.keys.equals(clntMap.keys))
      false
    else {

      val matches = servMap.keys.map { key =>
        (servMap(key), clntMap(key)) match {
          case (None, None) => true
          case (Some(servData), Some(clntData)) =>
            servData.hash.zip(clntData.hash).forall(bs => bs._1 == bs._2)
          case _ => false
        }
      }

      matches.reduce((m1, m2) => m1 && m2)

    }
  }

  def main(args: Array[String]) : Unit = {

    val timeStr = (new Date).getTime.toString

    val serverDir = buildTestDir(timeStr)
    val server = new Server(serverDir)(9000)
    val serverThread = new Thread(server)
    serverThread.start

    val clientDirs = List(new File("client1_test_dir_" + timeStr),
                          new File("client2_test_dir_" + timeStr),
                          new File("client3_test_dir_" + timeStr))
    clientDirs.foreach(_.mkdir)
    val clients = clientDirs.map(new Client(_)("localhost")(9000))
    val clientThreads = clients.map(new Thread(_))

    while (!server.isOpen) Thread.sleep(100)

    clientThreads.foreach(_.start)

    while (!clients.map(_.isOpen).reduce((c1, c2) => c1 && c2)) Thread.sleep(100)

    print("Verifying Client-Server hashmap consistency... ")
    assert(clients.map(c => hashmapEquals(server.hashes)(c.hashes)).reduce((c1, c2) => c1 && c2),
           cleanUp(serverDir)(clientDirs))
    println("done")

    cleanUp(serverDir)(clientDirs)

  }

}
