package bopdrox.test

import bopdrox.client.Client
import bopdrox.server.Server
import bopdrox.util.Utils
import java.io.File
import java.util.Date
import scala.collection.mutable.HashMap

/* A simple test framework for Bopdrox. Creates several test directories and spawns
 * a Server and several Clients to test some basic operations. The results of each
 * test are printed to the console.
 * NOTE THAT THESE TESTS ARE NOT COMPREHENSIVE! In particular they do not test
 * performance over an actual network. */
object Test {

  /*  Folder hierarchy:
   *
   *  + test_dir
   *    + nested
   *      + nested
   *        + nested
   *          + empty_dir
   *          - empty
   *          - test
   *      - empty.test
   *      - testFile
   *    + empty_dir
   *    - empty.test
   *    - test
   *    - test2
   */

  val emptyDirs = List(
      List("empty_dir"),
      List("nested", "nested", "nested", "empty_dir"))

  val emptyFiles = List(
      List("empty.test"),
      List("nested", "empty.test"),
      List("nested", "nested", "nested", "empty"))

  val nonEmptyFiles = List(
      List("test"),
      List("test2"),
      List("nested", "testFile"),
      List("nested", "nested", "nested", "test"))

  var allFileSubpaths = List(emptyDirs, emptyFiles, nonEmptyFiles).flatten

  // TODO(jacob) figure out how to define an implicit ordering on List[String]
  def allFiles : List[String] = allFileSubpaths.map(Utils.joinPath(_)).sorted

  private def buildEmptyDirs (serverDir: File) : Unit =
    emptyDirs.foreach(TestUtils.createNewDir(serverDir, _))

  private def buildEmptyFiles (serverDir: File) : Unit =
    emptyFiles.foreach(TestUtils.createNewFile(serverDir, _))

  private def buildNonEmptyFiles (serverDir: File) : Unit = {
    TestUtils.createNewFile(serverDir, List("test"), "this is a test\n\n")
    TestUtils.createNewFile(serverDir, List("test2"), "this is a test\n\n")
    TestUtils.createNewFile(serverDir, List("nested", "testFile"), "\nI'm\n\talso\n a \n test!\n\n")

    val longText = new String(Array.tabulate(1000000)(n => 'a'))
    TestUtils.createNewFile(serverDir, List("nested", "nested", "nested", "test"), longText)
  }

  private def buildServerDir (timeStr: String) : File = {
    val serverDir = new File("server_test_dir_" + timeStr)
    serverDir.mkdir
    serverDir
  }

  def buildTestDir (timeStr: String) : File = {
    val serverDir = buildServerDir(timeStr)

    buildEmptyDirs(serverDir)
    buildEmptyFiles(serverDir)
    buildNonEmptyFiles(serverDir)

    serverDir
  }

  // shortcut to create a new file and add it for tracking
  def createAndTrack (home: File, subpath: List[String]) : Unit = {
    TestUtils.createNewFile(home, subpath)
    track(subpath)
  }

  // shortcut to delete and untrack a file/folder
  def deleteAndUntrack (home: File, subpath: List[String]) : Unit = {
    Utils.dirDelete(Utils.newFile(home, subpath))
    untrack(subpath)
  }

  // update the list of tracked files/folders to include a new subpath
  def track (subpath: List[String]) : Unit =
    allFileSubpaths = subpath :: allFileSubpaths.diff(List(subpath.dropRight(1)))

  // remove a subpath from the list of tracked files/folders
  def untrack (subpath: List[String]) : Unit = {
    val parent = subpath.dropRight(1)
    allFileSubpaths = allFileSubpaths.diff(List(subpath))
    if (allFileSubpaths.forall(!Utils.listStartsWith(_)(parent)))
      allFileSubpaths = parent::allFileSubpaths
  }

  def verifyFiles (serverDir: File, clientDirs: List[File]) (file1: File) (file2: File) : Unit = {
    print("Verifying the contents of " + file1.getCanonicalPath + " and " + file2.getCanonicalPath + "... ")

    assert(Utils.verifyBytes(Utils.readFile(file1))(Utils.readFile(file2)),
          {println("ERROR!") ; TestUtils.cleanUp(serverDir)(clientDirs)})

    println("done")
  }

  def verifyHashMaps (serverDir: File, clientDirs: List[File]) (server: Server) (clients: List[Client]) : Unit = {
    print("Verifying Client-Server hashmap consistency... ")

    assert(server.hashes.keys.toList.map(Utils.joinPath(_)).sorted equals allFiles,
          {println("ERROR!\nServer hashmap does not match reference") ; println(server.hashes.keys.toList.map(Utils.joinPath(_)).sorted); println(allFiles); TestUtils.cleanUp(serverDir)(clientDirs)})
    assert(clients.map(c => TestUtils.hashmapEquals(server.hashes)(c.hashes)).reduce((c1, c2) => c1 && c2),
          {println("ERROR!\nClient-Server mismatch") ; TestUtils.cleanUp(serverDir)(clientDirs)})

    println("done")
  }

  // currently runs with no arguments
  def main(args: Array[String]) : Unit = {

    // give each run a unique timestamp
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

    val checkFiles = verifyFiles(serverDir, clientDirs) _
    val checkMaps = verifyHashMaps(serverDir, clientDirs) _

    while (!server.isOpen) Thread.sleep(100)

    clientThreads.foreach(_.start)

    while (!clients.map(_.isOpen).reduce((c1, c2) => c1 && c2)) Thread.sleep(100)

    // verify hashmaps have been built correctly
    checkMaps(server)(clients)

    /*  Folder hierarchy:
     *
     *  + test_dir
     *    + nested
     *      + nested
     *        + nested
     *          + empty_dir
     *          - empty
     *          - test
     *      - empty.test
     *      - testFile
     *    + empty_dir
     *    - empty.test
     *    - test
     *    - test2
     */

    println("Testing single client file creation...")

    createAndTrack(clientDirs(0), List("new_file1"))
    createAndTrack(clientDirs(0), List("new_file2"))
    createAndTrack(clientDirs(0), List("new_file3"))
    createAndTrack(clientDirs(0), List("empty_dir", "test"))
    createAndTrack(clientDirs(0), List("nested", "nested", "nested", "test2"))
    createAndTrack(clientDirs(0), List("nested", "nested", "nested", "empty_dir", "test.test"))
    createAndTrack(clientDirs(0), List("nested", "nested", "new_file"))

    // triple filesystem scanning interval is a reasonable requirement
    // (2*500) potentially for each Client plus an extra 500ms for overhead
    Thread.sleep(1500)

    checkMaps(server)(clients)

    println("Testing single client file deletion...")

    deleteAndUntrack(clientDirs(1), List("new_file1"))
    deleteAndUntrack(clientDirs(1), List("new_file2"))
    deleteAndUntrack(clientDirs(1), List("new_file3"))
    deleteAndUntrack(clientDirs(1), List("empty_dir", "test"))
    deleteAndUntrack(clientDirs(1), List("nested", "nested", "nested", "test2"))
    deleteAndUntrack(clientDirs(1), List("nested", "nested", "nested", "empty_dir", "test.test"))
    deleteAndUntrack(clientDirs(1), List("nested", "nested", "new_file"))

    // triple filesystem scanning interval is a reasonable requirement
    // (2*500) potentially for each Client plus an extra 500ms for overhead
    Thread.sleep(1500)

    checkMaps(server)(clients)

    println("Testing multiple client file creation...")

    createAndTrack(clientDirs(0), List("new_file1"))
    createAndTrack(clientDirs(1), List("new_file2"))
    createAndTrack(clientDirs(2), List("new_file3"))
    createAndTrack(clientDirs(2), List("empty_dir", "test"))
    createAndTrack(clientDirs(0), List("nested", "nested", "nested", "test2"))
    createAndTrack(clientDirs(1), List("nested", "nested", "nested", "empty_dir", "test.test"))
    createAndTrack(clientDirs(1), List("nested", "nested", "new_file"))

    // triple filesystem scanning interval is a reasonable requirement
    // (2*500) potentially for each Client plus an extra 500ms for overhead
    Thread.sleep(1500)

    checkMaps(server)(clients)

    // println("Testing multiple client file deletion...")

    // deleteAndUntrack(clientDirs(1), List("new_file1"))
    // deleteAndUntrack(clientDirs(2), List("new_file2"))
    // deleteAndUntrack(clientDirs(0), List("new_file3"))
    // deleteAndUntrack(clientDirs(1), List("empty_dir", "test"))
    // deleteAndUntrack(clientDirs(2), List("nested", "nested", "nested", "test2"))
    // deleteAndUntrack(clientDirs(0), List("nested", "nested", "nested", "empty_dir", "test.test"))
    // deleteAndUntrack(clientDirs(0), List("nested", "nested", "new_file"))

    // // triple filesystem scanning interval is a reasonable requirement
    // // (2*500) potentially for each Client plus an extra 500ms for overhead
    // Thread.sleep(1500)

    // checkMaps(server)(clients)


    TestUtils.cleanUp(serverDir)(clientDirs)
  }

}
