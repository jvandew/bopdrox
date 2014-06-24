package bopdrox.test

import bopdrox.client.{Client, ClientMap, DirData, FileData}
import bopdrox.server.Server
import bopdrox.util.{FSDirectory, FSFile, FSObject, Utils}
import java.io.{File, FileOutputStream}
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
    FSDirectory(List("empty_dir")),
    FSDirectory(List("nested", "nested", "nested", "empty_dir")))

  val emptyFiles = List(
    FSFile(List("empty.test")),
    FSFile(List("nested", "empty.test")),
    FSFile(List("nested", "nested", "nested", "empty")))

  val nonEmptyFiles = List(
    FSFile(List("test")),
    FSFile(List("test2")),
    FSFile(List("nested", "testFile")),
    FSFile(List("nested", "nested", "nested", "test")))

  val parentDirs = List(
    FSDirectory(List("nested")),
    FSDirectory(List("nested", "nested")),
    FSDirectory(List("nested", "nested", "nested")))

  // TODO(jacob) for now we use a ClientMap and don't worry about the chain
  val refMap = new ClientMap


  private def buildEmptyDirs (serverDir: File) : Unit =
    emptyDirs.foreach(createDirAndTrack(serverDir, _))


  private def buildEmptyFiles (serverDir: File) : Unit =
    emptyFiles.foreach(createAndTrack(serverDir, _))


  private def buildNonEmptyFiles (serverDir: File) : Unit = {
    createWriteAndTrack(serverDir, nonEmptyFiles(0))(List("this is a test\n\n"))
    createWriteAndTrack(serverDir, nonEmptyFiles(1))(List("this is a test\n\n"))
    createWriteAndTrack(serverDir, nonEmptyFiles(2))(List("\nI'm\n\talso\n a \n test!\n\n"))


    val longText = new String(Array.tabulate(1000000)(n => 'a'))
    TestUtils.createNewFile(serverDir, nonEmptyFiles(3), longText)
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
  def createAndTrack (home: File, fsFile: FSFile) : Unit = {
    val file = Utils.newFile(home, fsFile)
    refMap(fsFile) = FileData(file.lastModified, Utils.hashFile(file))
  }


  // shortcut to create a new directory and add it for tracking
  def createDirAndTrack (home: File, fsDir: FSDirectory) : Unit = {
    val dir = Utils.newDir(home, fsDir)
    refMap(fsDir) = DirData(dir.lastModified)
  }


  // shortcut to create a new file and erase and write strings to it multiple times
  def createWriteAndTrack (home: File, fsFile: FSFile) (strings: List[String]) : Unit = {
    val file = Utils.newFile(home, fsFile)
    file.createNewFile
    strings.foreach(TestUtils.writeFileString(file))
  }


  // shortcut to delete and untrack a directory
  def deleteAndUntrack (home: File, fsDir: FSDirectory) : Unit = {
    Utils.dirDelete(Utils.newDir(home, fsDir))
    refMap.remove(fsDir)
  }


  // shortcut to delete and untrack a file
  def deleteAndUntrack (home: File, fsFile: FSFile) : Unit = {
    Utils.dirDelete(Utils.newFile(home, fsFile))
    refMap.remove(fsFile)
  }


  // erase a file and then append multiple strings to it; assumes file exists
  def fileMultiwrite (home: File, fsFile: FSFile) (strings: List[String]) : Unit = {
    val out = new FileOutputStream(Utils.newFile(home, fsFile))
    strings.foreach(s => out.write(s.getBytes))
    out.close
  }


  def verifyFile (serverDir: File, clientDirs: List[File]) (fsFile: FSFile) : Boolean = {

    print("Verifying the contents of " + Utils.joinPath(fsFile.path) + "... ")
    val serverHash = Utils.hashFile(Utils.newFile(serverDir, fsFile))
    val clientHashes = clientDirs.map(home => Utils.hashFile(Utils.newFile(home, fsFile)))

    if (!clientHashes.forall(Utils.verifyHash(serverHash))) {
      println("FAILED!:\n" + TestUtils.hexHash(serverHash))
      clientHashes.foreach(hash => println(TestUtils.hexHash(hash)))

      println("done")
      false
    }
    else {
      println("done")
      true
    }
  }


  def verifyFiles (serverDir: File, clientDirs: List[File]) (server: Server) : Unit = {

    assert(server.hashes.keySetFile.forall(verifyFile(serverDir, clientDirs)), {

      println("ERROR!\nFile consistency check(s) failed. See printouts")
      TestUtils.cleanUp(serverDir, clientDirs)
    })
  }


  def verifyMaps (serverDir: File, clientDirs: List[File])
                 (server: Server, clients: List[Client])
      : Unit = {

    assert(clients.forall(server.hashes == _.hashes), {

      println("ERROR!\nClient-Server mismatch")
      TestUtils.printMap("Server Map:")(server.hashes)
      server.hashes.keySetFile.foreach(verifyFile(serverDir, clientDirs))
      clients.foreach(c => TestUtils.printMap("Client Map:")(c.hashes))

      TestUtils.cleanUp(serverDir, clientDirs)
    })
  }


  def verifySync (serverDir: File, clientDirs: List[File])
                 (server: Server, clients: List[Client])
      : Unit = {

    verifyMaps(serverDir, clientDirs)(server, clients)
    verifyFiles(serverDir, clientDirs)(server)
  }


  // currently runs with no arguments
  // TODO(jacob) test replacing file with folder and vice versa
  def main(args: Array[String]) : Unit = {

    // give each run a unique timestamp
    val timeStr = (new Date).getTime.toString

    // quadruple filesystem scanning interval is a reasonable requirement
    // (3*500) potentially for each Client plus an extra 500ms for overhead
    val sleep = 2000

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

    val checkSync = verifySync(serverDir, clientDirs) _

    while (!server.isOpen) Thread.sleep(100)

    clientThreads.foreach(_.start)
    while (!clients.forall(_.isOpen)) Thread.sleep(100)

    // verify hashmaps have been built correctly
    checkSync(server, clients)

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

    val testFiles = List(
      FSFile(List("new_file1")),
      FSFile(List("new_file2")),
      FSFile(List("new_file3")),
      FSFile(List("empty_dir", "test")),
      FSFile(List("nested", "nested", "nested", "test2")),
      FSFile(List("nested", "nested", "nested", "empty_dir", "test.test")),
      FSFile(List("nested", "nested", "new_file")))

    val testStrings = List(
      List("", "", ""),
      List("two\n", "things\n"),
      List("two\n", "things\n"),
      List("l", "o", "t", "s", " ", "o", "f", " ", "t", "h", "i", "n", "g", "s"),
      List("\n\t\n\t\n", "stuff", " hi"),
      List("hi", " ", "there"),
      List("hit", " ", "here"))

    println("Testing single client file creation...")
    testFiles.foreach(fsFile => createAndTrack(clientDirs(0), fsFile))
    Thread.sleep(sleep)

    checkSync(server, clients)

    println("Testing single client file deletion...")
    testFiles.foreach(fsFile => deleteAndUntrack(clientDirs(1), fsFile))
    Thread.sleep(sleep)

    checkSync(server, clients)

    println("Testing multiple client file creation...")

    createAndTrack(clientDirs(0), testFiles(0))
    createAndTrack(clientDirs(1), testFiles(1))
    createAndTrack(clientDirs(2), testFiles(2))
    createAndTrack(clientDirs(2), testFiles(3))
    createAndTrack(clientDirs(0), testFiles(4))
    createAndTrack(clientDirs(1), testFiles(5))
    createAndTrack(clientDirs(1), testFiles(6))

    Thread.sleep(sleep)
    checkSync(server, clients)

    println("Testing multiple client file deletion...")

    deleteAndUntrack(clientDirs(1), testFiles(0))
    deleteAndUntrack(clientDirs(2), testFiles(1))
    deleteAndUntrack(clientDirs(0), testFiles(2))
    deleteAndUntrack(clientDirs(1), testFiles(3))
    deleteAndUntrack(clientDirs(2), testFiles(4))
    deleteAndUntrack(clientDirs(0), testFiles(5))
    deleteAndUntrack(clientDirs(0), testFiles(6))

    Thread.sleep(sleep)
    checkSync(server, clients)

    println("Testing single client file creation and rewriting...")

    for (i <- 0 until testFiles.length) {
      createWriteAndTrack(clientDirs(0), testFiles(i))(testStrings(i))
    }

    Thread.sleep(sleep)
    checkSync(server, clients)

    println("Testing single client file appending...")

    for (i <- 0 until testFiles.length) {
      fileMultiwrite(clientDirs(0), testFiles(i))(testStrings(i))
    }

    Thread.sleep(sleep)
    checkSync(server, clients)

    println("Testing multiple client file deletion...")

    deleteAndUntrack(clientDirs(0), testFiles(0))
    deleteAndUntrack(clientDirs(1), testFiles(1))
    deleteAndUntrack(clientDirs(2), testFiles(2))
    deleteAndUntrack(clientDirs(0), testFiles(3))
    deleteAndUntrack(clientDirs(1), testFiles(4))
    deleteAndUntrack(clientDirs(2), testFiles(5))
    deleteAndUntrack(clientDirs(0), testFiles(6))

    Thread.sleep(sleep)
    checkSync(server, clients)

    println("Testing multiple client file creation and rewriting...")

    createWriteAndTrack(clientDirs(2), testFiles(0))(testStrings(0))
    createWriteAndTrack(clientDirs(1), testFiles(1))(testStrings(1))
    createWriteAndTrack(clientDirs(0), testFiles(2))(testStrings(2))
    createWriteAndTrack(clientDirs(1), testFiles(3))(testStrings(3))
    createWriteAndTrack(clientDirs(0), testFiles(4))(testStrings(4))
    createWriteAndTrack(clientDirs(1), testFiles(5))(testStrings(5))
    createWriteAndTrack(clientDirs(2), testFiles(6))(testStrings(6))

    Thread.sleep(sleep)
    checkSync(server, clients)

    println("Testing multiple client file appending...")

    fileMultiwrite(clientDirs(0), testFiles(0))(testStrings(0))
    fileMultiwrite(clientDirs(2), testFiles(1))(testStrings(1))
    fileMultiwrite(clientDirs(1), testFiles(2))(List("ttwo\n", "things\n"))
    fileMultiwrite(clientDirs(0), testFiles(2))(List("moar\n", "stuff\n"))
    fileMultiwrite(clientDirs(2), testFiles(3))(List("tl", "o", "t", "s", " ", "o", "f", " ", "t", "h", "i", "n", "g", "s"))
    fileMultiwrite(clientDirs(0), testFiles(3))(List("z", "z", "z", "z", "z", "z", "z", "z", "z", "z", "z", "z", "z", "z"))
    fileMultiwrite(clientDirs(1), testFiles(4))(testStrings(4))
    fileMultiwrite(clientDirs(2), testFiles(5))(testStrings(5))
    fileMultiwrite(clientDirs(0), testFiles(6))(testStrings(6))

    Thread.sleep(sleep)
    checkSync(server, clients)

    println("All tests pass (for what that's worth). Cleaning up...")
    TestUtils.cleanUp(serverDir, clientDirs)
  }

}
