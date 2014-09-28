package bopdrox.tests

import bopdrox.util.{FileHash, FSData, FSDirData, FSDirectory, FSFile, FSFileData,
                     FSMap, FSObject, FSPath, Utils}
import java.io.File
import java.util.Scanner
import scala.collection.mutable.HashMap

// This object stores some helpful functions used in the Test object

object TestUtils {

  // insert a "breakpoint" in the code
  def breakpoint () : Unit = {
    val scanner = new Scanner(System.in)
    println("Breakpoint reached. Press enter to continue.")
    scanner.nextLine
  }


  // TODO(jacob) clean up is still very messy...
  def cleanUp (serverDir: File, clientDirs: List[File]) : Unit = {

    breakpoint

    clientDirs.foreach(Utils.dirDelete(_))
    Utils.dirDelete(serverDir)

    sys.exit
  }


  def createNewFile (home: File, fsFile: FSFile, text: String) : Unit = {
    val file = Utils.newFile(home, fsFile)
    file.createNewFile
    writeFileString(file)(text)
  }


  def printMap[Data <: FSData, DirData <: Data with FSDirData, FileData <: Data with FSFileData] (desc: String) (map: FSMap[Data, DirData, FileData]) : Unit = {

    println(desc)

    map.foreach { kv: (FSDirectory, DirData) =>
      val path = Utils.joinPath(kv._1.path)
      println(path + " : Directory")
    }
    { kv: (FSFile, FileData) =>
      val path = Utils.joinPath(kv._1.path)
      val hash = Utils.hexHash(kv._2.hash).substring(0, 16)
      println(path + " : " + hash)
    }

    print("\n")
  }


  def readFileString (home: File, subpath: FSPath) : String =
    readFileString(home, Utils.joinPath(subpath))


  def readFileString (home: File, subpath: String) : String =
    readFileString(new File(home, subpath))


  // Note this function can produce unpredictable results when given
  // a non-default character encoding
  def readFileString (file: File) : String = new String(Utils.readFile(file))


  // write a String to a file. May do weird things with non-default encodings
  def writeFileString (file: File) (string: String) : Unit =
    Utils.writeFile(file)(string.getBytes)

}
