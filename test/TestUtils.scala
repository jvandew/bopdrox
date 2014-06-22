package bopdrox.test

import bopdrox.util.{FileHash, FSData, FSDirData, FSFile, FSFileData, FSMap,
                     FSObject, FSPath, Utils}
import java.io.File
import java.util.Scanner
import scala.collection.mutable.HashMap

// This object stores some helpful functions used in the Test object

object TestUtils {

  // insert a "breakpoint" in the code
  def breakpoint : Unit = {
    val scanner = new Scanner(System.in)
    println("Breakpoint reached. Press enter to continue.")
    scanner.nextLine
  }


  // TODO(jacob) clean up is still very messy...
  def cleanUp (serverDir: File) (clientDirs: List[File]) : Unit = {
    clientDirs.foreach(Utils.dirDelete(_))
    Utils.dirDelete(serverDir)

    sys.exit
  }


  def createNewFile (home: File, fsFile: FSFile, text: String) : Unit = {
    val file = Utils.newFile(home, fsFile)
    file.createNewFile
    writeFileString(file)(text)
  }


  // convert a hash in byte array format to a printable hex string
  def hexHash (hash: FileHash) : String = {
    val hexArr = hash.map { b =>
      val hex = b.toInt.toHexString
      ("00" + hex).substring(hex.length)
    }
    hexArr.reduce((h1, h2) => h1 + h2)
  }


  def printMap (desc: String) (map: FSMap[FSData, FSDirData, FSFileData]) : Unit = {

    println(desc)

    map.foreach { kv: (FSObject, FSData) =>
      kv match {
        case (fsObj, dirData: FSDirData) =>
          println(Utils.joinPath(fsObj.path) + " : Directory")
        case (fsObj, fileData: FSFileData) =>
          println(Utils.joinPath(fsObj.path) + " : " + hexHash(fileData.hash).substring(0, 16))
      }
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
