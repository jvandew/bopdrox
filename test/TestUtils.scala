package bopdrox.test

import bopdrox.util.{FileHash, FSPath, Utils}
import bopdrox.client.ClientData
import bopdrox.server.ServerData
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

  def createNewDir (home: File, subpath: FSPath) : Unit =
    Utils.newFile(home, subpath).mkdirs

  def createNewFile (home: File, subpath: FSPath) : Unit =
    createNewFile(home, subpath, "")

  def createNewFile (home: File, subpath: FSPath, text: String) : Unit = {
    val file = Utils.newFile(home, subpath)
    file.createNewFile
    writeFileString(file)(text)
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
            Utils.verifyBytes(servData.hash)(clntData.hash)
          case _ => false
        }
      }

      matches.reduce((m1, m2) => m1 && m2)

    }
  }

  // convert a hash in byte array format to a printable hex string
  def hexHash (hash: FileHash) : String = {
    val hexArr = hash.map { b =>
      val hex = b.toInt.toHexString
      ("00" + hex).substring(hex.length)
    }
    hexArr.reduce((h1, h2) => h1 + h2)
  }

  // TODO(jacob) create an overarching MapData type so duplicating this function is unnecessary
  def printClientMap (map: HashMap[FSPath, Option[ClientData]]) : Unit = {
    println("Begin Client map:")
    map.keys.foreach { key =>
      map(key) match {
        case None => println(Utils.joinPath(key) + ": None")
        case Some(data) => println(Utils.joinPath(key) + ": " + hexHash(data.hash).substring(0, 16))
      }
    }

    print("\n")
  }

  // TODO(jacob) create an overarching MapData type so duplicating this function is unnecessary
  def printServerMap (map: HashMap[FSPath, Option[ServerData]]) : Unit = {
    println("Begin Server map:")
    map.keys.foreach { key =>
      map(key) match {
        case None => println(Utils.joinPath(key) + ": None")
        case Some(data) => println(Utils.joinPath(key) + ": " + hexHash(data.hash).substring(0, 16))
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