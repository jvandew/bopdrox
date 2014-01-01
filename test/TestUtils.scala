package bopdrox.test

import bopdrox.util.Utils
import bopdrox.client.ClientData
import bopdrox.server.ServerData
import java.io.File
import scala.collection.mutable.HashMap

// This object stores some helpful functions used in the Test object

object TestUtils {

  // TODO(jacob) clean up is still very messy...
  def cleanUp (serverDir: File) (clientDirs: List[File]) : Unit = {
    clientDirs.foreach(Utils.dirDelete(_))
    Utils.dirDelete(serverDir)

    sys.exit
  }

  def createNewDir (home: File, subpath: List[String]) : Unit =
    Utils.newFile(home, subpath).mkdirs

  def createNewFile (home: File, subpath: List[String]) : Unit =
    createNewFile(home, subpath, "")

  // TODO(jacob) currying this method seems to cause problems with "ambiguity" figure out why
  def createNewFile (home: File, subpath: List[String], text: String) : Unit = {
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

  def readFileString (home: File, subpath: List[String]) : String =
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