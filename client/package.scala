package bopdrox

import bopdrox.util.FSMap

package object client {

  type ClientMap = FSMap[ClientData, DirData, FileData]

}