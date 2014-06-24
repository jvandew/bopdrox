package bopdrox

import bopdrox.util.FSMap

package object server {

  type ServerMap = FSMap[ServerData, DirData, FileData]

}