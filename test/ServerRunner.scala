package bopdrox.test

import bopdrox.server.Server

// Helper class to run a Server
class ServerRunner (home: String) (port: Int) extends Runnable {

  def run: Unit = Server.main(Array(home, port.toString))

}
