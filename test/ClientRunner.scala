package bopdrox.test

import bopdrox.client.Client

// Helper class to run a Client
class ClientRunner (home: String) (servAddr: String) extends Runnable {

  def run: Unit = Client.main(Array(home, servAddr))

}
