.PHONY: clean server

server: CHMessenger.scala ClientHandler.scala package.scala Server.scala ServerData.scala
	$(scalac)  \
		ServerData.scala     \
		package.scala        \
		CHMessenger.scala    \
		ClientHandler.scala  \
		Server.scala

clean:
	-rm *.class