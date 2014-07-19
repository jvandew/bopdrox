.PHONY: clean server

server: ClientHandler.scala package.scala Server.scala ServerData.scala
	$(scalac)  \
		ServerData.scala     \
		package.scala        \
		ClientHandler.scala  \
		Server.scala

clean:
	-rm *.class