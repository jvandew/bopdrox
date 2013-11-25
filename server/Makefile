scalac = scalac -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean server

server: ServerData.scala ClientHandler.scala Server.scala
	$(scalac) ServerData.scala ClientHandler.scala Server.scala

clean:
	-rm *.class