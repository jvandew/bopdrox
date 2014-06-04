scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean server

server: ServerMap.scala ClientHandler.scala Server.scala
	$(scalac) ServerMap.scala ClientHandler.scala Server.scala

clean:
	-rm *.class