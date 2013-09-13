scalac = scalac -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean server

server: ClientHandler.scala Server.scala
	$(scalac) ClientHandler.scala Server.scala

clean:
	-rm *.class