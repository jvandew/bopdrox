scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean server

server: ClientHandler.scala package.scala Server.scala
	$(scalac)  \
		package.scala        \
		ClientHandler.scala  \
		Server.scala

clean:
	-rm *.class