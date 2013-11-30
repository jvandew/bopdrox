scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean client

client: ClientData.scala Client.scala ClientListener.scala
	$(scalac) ClientData.scala ClientListener.scala Client.scala

clean:
	-rm *.class