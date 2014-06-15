scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean client

client: Client.scala ClientListener.scala ClientMap.scala
	$(scalac) ClientMap.scala ClientListener.scala Client.scala

clean:
	-rm *.class