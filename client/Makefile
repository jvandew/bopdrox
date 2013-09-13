scalac = scalac -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean client

client: Client.scala ClientListener.scala
	$(scalac) ClientListener.scala Client.scala

clean:
	-rm *.class