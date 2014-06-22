scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean client

client: Client.scala ClientListener.scala package.scala
	$(scalac) package.scala ClientListener.scala Client.scala

clean:
	-rm *.class