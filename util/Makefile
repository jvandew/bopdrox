scalac = scalac -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean util

util: MapData.scala Utils.scala
	$(scalac) MapData.scala Utils.scala

clean:
	-rm *.class