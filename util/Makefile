scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean util

util: Utils.scala
	$(scalac) Utils.scala

clean:
	-rm *.class