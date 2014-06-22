scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean util

util: FSMap.scala FSObject.scala Message.scala package.scala Utils.scala
	$(scalac)  \
		package.scala   \
		FSObject.scala  \
		Message.scala   \
		Utils.scala     \
		FSMap.scala

clean:
	-rm *.class
