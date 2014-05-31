scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean util

util: FSObject.scala Message.scala package.scala Utils.scala
	$(scalac)  \
		package.scala      \
		FSObject.scala  \
		Message.scala   \
		Utils.scala

clean:
	-rm *.class
