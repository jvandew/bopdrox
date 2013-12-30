scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean test

test: ServerRunner.scala ClientRunner.scala Test.scala
	$(scalac) ServerRunner.scala ClientRunner.scala Test.scala

clean:
	-rm *.class