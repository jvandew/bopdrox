scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean test

test: Test.scala
	$(scalac) Test.scala

clean:
	-rm *.class