scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean test

test: TestUtils.scala Test.scala
	$(scalac)  \
		TestUtils.scala  \
		Test.scala

clean:
	-rm *.class