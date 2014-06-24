scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean tests

tests: TestUtils.scala Test.scala
	$(scalac)  \
		TestUtils.scala  \
		Test.scala

clean:
	-rm *.class