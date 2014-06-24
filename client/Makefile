.PHONY: clean client

client: Client.scala ClientData.scala ClientListener.scala package.scala
	$(scalac)  \
		ClientData.scala      \
		package.scala         \
		ClientListener.scala  \
		Client.scala

clean:
	-rm *.class