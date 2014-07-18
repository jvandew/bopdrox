.PHONY: clean client

client: Client.scala ClientData.scala ClientListener.scala ClientMessenger.scala  \
	      package.scala
	$(scalac)  \
		ClientData.scala       \
		package.scala          \
		ClientListener.scala   \
		ClientMessenger.scala  \
		Client.scala

clean:
	-rm *.class