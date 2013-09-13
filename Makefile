.PHONY: all clean messages util server client
VPATH = messages:util:server:client

scalac = scalac -deprecation -optimise

all: messages util server client

infra: messages util

clientdeps: infra client

serverdeps: infra server

clean:
	-rm messages/*.class
	-rm util/*.class
	-rm server/*.class
	-rm client/*.class

messages: Ack.scala FileListMessage.scala FileMessage.scala FileRequest.scala \
					Message.scala RemovedMessage.scala
	$(scalac) $^

util: MapData.scala Utils.scala
	$(scalac) $^

server: ClientHandler.scala Server.scala
	$(scalac) $^

client: Client.scala ClientListener.scala
	$(scalac) $^
