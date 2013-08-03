default: 	Ack.scala Client.scala ClientHandler.scala FileListMessage.scala 		\
				 	FileMessage.scala FileRequest.scala MapData.scala Message.scala 		\
				 	RemovedMessage.scala Server.scala Utils.scala
	scalac -deprecation MapData.scala Message.scala Ack.scala FileListMessage.scala FileMessage.scala FileRequest.scala RemovedMessage.scala Utils.scala ClientHandler.scala Server.scala Client.scala