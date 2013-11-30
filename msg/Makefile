scalac = fsc -deprecation -optimise -classpath ../.. -d ../..

.PHONY: clean msg

msg: Ack.scala FileListMessage.scala FileMessage.scala FileRequest.scala \
					Message.scala RemovedMessage.scala
	$(scalac) Message.scala Ack.scala FileListMessage.scala FileMessage.scala FileRequest.scala RemovedMessage.scala

clean:
	-rm *.class