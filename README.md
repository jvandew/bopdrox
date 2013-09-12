bopdrox
=======

A basic personal Dropbox clone written in Scala.

This project is useable, but only at the user's own risk. While it should be safe, storing important files is not advised (currently network traffic is unencrypted so this is a bad idea anyway).

It is also worth noting that this was intended to be a personal (i.e. single user) system. Multi-user usage is safe in most instances with the exception of file updates/deletion which could cause strange things to happen. There is not currently any conflict resolution in place.