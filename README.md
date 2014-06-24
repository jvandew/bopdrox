bopdrox
=======

A basic personal Dropbox clone written in Scala.

This project is useable, but only at the user's own risk. While it should be safe, storing important files is not advised (currently network traffic is unencrypted so this is a bad idea anyway).

It is also worth noting that this was intended to be a personal (i.e. single user) system. Multi-user usage SHOULD be safe in all instances, with conflict resolution handled on a first-come, first-serve basis and conflicted copies created when conflicts are detected. Data-loss with this method has never been observed, but tread with caution as your mileage may vary.

Network hopping, for instance moving a laptop from one network to another, is still being tested and will likely produce some crashes.
