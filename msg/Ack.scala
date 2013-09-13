package bopdrox.msg

// A message to be sent to acknowledge a received message when no other reply is needed
// TODO(jacob) this is likely not necessary
case object Ack extends Message