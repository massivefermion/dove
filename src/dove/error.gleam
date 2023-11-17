import mug

pub type Error {
  MoreNeeded
  IsNotString
  InvalidHeader
  WrongPacketType
  InvalidStatusLine
  TCPError(mug.Error)
  UnableToSendRequest
  HttpsNotSupportedYet
  InvalidHeaderName(#(String, String))
  InvalidHeaderValue(#(String, String))
}
