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
  InvalidChunkedResponse
  DecompressionError(String)
  InvalidHeaderName(#(String, String))
  InvalidHeaderValue(#(String, String))
}
