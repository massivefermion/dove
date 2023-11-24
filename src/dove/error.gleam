import mug

pub type Error {
  MoreNeeded
  InvalidHeaders
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
