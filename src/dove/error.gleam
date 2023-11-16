import gleam/json
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
  DecodeError(json.DecodeError)
  InvalidHeaderName(#(String, String))
  InvalidHeaderValue(#(String, String))
}
