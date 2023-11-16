import gleam/json
import mug

pub type Error {
  WrongPacketType
  MoreNeeded
  IsNotString
  InvalidHeader
  InvalidStatusLine
  TCPError(mug.Error)
  UnableToSendRequest
  DecodeError(json.DecodeError)
  InvalidHeaderName(#(String, String))
  InvalidHeaderValue(#(String, String))
}
