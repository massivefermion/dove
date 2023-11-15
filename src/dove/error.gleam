import mug
import gleam/json

pub type Error {
  WrongPacketType
  MoreNeeded
  IsNotString
  InvalidHeader
  InvalidStatusLine
  TCPError(mug.Error)
  DecodeError(json.DecodeError)
  InvalidHeaderName(#(String, String))
  InvalidHeaderValue(#(String, String))
}
