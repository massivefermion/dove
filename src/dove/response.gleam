import gleam/json

pub type Body(a) {
  JSONDecoded(a)
  PlainText(String)
  OctetStream(BitArray)
  HeadersSayJSON(String)
  InvalidOrUnexpectedJSON(BitArray, json.DecodeError)
}
