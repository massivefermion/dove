import gleam/list
import gleam/result
import gleam/bit_array
import dove/error

pub fn decode(response: BitArray) {
  use response <- result.then(
    bit_array.to_string(response)
    |> result.replace_error(error.IsNotString),
  )

  case decode_status_line(response) {
    Ok(StatusLine(#(_, status, _reason), rest)) -> {
      use #(headers, rest) <- result.then(decode_headers(rest, []))
      Ok(#(status, headers, rest))
    }
    Ok(More) -> Error(error.MoreNeeded)
    Error(Nil) -> Error(error.InvalidStatusLine)
    _ -> Error(error.WrongPacketType)
  }
}

fn decode_headers(binary: String, storage: List(#(String, String))) {
  use result <- result.then(
    decode_header(binary)
    |> result.replace_error(error.InvalidHeader),
  )

  case result {
    Eoh(rest) -> Ok(#(storage, rest))
    Header(header, rest) -> decode_headers(rest, list.append(storage, [header]))
    More -> Error(error.MoreNeeded)
    StatusLine(..) -> Error(error.WrongPacketType)
  }
}

type DecodeResult {
  More
  Eoh(String)
  Header(#(String, String), String)
  StatusLine(#(#(Int, Int), Int, String), String)
}

@external(erlang, "dove_ffi", "decode_status_line")
fn decode_status_line(binary: String) -> Result(DecodeResult, Nil)

@external(erlang, "dove_ffi", "decode_header")
fn decode_header(binary: String) -> Result(DecodeResult, Nil)
