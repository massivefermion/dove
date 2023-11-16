import gleam/int
import gleam/list
import gleam/option
import gleam/string
import gleam/result
import gleam/http
import dove/error

pub fn decode(response: String) {
  case decode_status_line(response) {
    Ok(StatusLine(#(_, status, _reason), rest)) -> {
      use #(headers, rest) <- result.then(decode_headers(rest, []))
      case
        list.key_find(headers, "content-length")
        |> result.map(int.parse)
        |> result.flatten
      {
        Ok(0) | Error(Nil) -> Ok(#(#(status, headers, option.None), rest))
        Ok(length) -> {
          let body = string.slice(rest, 0, length)
          let rest = string.slice(rest, length, string.length(rest))
          Ok(#(#(status, headers, option.Some(body)), rest))
        }
      }
    }

    Ok(More) -> Error(error.MoreNeeded)
    Error(Nil) -> Error(error.InvalidStatusLine)
    _ -> Error(error.WrongPacketType)
  }
}

fn decode_headers(binary: String, storage: List(http.Header)) {
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
  Header(http.Header, String)
  StatusLine(#(#(Int, Int), Int, String), String)
}

@external(erlang, "dove_ffi", "decode_status_line")
fn decode_status_line(binary: String) -> Result(DecodeResult, Nil)

@external(erlang, "dove_ffi", "decode_header")
fn decode_header(binary: String) -> Result(DecodeResult, Nil)
