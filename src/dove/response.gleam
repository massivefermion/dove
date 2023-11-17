import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/bit_array
import gleam/http
import dove/error

pub fn decode(response: BitArray) {
  use #(status_line, rest) <- result.then(consume_till_crlf(response, <<>>))
  use status_line <- result.then(
    bit_array.to_string(status_line)
    |> result.replace_error(error.InvalidStatusLine),
  )

  use #(headers, rest) <- result.then(consume_till_double_crlf(rest, <<>>))
  use headers <- result.then(
    bit_array.to_string(headers)
    |> result.replace_error(error.InvalidHeader),
  )

  case decode_status_line(status_line) {
    Ok(StatusLine(#(_, status, _reason), _)) -> {
      use #(headers, _) <- result.then(decode_headers(headers, []))

      case
        list.key_find(headers, "content-length")
        |> result.map(int.parse)
        |> result.flatten
      {
        Ok(0) | Error(Nil) -> Ok(#(#(status, headers, option.None), rest))
        Ok(length) -> {
          use #(body, rest) <- result.then(
            consume_by_length(rest, length - 1, <<>>)
            |> result.replace_error(error.MoreNeeded),
          )

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

fn consume_till_crlf(
  data: BitArray,
  storage: BitArray,
) -> Result(#(BitArray, BitArray), error.Error) {
  case bit_array.byte_size(data) {
    0 -> Error(error.MoreNeeded)
    _ ->
      case data {
        <<"\r\n":utf8, rest:bits>> ->
          Ok(#(bit_array.append(storage, <<"\r\n":utf8>>), rest))
        <<ch:8, rest:bits>> ->
          consume_till_crlf(rest, bit_array.append(storage, <<ch>>))
      }
  }
}

fn consume_till_double_crlf(
  data: BitArray,
  storage: BitArray,
) -> Result(#(BitArray, BitArray), error.Error) {
  case bit_array.byte_size(data) {
    0 -> Error(error.MoreNeeded)
    _ ->
      case data {
        <<"\r\n\r\n":utf8, rest:bits>> ->
          Ok(#(bit_array.append(storage, <<"\r\n\r\n":utf8>>), rest))
        <<ch:8, rest:bits>> ->
          consume_till_double_crlf(rest, bit_array.append(storage, <<ch>>))
      }
  }
}

fn consume_by_length(
  data: BitArray,
  length: Int,
  storage: BitArray,
) -> Result(#(BitArray, BitArray), Nil) {
  case bit_array.byte_size(data) {
    0 -> Error(Nil)
    _ -> {
      let <<ch:8, rest:bits>> = data
      case bit_array.byte_size(storage) == length {
        True -> Ok(#(bit_array.append(storage, <<ch>>), rest))
        False ->
          consume_by_length(rest, length, bit_array.append(storage, <<ch>>))
      }
    }
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
