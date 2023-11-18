import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/bit_array
import gleam/http
import dove/error

pub fn decode(response: BitArray) {
  use #(status_line, rest) <- result.then(consume_till_crlf(
    response,
    <<>>,
    True,
  ))
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

      use #(response, rest) <- result.then(case
        #(
          list.key_find(headers, "transfer-encoding"),
          list.key_find(headers, "content-length")
          |> result.map(int.parse)
          |> result.flatten,
        )
      {
        #(Ok("chunked"), _) -> {
          use #(body, rest) <- result.then(gather_chunks(rest, <<>>))
          Ok(#(#(status, headers, option.Some(body)), rest))
        }
        #(_, Ok(length)) if length > 0 -> {
          use #(body, rest) <- result.then(consume_by_length(rest, length, <<>>))
          Ok(#(#(status, headers, option.Some(body)), rest))
        }
        _ -> Ok(#(#(status, headers, option.None), rest))
      })

      case response.2 {
        option.Some(body) ->
          case list.key_find(headers, "content-encoding") {
            Ok("gzip") -> {
              use decompressed <- result.then(
                gunzip(body)
                |> result.map_error(fn(error) {
                  error.DecompressionError(error)
                }),
              )
              Ok(#(#(status, headers, option.Some(decompressed)), rest))
            }

            Ok("deflate") -> {
              use decompressed <- result.then(
                inflate(body)
                |> result.map_error(fn(error) {
                  error.DecompressionError(error)
                }),
              )

              Ok(#(#(status, headers, option.Some(decompressed)), rest))
            }

            _ -> {
              use body <- result.then(
                bit_array.to_string(body)
                |> result.replace_error(error.IsNotString),
              )
              Ok(#(#(status, headers, option.Some(body)), rest))
            }
          }
        option.None -> Ok(#(#(status, headers, option.None), rest))
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
  include_crlf: Bool,
) -> Result(#(BitArray, BitArray), error.Error) {
  case bit_array.byte_size(data) {
    0 -> Error(error.MoreNeeded)
    _ ->
      case data {
        <<"\r\n":utf8, rest:bits>> if include_crlf ->
          Ok(#(bit_array.append(storage, <<"\r\n":utf8>>), rest))
        <<"\r\n":utf8, rest:bits>> -> Ok(#(storage, rest))
        <<ch:8, rest:bits>> ->
          consume_till_crlf(
            rest,
            bit_array.append(storage, <<ch>>),
            include_crlf,
          )
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
) -> Result(#(BitArray, BitArray), error.Error) {
  case bit_array.byte_size(data) {
    0 -> Error(error.MoreNeeded)
    _ -> {
      let <<ch:8, rest:bits>> = data
      let storage = bit_array.append(storage, <<ch>>)
      case bit_array.byte_size(storage) == length {
        True -> Ok(#(storage, rest))
        False -> consume_by_length(rest, length, storage)
      }
    }
  }
}

fn gather_chunks(data: BitArray, storage: BitArray) {
  case bit_array.byte_size(data) {
    0 -> Error(error.MoreNeeded)
    _ -> {
      use #(length, rest) <- result.then(consume_till_crlf(data, <<>>, False))

      use length <- result.then(
        bit_array.to_string(length)
        |> result.replace_error(error.InvalidChunkedResponse),
      )

      case int.base_parse(length, 16) {
        Ok(0) -> {
          let <<"\r\n":utf8, rest:bits>> = rest
          Ok(#(storage, rest))
        }
        Ok(length) -> {
          case consume_by_length(rest, length, <<>>) {
            Ok(#(chunk, rest)) -> {
              let <<"\r\n":utf8, rest:bits>> = rest
              gather_chunks(rest, bit_array.append(storage, chunk))
            }
            Error(error) -> Error(error)
          }
        }
        Error(Nil) -> Error(error.InvalidChunkedResponse)
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

@external(erlang, "dove_ffi", "gunzip")
fn gunzip(compressed: BitArray) -> Result(String, String)

@external(erlang, "dove_ffi", "inflate")
fn inflate(compressed: BitArray) -> Result(String, String)
