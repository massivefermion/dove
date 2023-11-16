import gleam/list
import gleam/option
import gleam/string
import gleam/bit_array
import dove/error

pub fn encode(
  host: String,
  method: String,
  path: String,
  query: option.Option(String),
  headers: List(#(String, String)),
  body: option.Option(String),
) {
  use <- bail_if_invalid_header(headers)
  let headers = list.prepend(headers, #("host", host))

  let path = case query {
    option.None -> path
    option.Some(query) -> path <> "?" <> query
  }

  {
    request_line(method, path) <> {
      list.map(headers, encode_header)
      |> string.join("")
    } <> "\r\n" <> {
      case body {
        option.Some(body) -> body
        option.None -> ""
      }
    }
  }
  |> bit_array.from_string
  |> Ok
}

fn is_valid_header_name(name: String) {
  name
  |> string.to_utf_codepoints
  |> list.map(string.utf_codepoint_to_int)
  |> list.all(fn(code) {
    case code {
      _ if code <= 32 -> False
      _ if code == 34 -> False
      _ if code == 40 -> False
      _ if code == 41 -> False
      _ if code == 44 -> False
      _ if code == 47 -> False
      _ if code == 58 -> False
      _ if code == 59 -> False
      _ if code == 60 -> False
      _ if code == 61 -> False
      _ if code == 62 -> False
      _ if code == 63 -> False
      _ if code == 64 -> False
      _ if code == 91 -> False
      _ if code == 92 -> False
      _ if code == 93 -> False
      _ if code == 123 -> False
      _ if code == 125 -> False
      _ if code == 127 -> False
      _ -> True
    }
  })
}

fn is_valid_header_value(name: String) {
  name
  |> string.to_utf_codepoints
  |> list.map(string.utf_codepoint_to_int)
  |> list.all(fn(code) {
    case code {
      _ if code <= 31 -> False
      _ if code == 127 -> False
      _ -> True
    }
  })
}

fn bail_if_invalid_header(
  headers: List(#(String, String)),
  rest: fn() -> Result(a, error.Error),
) {
  case
    list.find_map(
      headers,
      fn(header) {
        case [is_valid_header_name(header.0), is_valid_header_value(header.1)] {
          [False, _] -> Ok(error.InvalidHeaderName(header))
          [_, False] -> Ok(error.InvalidHeaderValue(header))
          [True, True] -> Error(Nil)
        }
      },
    )
  {
    Ok(error) -> Error(error)
    Error(Nil) -> rest()
  }
}

fn request_line(method: String, path: String) {
  [method, path, "HTTP/1.1\r\n"]
  |> string.join(" ")
}

fn encode_header(header: #(String, String)) {
  [header.0, header.1]
  |> string.join(":")
  |> string.append("\r\n")
}
