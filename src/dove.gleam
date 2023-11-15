import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/dynamic
import gleam/erlang/process
import dove/client
import dove/request

pub type Method {
  GET
  PUT
  POST
  HEAD
  PATCH
  TRACE
  DELETE
  CONNECT
  OPTIONS
}

pub type RequestOption(a) {
  Body(Body)
  Headers(List(#(String, String)))
  QueryParams(List(#(String, String)))
  ResponseDecoder(fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)))
}

pub type Body {
  JSON(String)
  PlainText(String)
}

pub fn request(
  conn: client.Connection(a),
  method: Method,
  path: String,
  options: List(RequestOption(a)),
  timeout: Int,
) {
  let assert Ok(method) = list.key_find(method_mapping, method)
  let query_params = get_query_params(options)
  let headers = get_headers(options)
  let body = get_body(options)
  let decoder = get_decoder(options)

  let #(body, headers) = case body {
    option.Some(JSON(body)) -> #(
      option.Some(body),
      list.append(
        headers,
        [
          #("content-type", "application/json"),
          #(
            "content-length",
            body
            |> string.length
            |> int.to_string,
          ),
        ],
      ),
    )

    option.Some(PlainText(body)) -> #(
      option.Some(body),
      list.append(
        headers,
        [
          #("content-type", "text/plain"),
          #(
            "content-length",
            body
            |> string.length
            |> int.to_string,
          ),
        ],
      ),
    )

    option.None -> #(option.None, headers)
  }

  use request <- result.then(request.encode(
    conn.host,
    method,
    path,
    query_params,
    headers,
    body,
  ))

  let subject = process.new_subject()
  process.send(conn.client, client.Request(request, subject, decoder, timeout))
  Ok(subject)
}

const method_mapping = [
  #(GET, "GET"),
  #(PUT, "PUT"),
  #(POST, "POST"),
  #(HEAD, "HEAD"),
  #(PATCH, "PATCH"),
  #(TRACE, "TRACE"),
  #(DELETE, "DELETE"),
  #(CONNECT, "CONNECT"),
  #(OPTIONS, "OPTIONS"),
]

fn get_query_params(options: List(RequestOption(a))) {
  case
    list.find_map(
      options,
      fn(opt) {
        case opt {
          QueryParams(params) -> Ok(params)
          _ -> Error(Nil)
        }
      },
    )
  {
    Ok(params) -> params
    Error(Nil) -> []
  }
}

fn get_decoder(options: List(RequestOption(a))) {
  case
    list.find_map(
      options,
      fn(opt) {
        case opt {
          ResponseDecoder(decoder) -> Ok(decoder)
          _ -> Error(Nil)
        }
      },
    )
  {
    Ok(decoder) -> option.Some(decoder)
    Error(Nil) -> option.None
  }
}

fn get_headers(options: List(RequestOption(a))) {
  case
    list.find_map(
      options,
      fn(opt) {
        case opt {
          Headers(headers) -> Ok(headers)
          _ -> Error(Nil)
        }
      },
    )
  {
    Ok(headers) -> headers
    Error(Nil) -> []
  }
}

fn get_body(options: List(RequestOption(a))) {
  case
    list.find_map(
      options,
      fn(opt) {
        case opt {
          Body(body) -> Ok(body)
          _ -> Error(Nil)
        }
      },
    )
  {
    Ok(body) -> option.Some(body)
    Error(Nil) -> option.None
  }
}
