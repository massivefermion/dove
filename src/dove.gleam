import gleam/int
import gleam/bool
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/dynamic
import gleam/bit_array
import gleam/json
import gleam/erlang
import gleam/erlang/process
import gleam/http
import gleam/http/request as gleam_http_request
import gleam/http/response as gleam_http_response
import dove/tcp
import dove/error
import dove/request
import dove/response
import mug

pub type RequestOption(a) {
  JSONDecoder(fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)))
}

pub type RequestBody {
  JSON(String)
  EmptyRequestBody
  PlainText(String)
}

pub type ResponseBody(a) {
  Raw(String)
  JSONDecoded(a)
  EmptyResponseBody
  InvalidOrUnexpectedJSON(String, json.DecodeError)
}

pub opaque type Connection(a) {
  Connection(
    host: String,
    socket: mug.Socket,
    buffer: String,
    requests: List(
      #(
        erlang.Reference,
        option.Option(
          fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)),
        ),
      ),
    ),
    responses: List(
      #(
        erlang.Reference,
        Result(gleam_http_response.Response(ResponseBody(a)), error.Error),
      ),
    ),
    default_timeout: Int,
  )
}

pub fn connect(host: String, port: Int, timeout: Int) {
  use socket <- result.then(tcp.connect(host, port, timeout))
  Ok(Connection(host <> ":" <> int.to_string(port), socket, "", [], [], timeout))
}

pub fn request(
  conn: Connection(a),
  request: gleam_http_request.Request(RequestBody),
  options: List(RequestOption(a)),
) {
  use <- bool.guard(
    request.scheme == http.Https,
    Error(error.HttpsNotSupportedYet),
  )

  let method = case request.method {
    http.Other(method) -> method
    _ -> {
      let assert Ok(method) = list.key_find(method_mapping, request.method)
      method
    }
  }

  let decoder = get_decoder(options)

  let #(body, headers) = case request.body {
    JSON(body) -> #(
      option.Some(body),
      list.append(
        request.headers,
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

    PlainText(body) -> #(
      option.Some(body),
      list.append(
        request.headers,
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

    EmptyRequestBody -> #(option.None, request.headers)
  }

  use request <- result.then(request.encode(
    conn.host,
    method,
    request.path,
    request.query,
    headers,
    body,
  ))

  use _ <- result.then(
    tcp.send(conn.socket, request)
    |> result.replace_error(error.UnableToSendRequest),
  )

  let ref = erlang.make_reference()
  Ok(#(
    Connection(
      conn.host,
      conn.socket,
      conn.buffer,
      list.append(conn.requests, [#(ref, decoder)]),
      conn.responses,
      conn.default_timeout,
    ),
    ref,
  ))
}

pub fn receive(conn: Connection(a), timeout) {
  let selector = tcp.new_selector()
  receive_internal(conn, selector, timeout)
}

pub fn build_request(
  method: http.Method,
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
  body: RequestBody,
) {
  gleam_http_request.new()
  |> gleam_http_request.set_scheme(http.Http)
  |> gleam_http_request.set_method(method)
  |> gleam_http_request.set_path(path)
  |> gleam_http_request.set_query(query)
  |> gleam_http_request.set_body(body)
  |> list.fold(
    headers,
    _,
    fn(request, header) {
      gleam_http_request.set_header(request, header.0, header.1)
    },
  )
}

pub fn build_empty_body_request(
  method: http.Method,
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
) {
  gleam_http_request.new()
  |> gleam_http_request.set_scheme(http.Http)
  |> gleam_http_request.set_method(method)
  |> gleam_http_request.set_path(path)
  |> gleam_http_request.set_query(query)
  |> gleam_http_request.set_body(EmptyRequestBody)
  |> list.fold(
    headers,
    _,
    fn(request, header) {
      gleam_http_request.set_header(request, header.0, header.1)
    },
  )
}

fn receive_internal(conn: Connection(a), selector, timeout) {
  case conn.requests {
    [#(ref, decoder), ..rest] -> {
      let data = case string.length(conn.buffer) {
        0 -> receive_packet(conn.socket, selector, "", now(), timeout)
        _ -> response.decode(conn.buffer)
      }

      let conn =
        Connection(
          conn.host,
          conn.socket,
          case data {
            Ok(#(_, rest)) -> string.append(conn.buffer, rest)
            _ -> conn.buffer
          },
          rest,
          list.append(
            conn.responses,
            [
              #(
                ref,
                data
                |> result.map(fn(response) {
                  case response.0 {
                    #(status, headers, option.Some(body)) ->
                      case decoder {
                        option.Some(decoder) ->
                          case json.decode(body, decoder) {
                            Ok(value) ->
                              Ok(gleam_http_response.Response(
                                status,
                                headers,
                                JSONDecoded(value),
                              ))
                            Error(decode_error) ->
                              Ok(gleam_http_response.Response(
                                status,
                                headers,
                                InvalidOrUnexpectedJSON(body, decode_error),
                              ))
                          }
                        option.None ->
                          Ok(gleam_http_response.Response(
                            status,
                            headers,
                            Raw(body),
                          ))
                      }

                    #(status, headers, option.None) ->
                      Ok(gleam_http_response.Response(
                        status,
                        headers,
                        EmptyResponseBody,
                      ))
                  }
                })
                |> result.flatten,
              ),
            ],
          ),
          conn.default_timeout,
        )

      case rest {
        [] -> conn
        _ -> receive_internal(conn, selector, timeout)
      }
    }
  }
}

pub fn get_response(conn: Connection(a), ref) {
  case list.key_pop(conn.responses, ref) {
    Ok(#(response, other_responses)) ->
      Ok(#(Connection(..conn, responses: other_responses), response))
    Error(Nil) -> Ok(#(conn, Error(error.TCPError(mug.Timeout))))
  }
}

pub fn shutdown(conn: Connection(a)) {
  let conn = receive(conn, conn.default_timeout)
  mug.shutdown(conn.socket)
  |> result.map(fn(_) { conn.responses })
  |> result.map_error(fn(tcp_error) { error.TCPError(tcp_error) })
}

const method_mapping = [
  #(http.Get, "GET"),
  #(http.Put, "PUT"),
  #(http.Post, "POST"),
  #(http.Head, "HEAD"),
  #(http.Patch, "PATCH"),
  #(http.Trace, "TRACE"),
  #(http.Delete, "DELETE"),
  #(http.Connect, "CONNECT"),
  #(http.Options, "OPTIONS"),
]

fn get_decoder(options: List(RequestOption(a))) {
  case
    list.find_map(
      options,
      fn(opt) {
        case opt {
          JSONDecoder(decoder) -> Ok(decoder)
          _ -> Error(Nil)
        }
      },
    )
  {
    Ok(decoder) -> option.Some(decoder)
    Error(Nil) -> option.None
  }
}

fn receive_packet(
  socket: mug.Socket,
  selector: process.Selector(Result(BitArray, mug.Error)),
  storage: String,
  start_time: Int,
  timeout: Int,
) {
  case response.decode(storage) {
    Ok(value) -> Ok(value)
    Error(error.MoreNeeded) -> {
      case now() - start_time >= timeout * 1_000_000 {
        True -> Error(error.TCPError(mug.Timeout))
        False ->
          case tcp.receive(socket, selector, timeout) {
            Error(tcp_error) -> Error(error.TCPError(tcp_error))
            Ok(packet) ->
              case bit_array.to_string(packet) {
                Ok(packet) ->
                  receive_packet(
                    socket,
                    selector,
                    string.append(storage, packet),
                    start_time,
                    timeout,
                  )

                Error(Nil) -> Error(error.IsNotString)
              }
          }
      }
    }

    Error(error) -> Error(error)
  }
}

@external(erlang, "erlang", "monotonic_time")
fn now() -> Int
