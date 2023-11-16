import gleam/int
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
import gleam/http/response as gleam_http_response
import dove/tcp
import dove/error
import dove/request
import dove/response
import mug

pub type RequestOption(a) {
  Body(RequestBody)
  Headers(List(#(String, String)))
  QueryParams(List(#(String, String)))
  ResponseDecoder(fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)))
}

pub type RequestBody {
  JSON(String)
  PlainText(String)
}

pub type ResponseBody(a) {
  Empty
  Decoded(a)
  Raw(String)
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
  )
}

pub fn connect(host: String, port: Int, timeout: Int) {
  use socket <- result.then(tcp.connect(host, port, timeout))
  Ok(Connection(host <> ":" <> int.to_string(port), socket, "", [], []))
}

pub fn request(
  conn: Connection(a),
  method: http.Method,
  path: String,
  options: List(RequestOption(a)),
) {
  let method = case method {
    http.Other(method) -> method
    _ -> {
      let assert Ok(method) = list.key_find(method_mapping, method)
      method
    }
  }

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
    ),
    ref,
  ))
}

pub fn receive(conn: Connection(a), timeout) {
  let selector = tcp.new_selector()
  receive_internal(conn, selector, timeout)
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
                                Decoded(value),
                              ))
                            Error(decode_error) ->
                              Error(error.DecodeError(decode_error))
                          }
                        option.None ->
                          Ok(gleam_http_response.Response(
                            status,
                            headers,
                            Raw(body),
                          ))
                      }

                    #(status, headers, option.None) ->
                      Ok(gleam_http_response.Response(status, headers, Empty))
                  }
                })
                |> result.flatten,
              ),
            ],
          ),
        )

      case rest {
        [] -> conn
        _ -> receive_internal(conn, selector, timeout)
      }
    }
  }
}

pub fn get_response(conn: Connection(a), ref) {
  result.flatten(
    list.key_find(conn.responses, ref)
    |> result.replace_error(error.TCPError(mug.Timeout)),
  )
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
