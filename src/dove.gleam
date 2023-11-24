import gleam/int
import gleam/bool
import gleam/list
import gleam/option
import gleam/string
import gleam/result
import gleam/dynamic
import gleam/bit_array
import gleam/json
import gleam/erlang
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request} as _
import gleam/http/response as gleam_http_response
import dove/error
import dove/request
import dove/response
import dove/internal/tcp
import dove/internal/request.{encode} as _
import dove/internal/response.{decode} as _
import mug

pub type RequestOption(a) {
  JSONDecoder(fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)))
}

pub opaque type Connection(a) {
  Connection(
    host: String,
    socket: mug.Socket,
    buffer: BitArray,
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
        Result(
          gleam_http_response.Response(option.Option(response.Body(a))),
          error.Error,
        ),
      ),
    ),
    default_timeout: Int,
  )
}

pub fn connect(host: String, port: Int, timeout: Int) {
  use socket <- result.then(tcp.connect(host, port, timeout))
  Ok(Connection(
    host <> ":" <> int.to_string(port),
    socket,
    <<>>,
    [],
    [],
    timeout,
  ))
}

pub fn request(
  conn: Connection(a),
  request: Request(option.Option(request.Body)),
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

  let #(body, headers) = case request.body {
    option.Some(request.JSON(body)) -> #(
      body
      |> bit_array.from_string
      |> option.Some,
      list.append(
        request.headers,
        [
          #("content-type", "application/json; charset=utf-8"),
          #(
            "content-length",
            body
            |> string.length
            |> int.to_string,
          ),
        ],
      ),
    )

    option.Some(request.PlainText(body)) -> #(
      body
      |> bit_array.from_string
      |> option.Some,
      list.append(
        request.headers,
        [
          #("content-type", "text/plain; charset=utf-8"),
          #(
            "content-length",
            body
            |> string.length
            |> int.to_string,
          ),
        ],
      ),
    )

    option.Some(request.OctetStream(body)) -> #(
      option.Some(body),
      list.append(
        request.headers,
        [
          #("content-type", "application/octet-stream"),
          #(
            "content-length",
            body
            |> bit_array.byte_size
            |> int.to_string,
          ),
        ],
      ),
    )

    option.None -> #(option.None, request.headers)
  }

  let decoder = get_decoder(options)
  use request <- result.then(encode(
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

fn receive_internal(conn: Connection(a), selector, timeout) {
  case conn.requests {
    [#(ref, decoder), ..rest] -> {
      let data = case bit_array.byte_size(conn.buffer) {
        0 -> receive_packet(conn.socket, selector, <<>>, now(), timeout)
        _ -> decode(conn.buffer)
      }

      let conn =
        Connection(
          conn.host,
          conn.socket,
          case data {
            Ok(#(_, rest)) -> rest
            _ -> <<>>
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
                          case json.decode_bits(body, decoder) {
                            Ok(value) ->
                              Ok(gleam_http_response.Response(
                                status,
                                headers,
                                option.Some(response.JSONDecoded(value)),
                              ))
                            Error(decode_error) ->
                              Ok(gleam_http_response.Response(
                                status,
                                headers,
                                option.Some(response.InvalidOrUnexpectedJSON(
                                  body,
                                  decode_error,
                                )),
                              ))
                          }

                        option.None ->
                          case bit_array.to_string(body) {
                            Ok(body) ->
                              case list.key_find(headers, "content-type") {
                                Ok(mime) ->
                                  case
                                    string.contains(mime, "application/json")
                                  {
                                    True ->
                                      Ok(gleam_http_response.Response(
                                        status,
                                        headers,
                                        option.Some(response.HeadersSayJSON(
                                          body,
                                        )),
                                      ))

                                    False ->
                                      Ok(gleam_http_response.Response(
                                        status,
                                        headers,
                                        option.Some(response.PlainText(body)),
                                      ))
                                  }

                                Error(Nil) ->
                                  Ok(gleam_http_response.Response(
                                    status,
                                    headers,
                                    option.Some(response.PlainText(body)),
                                  ))
                              }

                            Error(Nil) ->
                              Ok(gleam_http_response.Response(
                                status,
                                headers,
                                option.Some(response.OctetStream(body)),
                              ))
                          }
                      }

                    #(status, headers, option.None) ->
                      Ok(gleam_http_response.Response(
                        status,
                        headers,
                        option.None,
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
    [] -> conn
  }
}

pub fn get_response(conn: Connection(a), ref) {
  case list.key_pop(conn.responses, ref) {
    Ok(#(response, other_responses)) -> #(
      Connection(..conn, responses: other_responses),
      response,
    )
    Error(Nil) -> #(conn, Error(error.TCPError(mug.Timeout)))
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
  storage: BitArray,
  start_time: Int,
  timeout: Int,
) {
  case decode(storage) {
    Ok(value) -> Ok(value)
    Error(error.MoreNeeded) -> {
      case now() - start_time >= timeout * 1_000_000 {
        True -> Error(error.TCPError(mug.Timeout))
        False ->
          case tcp.receive(socket, selector, timeout) {
            Error(tcp_error) -> Error(error.TCPError(tcp_error))
            Ok(packet) -> {
              receive_packet(
                socket,
                selector,
                bit_array.append(storage, packet),
                start_time,
                timeout,
              )
            }
          }
      }
    }
    Error(error) -> Error(error)
  }
}

@external(erlang, "erlang", "monotonic_time")
fn now() -> Int
