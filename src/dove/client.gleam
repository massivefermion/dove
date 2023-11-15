import gleam/int
import gleam/list
import gleam/result
import gleam/option
import gleam/dynamic
import gleam/bit_array
import gleam/json
import gleam/otp/actor
import gleam/erlang/process
import dove/tcp
import dove/error.{type Error}
import dove/response/decoder.{decode}
import dove/response.{type Response, Decoded, Raw, Response, codes_mapping}
import mug

pub type Connection(a) {
  Connection(host: String, client: process.Subject(Message(a)))
}

type Reply(a) =
  Result(Response(a), Error)

// type Reply =
//   Result(#(#(Int, String), List(#(String, String)), String), Error)

pub type Message(a) {
  Shutdown
  Request(
    BitArray,
    process.Subject(Reply(a)),
    option.Option(fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError))),
    Int,
  )
}

pub fn connect(host: String, port: Int, timeout: Int) {
  use subject <- result.then(actor.start_spec(actor.Spec(
    init: fn() {
      case tcp.connect(host, port, timeout) {
        Ok(socket) -> actor.Ready(socket, process.new_selector())
        Error(_) -> actor.Failed("Unable to connect to Redis server")
      }
    },
    init_timeout: timeout,
    loop: handle_message,
  )))

  { host <> ":" <> int.to_string(port) }
  |> Connection(subject)
  |> Ok
}

fn handle_message(msg: Message(a), socket: mug.Socket) {
  case msg {
    Request(req, reply_with, decoder_option, timeout) -> {
      case tcp.send(socket, req) {
        Ok(Nil) -> {
          let selector = tcp.new_selector()
          case receive(socket, selector, <<>>, now(), timeout) {
            Ok(reply) -> {
              let assert Ok(status) = list.key_find(codes_mapping, reply.0)
              case decoder_option {
                option.Some(decoder) ->
                  case json.decode(reply.2, decoder) {
                    Ok(value) ->
                      actor.send(
                        reply_with,
                        Ok(Response(status, reply.1, Decoded(value))),
                      )
                    Error(decode_error) ->
                      actor.send(
                        reply_with,
                        Error(error.DecodeError(decode_error)),
                      )
                  }
                option.None ->
                  actor.send(
                    reply_with,
                    Ok(Response(status, reply.1, Raw(reply.2))),
                  )
              }

              actor.continue(socket)
            }

            Error(error) -> {
              let _ = mug.shutdown(socket)
              actor.send(reply_with, Error(error))
              actor.Stop(process.Abnormal("TCP Error"))
            }
          }
        }

        Error(error) -> {
          let _ = mug.shutdown(socket)
          actor.send(reply_with, Error(error.TCPError(error)))
          actor.Stop(process.Abnormal("TCP Error"))
        }
      }
    }

    Shutdown -> {
      let _ = mug.shutdown(socket)
      actor.Stop(process.Normal)
    }
  }
}

fn receive(
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
            Ok(packet) ->
              receive(
                socket,
                selector,
                bit_array.append(storage, packet),
                start_time,
                timeout,
              )
          }
      }
    }
    Error(error) -> Error(error)
  }
}

@external(erlang, "erlang", "monotonic_time")
fn now() -> Int
// @external(erlang, "dove_ffi", "send")
// fn send(pid: process.Pid, msg: a) -> b
