import gleam/list
import gleam/option
import gleam/http
import gleam/http/request as gleam_http_request

pub type Body {
  JSON(String)
  PlainText(String)
  OctetStream(BitArray)
}

pub fn build_request(
  method: http.Method,
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
  body: option.Option(Body),
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

pub fn build_get(
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
) {
  build_request(http.Get, headers, path, query, option.None)
}

pub fn build_post(
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
  body: Body,
) {
  build_request(http.Post, headers, path, query, option.Some(body))
}

pub fn build_put(
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
  body: Body,
) {
  build_request(http.Put, headers, path, query, option.Some(body))
}

pub fn build_delete(
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
) {
  build_request(http.Delete, headers, path, query, option.None)
}

pub fn build_patch(
  headers: List(http.Header),
  path: String,
  query: List(#(String, String)),
  body: Body,
) {
  build_request(http.Patch, headers, path, query, option.Some(body))
}
