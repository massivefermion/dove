pub type Response(a) {
  Response(
    status: HttpStatus,
    headers: List(#(String, String)),
    body: ResponseBody(a),
  )
}

pub type ResponseBody(a) {
  Raw(String)
  Decoded(a)
}

pub type HttpStatus {
  Continue
  SwitchingProtocols
  Processing
  EarlyHints
  OK
  Created
  Accepted
  NonAuthoritativeInformation
  NoContent
  ResetContent
  PartialContent
  MultiStatus
  MultipleChoices
  MovedPermanently
  Found
  SeeOther
  NotModified
  UseProxy
  TemporaryRedirect
  BadRequest
  Unauthorized
  PaymentRequired
  Forbidden
  NotFound
  MethodNotAllowed
  NotAcceptable
  ProxyAuthenticationRequired
  RequestTimeout
  Conflict
  Gone
  LengthRequired
  PreconditionFailed
  PayloadTooLarge
  URITooLong
  UnsupportedMediaType
  RangeNotSatisfiable
  ExpectationFailed
  ImATeapot
  UnprocessableEntity
  Locked
  FailedDependency
  TooEarly
  UpgradeRequired
  PreconditionRequired
  TooManyRequests
  RequestHeaderFieldsTooLarge
  UnavailableForLegalReasons
  InternalServerError
  NotImplemented
  BadGateway
  ServiceUnavailable
  GatewayTimeout
  HTTPVersionNotSupported
  VariantAlsoNegotiates
  InsufficientStorage
  LoopDetected
  NotExtended
  NetworkAuthenticationRequired
}

pub const codes_mapping = [
  #(100, Continue),
  #(101, SwitchingProtocols),
  #(102, Processing),
  #(103, EarlyHints),
  #(200, OK),
  #(201, Created),
  #(202, Accepted),
  #(203, NonAuthoritativeInformation),
  #(204, NoContent),
  #(205, ResetContent),
  #(206, PartialContent),
  #(207, MultiStatus),
  #(300, MultipleChoices),
  #(301, MovedPermanently),
  #(302, Found),
  #(303, SeeOther),
  #(304, NotModified),
  #(305, UseProxy),
  #(307, TemporaryRedirect),
  #(400, BadRequest),
  #(401, Unauthorized),
  #(402, PaymentRequired),
  #(403, Forbidden),
  #(404, NotFound),
  #(405, MethodNotAllowed),
  #(406, NotAcceptable),
  #(407, ProxyAuthenticationRequired),
  #(408, RequestTimeout),
  #(409, Conflict),
  #(410, Gone),
  #(411, LengthRequired),
  #(412, PreconditionFailed),
  #(413, PayloadTooLarge),
  #(414, URITooLong),
  #(415, UnsupportedMediaType),
  #(416, RangeNotSatisfiable),
  #(417, ExpectationFailed),
  #(418, ImATeapot),
  #(422, UnprocessableEntity),
  #(423, Locked),
  #(424, FailedDependency),
  #(425, TooEarly),
  #(426, UpgradeRequired),
  #(428, PreconditionRequired),
  #(429, TooManyRequests),
  #(431, RequestHeaderFieldsTooLarge),
  #(451, UnavailableForLegalReasons),
  #(500, InternalServerError),
  #(501, NotImplemented),
  #(502, BadGateway),
  #(503, ServiceUnavailable),
  #(504, GatewayTimeout),
  #(505, HTTPVersionNotSupported),
  #(506, VariantAlsoNegotiates),
  #(507, InsufficientStorage),
  #(508, LoopDetected),
  #(510, NotExtended),
  #(511, NetworkAuthenticationRequired),
]
