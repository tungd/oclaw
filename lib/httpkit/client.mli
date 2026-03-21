(** Curl-backed executor for HTTP requests built with H1 types. *)

(** Perform a single HTTP request.

    The caller constructs the request with [H1.Request.create]. The optional
    [body] is sent as the request payload. If [on_write] is provided, it is
    called with each response chunk as it arrives.

    Returns [Ok (response, body)] when an HTTP response was received, including
    non-2xx statuses. Returns [Error msg] for transport/runtime failures. *)
val execute_request :
  ?body:string ->
  ?timeout:int ->
  ?on_write:(string -> unit) ->
  H1.Request.t ->
  (H1.Response.t * string, string) result
