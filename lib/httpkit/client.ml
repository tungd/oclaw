(* Curl-backed HTTP client built on H1 request/response types. *)

module Log = (val Logs.src_log (Logs.Src.create "httpkit.client") : Logs.LOG)

let default_user_agent = "OpenAI/Go 3.22.0"

type request_options = {
  request : H1.Request.t;
  body : string option;
  timeout : int;
  on_write : (string -> unit) option;
}

let has_ci_header name headers =
  let lname = String.lowercase_ascii name in
  List.exists (fun (k, _v) -> String.equal (String.lowercase_ascii k) lname) headers

let with_default_request_headers headers =
  if has_ci_header "user-agent" headers then headers
  else ("User-Agent", default_user_agent) :: headers

let to_curl_headers headers =
  with_default_request_headers headers
  |> List.map (fun (k, v) -> k ^ ": " ^ v)

exception Http_error of string

(* Convenience functions for creating requests *)
let init_curl_handle () =
  let handle = Curl.init () in
  Curl.set_timeout handle 30;
  Curl.set_followlocation handle true;
  Curl.set_maxredirs handle 5;
  handle

let set_request_options handle req =
  Curl.set_url handle req.request.H1.Request.target;

  let method_name = H1.Method.to_string req.request.H1.Request.meth in
  begin
    if String.equal method_name "GET" then
      Curl.set_httpget handle true
    else if String.equal method_name "POST" then
      Curl.set_post handle true
    else
      Curl.set_customrequest handle method_name
  end;

  let headers = req.request.H1.Request.headers |> H1.Headers.to_list |> to_curl_headers in
  Curl.set_httpheader handle headers;

  begin
    match req.body with
    | Some body -> Curl.set_postfields handle body
    | None -> ()
  end;

  Curl.set_timeout handle req.timeout

let response_status_of_code code =
  H1.Status.of_code code

let response_of_parts ~status ~headers ~body =
  let response_headers = H1.Headers.of_list headers in
  let response = H1.Response.create ~headers:response_headers (response_status_of_code status) in
  (response, body)

let perform_request ?on_write handle =
  let response_body = Buffer.create 1024 in
  Curl.set_writefunction handle (fun data ->
    Option.iter (fun callback -> callback data) on_write;
    Buffer.add_string response_body data;
    String.length data
  );
  
  let response_headers = ref [] in
  Curl.set_headerfunction handle (fun header ->
    (* Parse header line *)
    if String.length header > 0 && header.[0] <> '\r' && header.[0] <> '\n' then (
      try
        let colon_pos = String.index header ':' in
        let name = String.trim (String.sub header 0 colon_pos) in
        let value = String.trim (String.sub header (colon_pos + 1) (String.length header - colon_pos - 1)) in
        response_headers := (name, value) :: !response_headers
      with Not_found -> () (* Skip malformed headers *)
    );
    String.length header
  );
  
  (
    try
      Curl.perform handle
    with
    | Curl.CurlException (code, _, msg) ->
      raise (Http_error (msg ^ " (" ^ Curl.strerror code ^ ")"))
  );
  
  let status_code = 
    let code = Curl.get_responsecode handle in
    if code = 0 then 200 else code (* curl returns 0 if no HTTP status was received *)
  in

  response_of_parts ~status:status_code ~headers:(List.rev !response_headers) ~body:(Buffer.contents response_body)

let cleanup_curl_handle handle =
  Curl.cleanup handle

let execute_request ?body ?(timeout=30) ?on_write request =
  let req = { request; body; timeout; on_write } in
  let handle = init_curl_handle () in
  set_request_options handle req;
  
  try
    let response = perform_request ?on_write:req.on_write handle in
    cleanup_curl_handle handle;
    Ok response
  with
  | Http_error msg ->
      Log.err (fun m -> m "HTTP Error: %s" msg);
      cleanup_curl_handle handle;
      Error msg
  | exn ->
      Log.err (fun m -> m "HTTP Exception: %s" (Printexc.to_string exn));
      cleanup_curl_handle handle;
      Error (Printexc.to_string exn)
