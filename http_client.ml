(* HTTP Client using curl.multi and iomux for concurrent requests *)

open Curl
open Iomux

module Log = (val Logs.src_log (Logs.Src.create "http_client") : Logs.LOG)

(* Module for HTTP methods *)
module HttpMethod = struct
  type t = GET | POST | PUT | DELETE
  
  let to_string = function
    | GET -> "GET"
    | POST -> "POST" 
    | PUT -> "PUT"
    | DELETE -> "DELETE"
  
  let of_string = function
    | "GET" -> Some GET
    | "POST" -> Some POST
    | "PUT" -> Some PUT
    | "DELETE" -> Some DELETE
    | _ -> None
end

(* Module for HTTP requests *)
module HttpRequest = struct
  type t = {
    method_ : HttpMethod.t;
    url : string;
    headers : (string * string) list;
    body : string option;
    timeout : int;
  }
  
  let create ~method_ ~url ?(headers=[]) ?body ?(timeout=30) () = {
    method_; url; headers; body; timeout
  }
  
  let to_curl_handle req =
    let handle = Curl.init () in
    Curl.set_url handle req.url;
    Curl.set_timeout handle req.timeout;
    
    (* Set HTTP method *)
    (match req.method_ with
     | HttpMethod.GET -> Curl.set_httpget handle true
     | HttpMethod.POST -> Curl.set_post handle true
     | HttpMethod.PUT -> Curl.set_upload handle true
     | HttpMethod.DELETE -> Curl.set_customrequest handle "DELETE");
    
    (* Set headers *)
    let headers_list = List.map (fun (k, v) -> k ^ ": " ^ v) req.headers in
    Curl.set_httpheader handle headers_list;
    
    (* Set body for POST/PUT *)
    (match req.method_, req.body with
     | (HttpMethod.POST | HttpMethod.PUT), Some body -> Curl.set_postfields handle body
     | _ -> ());
    
    handle
end

(* Module for HTTP responses *)
module HttpResponse = struct
  type t = {
    status : int;
    headers : (string * string) list;
    body : string;
    error : string option;
  }
  
  let create ~status ~headers ~body ?error () = {
    status; headers; body; error
  }
  
  let is_success resp =
    resp.status >= 200 && resp.status < 300
  
  let get_header resp key =
    List.assoc_opt (String.uppercase_ascii key) 
      (List.map (fun (k, v) -> (String.uppercase_ascii k, v)) resp.headers)
end

exception Http_error of string

(* Convenience functions for creating requests *)
let init_curl_handle () =
  let handle = Curl.init () in
  Curl.set_timeout handle 30;
  Curl.set_followlocation handle true;
  Curl.set_maxredirs handle 5;
  handle

(* Streaming HTTP request with callback *)
let perform_streaming_request url headers body timeout callback =
  let handle = init_curl_handle () in
  
  (* Set URL and method *)
  Curl.set_url handle url;
  Curl.set_post handle true;
  
  (* Set headers *)
  let headers = List.map (fun (name, value) -> name ^ ": " ^ value) headers in
  Curl.set_httpheader handle headers;
  
  (* Set body *)
  Curl.set_postfields handle body;
  
  (* Set timeout *)
  Curl.set_timeout handle timeout;
  
  (* Set streaming callback *)
  Curl.set_writefunction handle (fun data ->
    callback data;
    String.length data
  );
  
  (* Perform request *)
  try
    Curl.perform handle;
    Curl.cleanup handle;
    Ok ()
  with
  | Curl.CurlException (code, _, msg) ->
      Curl.cleanup handle;
      Error (msg ^ " (" ^ Curl.strerror code ^ ")")
  | exn ->
      Curl.cleanup handle;
      Error (Printexc.to_string exn)

let set_request_options handle req =
  Curl.set_url handle req.HttpRequest.url;
  
  (* Set HTTP method *)
  begin match req.HttpRequest.method_ with
    | HttpMethod.POST -> Curl.set_post handle true
    | HttpMethod.PUT -> Curl.set_customrequest handle "PUT"
    | HttpMethod.DELETE -> Curl.set_customrequest handle "DELETE"
    | HttpMethod.GET -> () (* GET is default *)
  end;
  
  (* Set headers *)
  let headers = List.map (fun (name, value) -> name ^ ": " ^ value) req.HttpRequest.headers in
  Curl.set_httpheader handle headers;
  
  (* Set body for POST/PUT *)
  begin match req.HttpRequest.method_, req.HttpRequest.body with
    | (HttpMethod.POST | HttpMethod.PUT), Some body -> Curl.set_postfields handle body
    | _ -> ()
  end;
  
  (* Set timeout *)
  Curl.set_timeout handle req.HttpRequest.timeout

let perform_request handle =
  let response_body = Buffer.create 1024 in
  Curl.set_writefunction handle (fun data ->
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
  
  HttpResponse.create ~status:status_code ~headers:(List.rev !response_headers) ~body:(Buffer.contents response_body) ()

let cleanup_curl_handle handle =
  Curl.cleanup handle

let make_request req =
  let handle = init_curl_handle () in
  set_request_options handle req;
  
  try
    let response = perform_request handle in
    Log.debug (fun m -> m "HTTP Response: Status %d, Body length: %d"
                     response.HttpResponse.status (String.length response.HttpResponse.body));
    cleanup_curl_handle handle;
    response
  with
  | Http_error msg ->
      Log.err (fun m -> m "HTTP Error: %s" msg);
      cleanup_curl_handle handle;
      HttpResponse.create ~status:0 ~headers:[] ~body:"" ~error:msg ()
  | exn ->
      Log.err (fun m -> m "HTTP Exception: %s" (Printexc.to_string exn));
      cleanup_curl_handle handle;
      HttpResponse.create ~status:0 ~headers:[] ~body:"" ~error:(Printexc.to_string exn) ()

(* Multi request handling using curl.multi *)
let perform_multi_requests requests =
  let multi_handle = Curl.Multi.create () in
  let easy_handles = List.map (fun req ->
    let handle = init_curl_handle () in
    set_request_options handle req;
    Curl.Multi.add multi_handle handle;
    (handle, req)
  ) requests in
  
  let results = ref [] in
  
  try
    (* Main loop for multi requests *)
    let still_running = ref true in
    while !still_running do
      let running_handles = Curl.Multi.perform multi_handle in
      if running_handles = 0 then
        still_running := false
      else
        (* Simple wait - sleep for a short time *)
        Unix.sleepf 0.1
    done;
    
    (* Collect results *)
    List.iter (fun (handle, req) ->
      try
        let response = perform_request handle in
        results := response :: !results
      with exn ->
        results := HttpResponse.create ~status:0 ~headers:[] ~body:"" ~error:(Printexc.to_string exn) () :: !results
    ) easy_handles;
    
    (* Cleanup *)
    List.iter (fun (handle, _) ->
      Curl.Multi.remove multi_handle handle;
      cleanup_curl_handle handle
    ) easy_handles;
    
    List.rev !results
    
  with exn ->
    (* Cleanup on error *)
    List.iter (fun (handle, _) ->
      cleanup_curl_handle handle
    ) easy_handles;
    raise exn

(* Simple GET request *)
let get url headers timeout =
  let req = HttpRequest.create ~method_:HttpMethod.GET ~url ~headers ~timeout () in
  make_request req

(* Simple POST request *)
let post url headers body timeout =
  let req = HttpRequest.create ~method_:HttpMethod.POST ~url ~headers ~body:body ~timeout () in
  make_request req

let post_streaming url headers body timeout callback =
  perform_streaming_request url headers body timeout callback

(* Simple PUT request *)
let put url headers body timeout =
  let req = HttpRequest.create ~method_:HttpMethod.PUT ~url ~headers ~body:body ~timeout () in
  make_request req

(* Simple DELETE request *)
let delete url headers timeout =
  let req = HttpRequest.create ~method_:HttpMethod.DELETE ~url ~headers ~timeout () in
  make_request req