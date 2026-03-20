(* HTTP Client using curl.multi and iomux for concurrent requests *)

module Log = (val Logs.src_log (Logs.Src.create "http_client") : Logs.LOG)

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

  response_of_parts ~status:status_code ~headers:(List.rev !response_headers) ~body:(Buffer.contents response_body)

let cleanup_curl_handle handle =
  Curl.cleanup handle

type transfer_state = {
  index : int;
  response_body : Buffer.t;
  response_headers : (string * string) list ref;
}

let status_code_of_handle handle =
  let code = Curl.get_responsecode handle in
  if code = 0 then 200 else code

let response_of_transfer handle state =
  response_of_parts
    ~status:(status_code_of_handle handle)
    ~headers:(List.rev !(state.response_headers))
    ~body:(Buffer.contents state.response_body)

let drain_finished multi_handle states active_handles results remaining =
  let rec loop remaining =
    match Curl.Multi.remove_finished multi_handle with
    | None -> remaining
    | Some (handle, result_code) ->
        begin
          match Hashtbl.find_opt states handle with
          | Some state ->
              let response =
                match result_code with
                | Curl.CURLE_OK -> Ok (response_of_transfer handle state)
                | code -> Error (Curl.strerror code)
              in
              results.(state.index) <- Some response;
              Hashtbl.remove states handle
          | None -> ()
        end;
        Hashtbl.remove active_handles handle;
        cleanup_curl_handle handle;
        loop (remaining - 1)
  in
  loop remaining

let execute_request ?body ?(timeout=30) ?on_write request =
  let req = { request; body; timeout; on_write } in
  let handle = init_curl_handle () in
  set_request_options handle req;
  
  try
    let response = perform_request handle in
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

(* Multi request handling using curl.multi with proper event loop *)
let execute_requests ?(timeout=30) ?on_write requests =
  if requests = [] then []
  else
    let multi_handle = Curl.Multi.create () in
    let states = Hashtbl.create (List.length requests) in
    let active_handles = Hashtbl.create (List.length requests) in
    let socket_polls = Hashtbl.create 16 in
    let next_timeout_ms = ref None in
    let results : ((H1.Response.t * string, string) result option) array =
      Array.make (List.length requests) None
    in
    let register_socket fd poll =
      match poll with
      | Curl.Multi.POLL_REMOVE -> Hashtbl.remove socket_polls fd
      | poll -> Hashtbl.replace socket_polls fd poll
    in
    let set_timeout ms =
      if ms < 0 then next_timeout_ms := None
      else next_timeout_ms := Some ms
    in
    Curl.Multi.set_socket_function multi_handle register_socket;
    Curl.Multi.set_timer_function multi_handle set_timeout;
    let perform_timeout () =
      Curl.Multi.action_timeout multi_handle
    in
    let perform_socket_action fd flags =
      let has_in =
        Iomux.Poll.Flags.mem Iomux.Poll.Flags.pollin flags
        || Iomux.Poll.Flags.mem Iomux.Poll.Flags.pollpri flags
        || Iomux.Poll.Flags.mem Iomux.Poll.Flags.pollhup flags
        || Iomux.Poll.Flags.mem Iomux.Poll.Flags.pollerr flags
      in
      let has_out =
        Iomux.Poll.Flags.mem Iomux.Poll.Flags.pollout flags
        || Iomux.Poll.Flags.mem Iomux.Poll.Flags.pollhup flags
        || Iomux.Poll.Flags.mem Iomux.Poll.Flags.pollerr flags
      in
      let status =
        match has_in, has_out with
        | true, true -> Curl.Multi.EV_INOUT
        | true, false -> Curl.Multi.EV_IN
        | false, true -> Curl.Multi.EV_OUT
        | false, false -> Curl.Multi.EV_AUTO
      in
      ignore (Curl.Multi.action multi_handle fd status)
    in
    let cleanup_remaining_handles () =
      Hashtbl.iter
        (fun handle _ ->
          (try Curl.Multi.remove multi_handle handle with _ -> ());
          cleanup_curl_handle handle)
        active_handles;
      Hashtbl.clear active_handles;
      Hashtbl.clear states
    in
    try
      List.iteri
        (fun i (request, body) ->
          let req = {
            request;
            body;
            timeout;
            on_write = Option.map (fun callback -> callback i) on_write;
          } in
          let handle = init_curl_handle () in
          set_request_options handle req;
          let response_body = Buffer.create 1024 in
          let response_headers = ref [] in
          Curl.set_writefunction handle (fun data ->
              (match req.on_write with
               | Some cb -> cb data
               | None -> Buffer.add_string response_body data);
              String.length data);
          Curl.set_headerfunction handle (fun header ->
              if String.length header > 0 && header.[0] <> '\r' && header.[0] <> '\n' then (
                try
                  let colon_pos = String.index header ':' in
                  let name = String.trim (String.sub header 0 colon_pos) in
                  let value = String.trim (String.sub header (colon_pos + 1) (String.length header - colon_pos - 1)) in
                  response_headers := (name, value) :: !response_headers
                with Not_found -> ()
              );
              String.length header);
          Hashtbl.add states handle { index = i; response_body; response_headers };
          Hashtbl.add active_handles handle ();
          Curl.Multi.add multi_handle handle)
        requests;
      let remaining = ref (List.length requests) in
      perform_timeout ();
      remaining := drain_finished multi_handle states active_handles results !remaining;
      while !remaining > 0 do
        begin
          match !next_timeout_ms with
          | Some 0 ->
              next_timeout_ms := None;
              perform_timeout ()
          | _ ->
              let socket_entries = Hashtbl.to_seq socket_polls |> List.of_seq in
              if socket_entries = [] then (
                let timeout_ms =
                  match !next_timeout_ms with
                  | Some ms -> max 0 ms
                  | None ->
                      let ms =
                        try Curl.Multi.timeout multi_handle with _ -> 1000
                      in
                      if ms < 0 then 1000 else ms
                in
                ignore (Unix.select [] [] [] (float_of_int timeout_ms /. 1000.0));
                next_timeout_ms := None;
                perform_timeout ()
              ) else (
                let poller = Iomux.Poll.create ~maxfds:(List.length socket_entries) () in
                List.iteri
                  (fun idx (fd, poll) ->
                    let events =
                      match poll with
                      | Curl.Multi.POLL_NONE -> Iomux.Poll.Flags.empty
                      | Curl.Multi.POLL_IN ->
                          Iomux.Poll.Flags.(pollin + pollpri)
                      | Curl.Multi.POLL_OUT ->
                          Iomux.Poll.Flags.pollout
                      | Curl.Multi.POLL_INOUT ->
                          Iomux.Poll.Flags.(pollin + pollpri + pollout)
                      | Curl.Multi.POLL_REMOVE -> Iomux.Poll.Flags.empty
                    in
                    Iomux.Poll.set_index poller idx fd events)
                  socket_entries;
                let timeout : Iomux.Poll.poll_timeout =
                  match !next_timeout_ms with
                  | None -> Iomux.Poll.Infinite
                  | Some ms -> Iomux.Poll.Milliseconds (max 0 ms)
                in
                let nready = Iomux.Poll.poll poller (List.length socket_entries) timeout in
                next_timeout_ms := None;
                if nready = 0 then
                  perform_timeout ()
                else
                  Iomux.Poll.iter_ready poller nready (fun _ fd flags ->
                      perform_socket_action fd flags)
              )
        end;
        remaining := drain_finished multi_handle states active_handles results !remaining
      done;
      Hashtbl.clear active_handles;
      Curl.Multi.cleanup multi_handle;
      Array.to_list results |> List.map (function Some response -> response | None -> Error "Request failed")
    with exn ->
      cleanup_remaining_handles ();
      Curl.Multi.cleanup multi_handle;
      Log.err (fun m -> m "Multi request exception: %s" (Printexc.to_string exn));
      []
