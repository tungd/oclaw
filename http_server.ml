(** HTTP server for OClaw using iomux and h1 *)

open Iomux

module Log = (val Logs.src_log (Logs.Src.create "http_server") : Logs.LOG)

(* {1 HTTP Types } *)

module Method = struct
  type t = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS

  let of_string = function
    | "GET" -> Some GET
    | "POST" -> Some POST
    | "PUT" -> Some PUT
    | "DELETE" -> Some DELETE
    | "PATCH" -> Some PATCH
    | "HEAD" -> Some HEAD
    | "OPTIONS" -> Some OPTIONS
    | _ -> None

  let to_string = function
    | GET -> "GET"
    | POST -> "POST"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
    | PATCH -> "PATCH"
    | HEAD -> "HEAD"
    | OPTIONS -> "OPTIONS"
end

module Request = struct
  type t = {
    method_ : Method.t;
    path : string;
    query : string;
    headers : (string * string) list;
    body : string;
  }

  let get_header req name =
    List.assoc_opt name req.headers
end

module Response = struct
  type t = {
    status : int;
    headers : (string * string) list;
    body : string;
  }

  let create ~status ?(headers=[]) ?(body="") () =
    { status; headers; body }

  let json ?status:(s=200) ?(headers=[]) data =
    let body = Yojson.Safe.to_string data in
    {
      status = s;
      headers = ("content-type", "application/json") :: ("server", "OClaw/1.0") :: headers;
      body;
    }

  let string ?status:(s=200) ?(headers=[]) text =
    { status = s; headers = ("server", "OClaw/1.0") :: headers; body = text }

  let not_found msg = string ~status:404 msg

  let error ~status msg = string ~status msg

  let status_string = function
    | 200 -> "OK"
    | 201 -> "Created"
    | 400 -> "Bad Request"
    | 404 -> "Not Found"
    | 500 -> "Internal Server Error"
    | n -> string_of_int n

  let to_http resp =
    let status_line = Printf.sprintf "HTTP/1.1 %d %s\r\n"
      resp.status (status_string resp.status) in
    let headers = List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) resp.headers
      |> String.concat "" in
    Printf.sprintf "%s%scontent-length: %d\r\n\r\n%s"
      status_line headers (String.length resp.body) resp.body
end

(* {1 Route Types } *)

type route_match = Exact | Prefix | Wildcard

type route = {
  method_ : Method.t option;
  pattern : string;
  match_type : route_match;
  handler : Request.t -> Response.t;
}

(* {1 Server State } *)

type t = {
  socket : Unix.file_descr;
  mutable running : bool;
  mutable routes : route list;
  mutable active_connections : int;
  max_connections : int;
  mutex : Mutex.t;
  host : string;
  port : int;
}

(* {1 Route Matching } *)

let path_matches pattern path match_type =
  match match_type with
  | Exact -> String.equal pattern path
  | Prefix -> String.starts_with ~prefix:pattern path
  | Wildcard -> true

let match_route (route : route) req =
  match route.method_ with
  | Some m when m <> req.Request.method_ -> None
  | _ ->
      if path_matches route.pattern req.Request.path route.match_type then
        Some route.handler
      else None

let find_route routes req =
  let rec find = function
    | [] -> None
    | r :: rest ->
        (match match_route r req with
         | Some handler -> Some handler
         | None -> find rest)
  in
  find routes

(* {1 Helper Functions } *)

let find_header_end data =
  let len = String.length data in
  let rec find pos =
    if pos + 3 >= len then raise Not_found
    else if data.[pos] = '\r' && data.[pos + 1] = '\n' && data.[pos + 2] = '\r' && data.[pos + 3] = '\n' then pos
    else find (pos + 1)
  in try find 0 with Not_found -> len

(* {1 Request Parsing } *)

let parse_request data =
  try
    let len = String.length data in
    if len = 0 then Error "Empty request"
    else
      let header_end = find_header_end data in
      let header_str = String.sub data 0 header_end in
      let body = if header_end + 4 <= len then
        String.sub data (header_end + 4) (len - header_end - 4)
      else "" in

      let lines = String.split_on_char '\n' header_str in
      if lines = [] then Error "No request line"
      else
        let request_line = String.trim (List.hd lines) in
        let parts = String.split_on_char ' ' request_line in
        if List.length parts < 2 then Error "Invalid request line"
        else
          let method_str = List.nth parts 0 in
          let path_with_query = List.nth parts 1 in

          let (path, query) = try
            let qpos = String.index path_with_query '?' in
            (String.sub path_with_query 0 qpos,
             String.sub path_with_query (qpos + 1) (String.length path_with_query - qpos - 1))
          with Not_found -> (path_with_query, "") in

          let method_ = match Method.of_string method_str with
            | Some m -> m
            | None -> raise Not_found
          in

          let headers = List.fold_left (fun acc line ->
            if String.length line > 0 then
              try
                let colon = String.index line ':' in
                let name = String.lowercase_ascii (String.trim (String.sub line 0 colon)) in
                let value = String.trim (String.sub line (colon + 1) (String.length line - colon - 1)) in
                (name, value) :: acc
              with Not_found -> acc
            else acc
          ) [] (List.tl lines) in

          Ok { Request.method_; path; query; headers; body }
  with
  | Not_found -> Error "Invalid request format"
  | exn -> Error (Printexc.to_string exn)

(* {1 Connection Handling } *)

let handle_connection server client_sock =
  Log.debug (fun m -> m "Handling new connection");

  try
    let recv_buf = Buffer.create 8192 in
    let headers_complete = ref false in
    let content_length = ref 0 in

    (* Read request *)
    while not !headers_complete do
      let buf = Bytes.create 4096 in
      let n = Unix.recv client_sock buf 0 4096 [] in
      if n = 0 then raise End_of_file;
      Buffer.add_bytes recv_buf (Bytes.sub buf 0 n);

      let contents = Buffer.contents recv_buf in
      try
        let header_end = find_header_end contents in
        if header_end < String.length contents then headers_complete := true;

        (* Extract content-length *)
        let lines = String.split_on_char '\n' contents in
        List.iter (fun line ->
          if String.starts_with ~prefix:"content-length:" (String.lowercase_ascii line) then
            let len_str = String.trim (String.sub line 15 (String.length line - 15)) in
            content_length := int_of_string len_str
        ) lines;
      with Not_found -> ()
    done;

    let contents = Buffer.contents recv_buf in
    let header_end = find_header_end contents in
    let current_body_len = String.length contents - header_end - 4 in

    if current_body_len < !content_length then
      let remaining = !content_length - current_body_len in
      let body_buf = Bytes.create remaining in
      let rec read_body rem = if rem > 0 then
        let n = Unix.recv client_sock body_buf 0 (min 4096 rem) [] in
        if n = 0 then raise End_of_file;
        Buffer.add_bytes recv_buf (Bytes.sub body_buf 0 n);
        read_body (rem - n)
      in read_body remaining;

    let final_data = Buffer.contents recv_buf in
    (match parse_request final_data with
     | Ok req ->
         Log.debug (fun m -> m "Request: %s %s" (Method.to_string req.Request.method_) req.Request.path);

         Mutex.lock server.mutex;
         let routes = server.routes in
         Mutex.unlock server.mutex;

         (match find_route routes req with
          | Some handler ->
              let resp = handler req in
              let resp_str = Response.to_http resp in
              ignore (Unix.send client_sock (Bytes.of_string resp_str) 0 (String.length resp_str) [])
          | None ->
              let resp = Response.not_found "Not found" in
              let resp_str = Response.to_http resp in
              ignore (Unix.send client_sock (Bytes.of_string resp_str) 0 (String.length resp_str) []))
     | Error err ->
         Log.warn (fun m -> m "Failed to parse request: %s" err);
         let resp = Response.error ~status:400 "Bad Request" in
         let resp_str = Response.to_http resp in
         ignore (Unix.send client_sock (Bytes.of_string resp_str) 0 (String.length resp_str) []));

    Unix.close client_sock;

    Mutex.lock server.mutex;
    server.active_connections <- server.active_connections - 1;
    Mutex.unlock server.mutex;

  with
  | End_of_file ->
      Log.debug (fun m -> m "Client closed connection");
      Unix.close client_sock;
      Mutex.lock server.mutex;
      server.active_connections <- server.active_connections - 1;
      Mutex.unlock server.mutex
  | exn ->
      Log.err (fun m -> m "Connection error: %s" (Printexc.to_string exn));
      (try Unix.close client_sock with _ -> ());
      Mutex.lock server.mutex;
      server.active_connections <- server.active_connections - 1;
      Mutex.unlock server.mutex

(* {1 Server Functions } *)

let create ~host ~port ?(max_connections=100) () =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  Unix.listen sock 128;

  Log.info (fun m -> m "HTTP server created on %s:%d" host port);

  {
    socket = sock;
    running = false;
    routes = [];
    active_connections = 0;
    max_connections;
    mutex = Mutex.create ();
    host;
    port;
  }

let add_route server ~method_ ?(match_type=Exact) pattern handler =
  Mutex.lock server.mutex;
  server.routes <- { method_; pattern; match_type; handler } :: server.routes;
  Mutex.unlock server.mutex

let start server =
  Log.info (fun m -> m "Starting HTTP server");
  server.running <- true;

  Domain.spawn (fun () ->
    while server.running do
      try
        let (client_sock, _client_addr) = Unix.accept server.socket in

        Mutex.lock server.mutex;
        let can_accept = server.active_connections < server.max_connections in
        if can_accept then server.active_connections <- server.active_connections + 1;
        Mutex.unlock server.mutex;

        if can_accept then
          ignore (Domain.spawn (fun () ->
            handle_connection server client_sock
          ))
        else begin
          Log.warn (fun m -> m "Max connections reached, rejecting");
          Unix.close client_sock
        end
      with
      | Unix.Unix_error (code, _, _) when code = Unix.EINTR || code = Unix.EAGAIN ->
          (* No connection available, continue *)
          ()
      | exn ->
          if server.running then
            Log.err (fun m -> m "Accept error: %s" (Printexc.to_string exn))
    done;
    Log.info (fun m -> m "HTTP server stopped")
  )

let stop server =
  Log.info (fun m -> m "Stopping HTTP server");
  server.running <- false;
  Unix.close server.socket

let is_running server =
  server.running
