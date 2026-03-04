(** HTTP server for OClaw using h1 + iomux *)

module Log = (val Logs.src_log (Logs.Src.create "http_server") : Logs.LOG)

module Method = H1.Method
module Status = H1.Status
module Headers = H1.Headers
module Request = H1.Request
module Response = H1.Response
module Reqd = H1.Reqd
module Body = H1.Body

module Poll = Iomux.Poll
module Poll_flags = Poll.Flags

type route_match = Exact | Prefix | Wildcard

type handler = Reqd.t -> unit

type route = {
  method_ : Method.t option;
  pattern : string;
  match_type : route_match;
  handler : handler;
}

type read_body_error =
  [ `Too_large of int
  | `Exception of exn
  ]

type async_response = {
  status : Status.t;
  headers : (string * string) list;
  body : string;
}

module Async_response = struct
  type t = async_response

  let text ?(status=`OK) ?(headers=[]) body =
    { status; headers; body }

  let json ?(status=`OK) ?(headers=[]) data =
    let body = Yojson.Safe.to_string data in
    { status; headers = ("content-type", "application/json") :: headers; body }
end

type connection = {
  fd : Unix.file_descr;
  h1 : H1.Server_connection.t;
  read_buffer : Bstr.t;
  mutable want_read : bool;
  mutable want_write : bool;
  mutable closed : bool;
  mutable last_active : float;
}

type work_item = {
  id : int;
  run : unit -> async_response;
}

type work_result = {
  id : int;
  result : (async_response, exn) result;
}

type worker_pool = {
  jobs : work_item Queue.t;
  job_mutex : Mutex.t;
  job_cond : Condition.t;
  completions : work_result Queue.t;
  completion_mutex : Mutex.t;
  mutable workers : unit Domain.t array;
  running : bool Atomic.t;
  max_pending_jobs : int;
}

type t = {
  host : string;
  port : int;
  max_connections : int;
  idle_timeout_s : float;
  mutable routes : route list;
  mutex : Mutex.t;
  running : bool Atomic.t;
  mutable listener : Unix.file_descr option;
  mutable wake_read : Unix.file_descr option;
  mutable wake_write : Unix.file_descr option;
  mutable loop_domain : unit Domain.t option;
  conns : (Unix.file_descr, connection) Hashtbl.t;
  pending_jobs : (int, Reqd.t * connection) Hashtbl.t;
  mutable next_job_id : int;
  worker_pool : worker_pool;
}

let active_server : t option Atomic.t = Atomic.make None
let current_connection : connection option ref = ref None

let wake_byte = Bytes.make 1 '\x00'

let has_ci_header name headers =
  let lname = String.lowercase_ascii name in
  List.exists (fun (k, _v) -> String.equal (String.lowercase_ascii k) lname) headers

let with_default_headers headers body =
  let headers =
    if has_ci_header "server" headers then headers
    else ("server", "OClaw/1.0") :: headers
  in
  if has_ci_header "content-length" headers then headers
  else ("content-length", string_of_int (String.length body)) :: headers

let signal_wakeup server =
  let wake_write =
    Mutex.lock server.mutex;
    let fd = server.wake_write in
    Mutex.unlock server.mutex;
    fd
  in
  match wake_write with
  | None -> ()
  | Some fd ->
      (try
         ignore (Unix.write fd wake_byte 0 1)
       with
       | Unix.Unix_error (code, _, _) when code = Unix.EAGAIN || code = Unix.EWOULDBLOCK -> ()
       | _ -> ())

let mark_reqd_writable reqd =
  let _ = reqd in
  match Atomic.get active_server, !current_connection with
  | Some server, Some conn ->
      conn.want_write <- true;
      conn.last_active <- Unix.gettimeofday ();
      signal_wakeup server
  | _ -> ()

let with_current_connection conn f =
  let prev = !current_connection in
  current_connection := Some conn;
  Fun.protect ~finally:(fun () -> current_connection := prev) f

let respond_text ?(status=`OK) ?(headers=[]) reqd text =
  let headers = with_default_headers headers text in
  let response = Response.create ~headers:(Headers.of_list headers) status in
  Reqd.respond_with_string reqd response text;
  mark_reqd_writable reqd

let respond_json ?(status=`OK) ?(headers=[]) reqd data =
  let body = Yojson.Safe.to_string data in
  let headers =
    if has_ci_header "content-type" headers then headers
    else ("content-type", "application/json") :: headers
  in
  respond_text ~status ~headers reqd body

let read_body ?(max_bytes=1_048_576) body callback =
  let done_ = ref false in
  let total = ref 0 in
  let buf = Buffer.create 1024 in
  let finish value =
    if not !done_ then begin
      done_ := true;
      callback value
    end
  in
  let rec schedule () =
    Body.Reader.schedule_read body
      ~on_eof:(fun () -> finish (Ok (Buffer.contents buf)))
      ~on_read:(fun chunk ~off ~len ->
        if !done_ then ()
        else
          try
            let new_total = !total + len in
            if new_total > max_bytes then begin
              Body.Reader.close body;
              finish (Error (`Too_large max_bytes))
            end else begin
              total := new_total;
              Buffer.add_string buf (Bstr.sub_string chunk ~off ~len);
              schedule ()
            end
          with exn ->
            Body.Reader.close body;
            finish (Error (`Exception exn)))
  in
  schedule ()

let path_matches pattern path match_type =
  match match_type with
  | Exact -> String.equal pattern path
  | Prefix -> String.starts_with ~prefix:pattern path
  | Wildcard -> true

let normalize_target target =
  if String.length target = 0 then "/"
  else if target.[0] = '/' then target
  else
    match String.index_opt target '/' with
    | None -> "/"
    | Some idx -> String.sub target idx (String.length target - idx)

let split_path_query target =
  let target = normalize_target target in
  match String.index_opt target '?' with
  | None -> (target, "")
  | Some idx ->
      let path = String.sub target 0 idx in
      let query = String.sub target (idx + 1) (String.length target - idx - 1) in
      (path, query)

let find_route routes meth path =
  let rec go = function
    | [] -> None
    | route :: rest ->
        let method_ok =
          match route.method_ with
          | None -> true
          | Some expected -> expected = meth
        in
        if method_ok && path_matches route.pattern path route.match_type then
          Some route.handler
        else go rest
  in
  go routes

let respond_async reqd (resp : async_response) =
  respond_text ~status:resp.status ~headers:resp.headers reqd resp.body

let submit_job server reqd (run : unit -> async_response) =
  let pool = server.worker_pool in
  let conn = !current_connection in
  Mutex.lock pool.job_mutex;
  let pending = Queue.length pool.jobs in
  if conn = None || not (Atomic.get pool.running) || pending >= pool.max_pending_jobs then begin
    Mutex.unlock pool.job_mutex;
    respond_text ~status:`Service_unavailable reqd "Server busy"
  end else begin
    let id = server.next_job_id in
    server.next_job_id <- server.next_job_id + 1;
    Hashtbl.replace server.pending_jobs id (reqd, Option.get conn);
    Queue.add { id; run } pool.jobs;
    Condition.signal pool.job_cond;
    Mutex.unlock pool.job_mutex
  end

let rec worker_loop server () =
  let pool = server.worker_pool in
  let next_item () =
    Mutex.lock pool.job_mutex;
    while Atomic.get pool.running && Queue.is_empty pool.jobs do
      Condition.wait pool.job_cond pool.job_mutex
    done;
    let item =
      if Queue.is_empty pool.jobs then None
      else Some (Queue.take pool.jobs)
    in
    Mutex.unlock pool.job_mutex;
    item
  in
  match next_item () with
  | None -> ()
  | Some item ->
      let result =
        try Ok (item.run ())
        with exn -> Error exn
      in
      Mutex.lock pool.completion_mutex;
      Queue.add { id = item.id; result } pool.completions;
      Mutex.unlock pool.completion_mutex;
      signal_wakeup server;
      worker_loop server ()

let start_workers server =
  let pool = server.worker_pool in
  Atomic.set pool.running true;
  let count = max 2 (Domain.recommended_domain_count () - 1) in
  pool.workers <- Array.init count (fun _ -> Domain.spawn (worker_loop server))

let stop_workers server =
  let pool = server.worker_pool in
  Atomic.set pool.running false;
  Mutex.lock pool.job_mutex;
  Condition.broadcast pool.job_cond;
  Mutex.unlock pool.job_mutex;
  Array.iter Domain.join pool.workers;
  pool.workers <- [||]

let remove_pending_jobs_for_conn server conn =
  let doomed_job_ids = ref [] in
  Hashtbl.iter (fun id (_reqd, mapped_conn) ->
    if mapped_conn == conn then
      doomed_job_ids := id :: !doomed_job_ids
  ) server.pending_jobs;
  List.iter (fun id -> Hashtbl.remove server.pending_jobs id) !doomed_job_ids

let close_connection server conn =
  if not conn.closed then conn.closed <- true;
  remove_pending_jobs_for_conn server conn;
  Hashtbl.remove server.conns conn.fd;
  (try Unix.close conn.fd with _ -> ())

let handle_request server conn reqd =
  let request = Reqd.request reqd in
  let path, _query = split_path_query request.target in
  let routes =
    Mutex.lock server.mutex;
    let routes = server.routes in
    Mutex.unlock server.mutex;
    routes
  in
  match find_route routes request.meth path with
  | None ->
      (try with_current_connection conn (fun () -> respond_text ~status:`Not_found reqd "Not found")
       with exn ->
         Log.err (fun m -> m "404 response error: %s" (Printexc.to_string exn)))
  | Some route_handler ->
      (try with_current_connection conn (fun () -> route_handler reqd)
       with exn ->
         Log.err (fun m -> m "Route handler error: %s" (Printexc.to_string exn));
         with_current_connection conn (fun () -> respond_text ~status:`Internal_server_error reqd "Internal Server Error"))

let is_flag_set flags value = Poll_flags.mem flags value

let write_iovecs fd iovecs =
  let rec go total = function
    | [] -> `Ok total
    | { H1.IOVec.buffer; off; len } :: rest ->
        if len = 0 then go total rest
        else begin
          let chunk = Bstr.sub_string buffer ~off ~len in
          try
            let n = Unix.single_write_substring fd chunk 0 len in
            if n = 0 then
              if total = 0 then `Would_block else `Ok total
            else if n < len then `Ok (total + n)
            else go (total + n) rest
          with
          | Unix.Unix_error (code, _, _) when code = Unix.EAGAIN || code = Unix.EWOULDBLOCK ->
              if total = 0 then `Would_block else `Ok total
          | Unix.Unix_error (code, _, _) when code = Unix.EPIPE || code = Unix.ECONNRESET ->
              `Closed
          | _ -> `Closed
        end
  in
  go 0 iovecs

let handle_readable server conn =
  let buf = Bytes.create 4096 in
  let rec loop () =
    match H1.Server_connection.next_read_operation conn.h1 with
    | `Read ->
        begin
          try
            let n = Unix.read conn.fd buf 0 (Bytes.length buf) in
            if n = 0 then begin
              ignore (with_current_connection conn (fun () ->
                H1.Server_connection.read_eof conn.h1 conn.read_buffer ~off:0 ~len:0));
              conn.closed <- true
            end else begin
              Bstr.blit_from_bytes buf ~src_off:0 conn.read_buffer ~dst_off:0 ~len:n;
              ignore (with_current_connection conn (fun () ->
                H1.Server_connection.read conn.h1 conn.read_buffer ~off:0 ~len:n));
              conn.last_active <- Unix.gettimeofday ();
              loop ()
            end
          with
          | Unix.Unix_error (code, _, _) when code = Unix.EAGAIN || code = Unix.EWOULDBLOCK ->
              ()
          | exn ->
              H1.Server_connection.report_exn conn.h1 exn;
              conn.closed <- true
        end
    | `Yield ->
        conn.want_read <- false;
        H1.Server_connection.yield_reader conn.h1 (fun () ->
          conn.want_read <- true;
          signal_wakeup server)
    | `Close ->
        conn.closed <- true
    | `Upgrade ->
        conn.closed <- true
  in
  loop ()

let handle_writable server conn =
  let rec loop () =
    match H1.Server_connection.next_write_operation conn.h1 with
    | `Write iovecs ->
        begin
          match write_iovecs conn.fd iovecs with
          | `Closed ->
              H1.Server_connection.report_write_result conn.h1 `Closed;
              conn.closed <- true
          | `Would_block ->
              ()
          | `Ok written ->
              if written > 0 then begin
                H1.Server_connection.report_write_result conn.h1 (`Ok written);
                conn.last_active <- Unix.gettimeofday ();
                loop ()
              end
        end
    | `Yield ->
        conn.want_write <- false;
        H1.Server_connection.yield_writer conn.h1 (fun () ->
          conn.want_write <- true;
          signal_wakeup server)
    | `Close _ ->
        conn.closed <- true
    | `Upgrade ->
        conn.closed <- true
  in
  loop ()

let close_idle_connections server now =
  let to_close = ref [] in
  Hashtbl.iter (fun _fd conn ->
    if now -. conn.last_active > server.idle_timeout_s then
      to_close := conn :: !to_close
  ) server.conns;
  List.iter (fun conn ->
    Log.debug (fun m -> m "Closing idle connection");
    close_connection server conn
  ) !to_close

let drain_wakeup_fd fd =
  let tmp = Bytes.create 64 in
  let rec loop () =
    try
      let n = Unix.read fd tmp 0 (Bytes.length tmp) in
      if n > 0 then loop ()
    with
    | Unix.Unix_error (code, _, _) when code = Unix.EAGAIN || code = Unix.EWOULDBLOCK ->
        ()
  in
  loop ()

let process_completions server =
  let pool = server.worker_pool in
  let rec pop_all acc =
    Mutex.lock pool.completion_mutex;
    let item =
      if Queue.is_empty pool.completions then None
      else Some (Queue.take pool.completions)
    in
    Mutex.unlock pool.completion_mutex;
    match item with
    | None -> List.rev acc
    | Some x -> pop_all (x :: acc)
  in
  let completions = pop_all [] in
  List.iter (fun { id; result } ->
    match Hashtbl.find_opt server.pending_jobs id with
    | None -> ()
    | Some (reqd, conn) ->
        Hashtbl.remove server.pending_jobs id;
        if Hashtbl.mem server.conns conn.fd && not conn.closed then
          with_current_connection conn (fun () ->
            match result with
            | Ok resp ->
                (try respond_async reqd resp
                 with exn ->
                   Log.err (fun m -> m "Failed to write async response: %s" (Printexc.to_string exn)))
            | Error exn ->
                (try
                   respond_text ~status:`Internal_server_error reqd
                     ("Internal error: " ^ Printexc.to_string exn)
                 with _ -> ()))) completions

let accept_all server listener =
  let rec loop () =
    try
      let client_fd, _addr = Unix.accept listener in
      Unix.set_nonblock client_fd;
      if Hashtbl.length server.conns >= server.max_connections then begin
        (try Unix.close client_fd with _ -> ());
        loop ()
      end else begin
        let conn_ref = ref None in
        let request_handler reqd =
          match !conn_ref with
          | None -> ()
          | Some conn -> handle_request server conn reqd
        in
        let error_handler ?request:_ error start_response =
          let status, message =
            match error with
            | `Bad_request -> (`Bad_request, "Bad Request")
            | `Bad_gateway -> (`Bad_gateway, "Bad Gateway")
            | `Internal_server_error -> (`Internal_server_error, "Internal Server Error")
            | `Exn _ -> (`Bad_request, "Bad Request")
          in
          let headers = Headers.of_list [
            ("content-type", "text/plain; charset=utf-8");
            ("server", "OClaw/1.0");
          ] in
          let body = start_response headers in
          Body.Writer.write_string body message;
          Body.Writer.close body;
          (match !conn_ref with
           | None -> ()
           | Some conn ->
               conn.want_write <- true;
               signal_wakeup server)
        in
        let h1 = H1.Server_connection.create ~error_handler request_handler in
        let conn = {
          fd = client_fd;
          h1;
          read_buffer = Bstr.create 4096;
          want_read = true;
          want_write = false;
          closed = false;
          last_active = Unix.gettimeofday ();
        } in
        conn_ref := Some conn;
        Hashtbl.replace server.conns client_fd conn;
        loop ()
      end
    with
    | Unix.Unix_error (code, _, _) when code = Unix.EAGAIN || code = Unix.EWOULDBLOCK ->
        ()
  in
  loop ()

let poll_loop server listener wake_read =
  let poller = Poll.create ~maxfds:(2 + server.max_connections) () in
  let progress_connections () =
    let conn_snapshot = Hashtbl.fold (fun _fd conn acc -> conn :: acc) server.conns [] in
    List.iter (fun conn ->
      if conn.closed then close_connection server conn
      else begin
        if conn.want_read then handle_readable server conn;
        if conn.want_write then handle_writable server conn;
        if conn.closed then close_connection server conn
      end
    ) conn_snapshot
  in
  let rec loop () =
    (* Non-blocking accept pass so incoming sockets are not starved if the poll
       backend doesn't surface listener readability on this platform. *)
    accept_all server listener;
    process_completions server;
    progress_connections ();

    let conn_list = Hashtbl.fold (fun _fd conn acc -> conn :: acc) server.conns [] in
    Poll.set_index poller 0 listener Poll_flags.pollin;
    Poll.set_index poller 1 wake_read Poll_flags.pollin;

    let idx_to_conn = Hashtbl.create (List.length conn_list + 2) in
    let index = ref 2 in
    List.iter (fun conn ->
      if conn.closed then ()
      else begin
        let flags =
          let flags = if conn.want_read then Poll_flags.pollin else Poll_flags.empty in
          let flags =
            if conn.want_write then
              let open Poll_flags in
              flags + pollout
            else flags
          in
          flags
        in
        if flags <> Poll_flags.empty then begin
          Poll.set_index poller !index conn.fd flags;
          Hashtbl.replace idx_to_conn !index conn;
          incr index
        end
      end
    ) conn_list;

    let nfds = !index in
    let nready =
      try Poll.poll poller nfds Poll.Nowait
      with exn ->
        Log.err (fun m -> m "Poll error: %s" (Printexc.to_string exn));
        0
    in

    if nready > 0 then
      Poll.iter_ready poller nready (fun idx _fd flags ->
        if idx = 0 then accept_all server listener
        else if idx = 1 then begin
          drain_wakeup_fd wake_read;
          process_completions server
        end else
          match Hashtbl.find_opt idx_to_conn idx with
          | None -> ()
          | Some conn ->
              if is_flag_set flags Poll_flags.pollerr
                 || is_flag_set flags Poll_flags.pollhup
                 || is_flag_set flags Poll_flags.pollnval
              then
                conn.closed <- true
              else begin
                if conn.want_read && is_flag_set flags Poll_flags.pollin then
                  handle_readable server conn;
                if conn.want_write && is_flag_set flags Poll_flags.pollout then
                  handle_writable server conn
              end;
              if conn.closed then close_connection server conn
      );

    (* Opportunistic non-blocking progress pass for read/write state machines. *)
    progress_connections ();

    let now = Unix.gettimeofday () in
    close_idle_connections server now;
    Unix.sleepf 0.01;

    if Atomic.get server.running then loop ()
  in
  try loop ()
  with exn ->
    Log.err (fun m -> m "Server loop crashed: %s" (Printexc.to_string exn))

let close_fd_opt fd_opt =
  match fd_opt with
  | None -> ()
  | Some fd -> (try Unix.close fd with _ -> ())

let create ~host ~port ?(max_connections=100) ?(max_pending_jobs=256) ?(idle_timeout_s=30.0) () =
  let worker_pool = {
    jobs = Queue.create ();
    job_mutex = Mutex.create ();
    job_cond = Condition.create ();
    completions = Queue.create ();
    completion_mutex = Mutex.create ();
    workers = [||];
    running = Atomic.make false;
    max_pending_jobs;
  } in
  {
    host;
    port;
    max_connections;
    idle_timeout_s;
    routes = [];
    mutex = Mutex.create ();
    running = Atomic.make false;
    listener = None;
    wake_read = None;
    wake_write = None;
    loop_domain = None;
    conns = Hashtbl.create max_connections;
    pending_jobs = Hashtbl.create max_pending_jobs;
    next_job_id = 1;
    worker_pool;
  }

let add_route server ~method_ ?(match_type=Exact) pattern handler =
  Mutex.lock server.mutex;
  server.routes <- { method_; pattern; match_type; handler } :: server.routes;
  Mutex.unlock server.mutex

let start server =
  if Atomic.get server.running then begin
    Log.warn (fun m -> m "HTTP server already running");
    match server.loop_domain with
    | Some d -> d
    | None -> failwith "HTTP server running state is inconsistent"
  end else begin
    let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt listener Unix.SO_REUSEADDR true;
    Unix.set_nonblock listener;
    Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_of_string server.host, server.port));
    Unix.listen listener 128;

    let wake_read, wake_write = Unix.pipe () in
    Unix.set_nonblock wake_read;
    Unix.set_nonblock wake_write;

    Mutex.lock server.mutex;
    server.listener <- Some listener;
    server.wake_read <- Some wake_read;
    server.wake_write <- Some wake_write;
    Mutex.unlock server.mutex;

    start_workers server;
    Atomic.set server.running true;
    Atomic.set active_server (Some server);

    let loop = Domain.spawn (fun () ->
      Log.info (fun m -> m "HTTP server started on %s:%d" server.host server.port);
      poll_loop server listener wake_read;
      Hashtbl.iter (fun _fd conn -> close_connection server conn) server.conns;
      Hashtbl.clear server.conns;
      Hashtbl.clear server.pending_jobs;

      Mutex.lock server.mutex;
      close_fd_opt server.listener;
      close_fd_opt server.wake_read;
      close_fd_opt server.wake_write;
      server.listener <- None;
      server.wake_read <- None;
      server.wake_write <- None;
      server.loop_domain <- None;
      Mutex.unlock server.mutex;

      Log.info (fun m -> m "HTTP server loop stopped")
    ) in

    Mutex.lock server.mutex;
    server.loop_domain <- Some loop;
    Mutex.unlock server.mutex;

    loop
  end

let stop server =
  if not (Atomic.get server.running) then
    Log.warn (fun m -> m "HTTP server not running")
  else begin
    Atomic.set server.running false;
    signal_wakeup server;

    let loop_domain =
      Mutex.lock server.mutex;
      let loop = server.loop_domain in
      Mutex.unlock server.mutex;
      loop
    in
    (match loop_domain with
     | None -> ()
     | Some d -> Domain.join d);

    stop_workers server;

    (match Atomic.get active_server with
     | Some active when active == server -> Atomic.set active_server None
     | _ -> ())
  end

let is_running server =
  Atomic.get server.running
