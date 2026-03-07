(** ACP transport helpers.

    ACP permits custom transports, so we use newline-delimited JSON-RPC over TCP
    for the local/remote `host:port` endpoint. *)

module Log = (val Logs.src_log (Logs.Src.create "acp_transport") : Logs.LOG)

type endpoint = {
  host : string;
  port : int;
}

type connection = {
  fd : Unix.file_descr;
  ic : in_channel;
  oc : out_channel;
  write_mutex : Mutex.t;
  next_id : int Atomic.t;
}

let default_endpoint = {
  host = "127.0.0.1";
  port = 9102;
}

let endpoint_to_string endpoint =
  Printf.sprintf "%s:%d" endpoint.host endpoint.port

let parse_endpoint raw =
  let raw = String.trim raw in
  match String.rindex_opt raw ':' with
  | None -> Error "endpoint must be in host:port form"
  | Some idx ->
      let host = String.sub raw 0 idx |> String.trim in
      let port_s = String.sub raw (idx + 1) (String.length raw - idx - 1) |> String.trim in
      if host = "" then Error "endpoint host is required"
      else
        match int_of_string_opt port_s with
        | None -> Error "endpoint port must be an integer"
        | Some port when port <= 0 || port > 65535 -> Error "endpoint port must be between 1 and 65535"
        | Some port -> Ok { host; port }

let resolve_host host =
  try Unix.inet_addr_of_string host
  with _ ->
    let entry = Unix.gethostbyname host in
    if Array.length entry.Unix.h_addr_list = 0 then
      failwith ("Unable to resolve host: " ^ host)
    else
      entry.Unix.h_addr_list.(0)

let create_connection fd =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr (Unix.dup fd) in
  {
    fd;
    ic;
    oc;
    write_mutex = Mutex.create ();
    next_id = Atomic.make 1;
  }

let connect endpoint =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  try
    let addr = Unix.ADDR_INET (resolve_host endpoint.host, endpoint.port) in
    Unix.connect fd addr;
    create_connection fd
  with exn ->
    (try Unix.close fd with _ -> ());
    raise exn

let next_request_id conn =
  Atomic.fetch_and_add conn.next_id 1

let send_json conn json =
  let payload = Yojson.Safe.to_string json in
  Mutex.lock conn.write_mutex;
  try
    output_string conn.oc payload;
    output_char conn.oc '\n';
    flush conn.oc;
    Mutex.unlock conn.write_mutex
  with exn ->
    Mutex.unlock conn.write_mutex;
    raise exn

let recv_json conn =
  input_line conn.ic
  |> Yojson.Safe.from_string

let close conn =
  (try close_in_noerr conn.ic with _ -> ());
  (try close_out_noerr conn.oc with _ -> ());
  (try Unix.close conn.fd with _ -> ());
  Log.debug (fun m -> m "Closed ACP transport connection")
