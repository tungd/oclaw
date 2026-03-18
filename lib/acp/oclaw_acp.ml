(** Agent Client Protocol implementation. *)

open Yojson.Safe

module Json_rpc = struct
  type id = [ `Int of int | `String of string | `Null ]

  type error = {
    code : int;
    message : string;
    data : Yojson.Safe.t option;
  }

  type request = {
    id : id;
    method_name : string;
    params : Yojson.Safe.t;
  }

  type response = {
    id : id;
    result : (Yojson.Safe.t, error) result;
  }

  type notification = {
    method_name : string;
    params : Yojson.Safe.t;
  }

  type message =
    | Request of request
    | Response of response
    | Notification of notification

  let id_to_yojson = function
    | `Int i -> `Int i
    | `String s -> `String s
    | `Null -> `Null

  let id_of_yojson = function
    | `Int i -> Some (`Int i)
    | `String s -> Some (`String s)
    | `Null -> Some `Null
    | _ -> None

  let error_to_yojson e =
    let fields = [
      ("code", `Int e.code);
      ("message", `String e.message);
    ] in
    let fields = match e.data with
      | Some d -> ("data", d) :: fields
      | None -> fields
    in
    `Assoc fields

  let error_of_yojson = function
    | `Assoc fields ->
        let code = match List.assoc_opt "code" fields with
          | Some (`Int i) -> i
          | _ -> 0
        in
        let message = match List.assoc_opt "message" fields with
          | Some (`String s) -> s
          | _ -> "Unknown error"
        in
        let data = List.assoc_opt "data" fields in
        Some { code; message; data }
    | _ -> None

  let to_yojson = function
    | Request r ->
        `Assoc [
          ("jsonrpc", `String "2.0");
          ("id", id_to_yojson r.id);
          ("method", `String r.method_name);
          ("params", r.params);
        ]
    | Response r ->
        let fields = [("jsonrpc", `String "2.0"); ("id", id_to_yojson r.id)] in
        let fields = match r.result with
          | Ok res -> ("result", res) :: fields
          | Error e -> ("error", error_to_yojson e) :: fields
        in
        `Assoc fields
    | Notification n ->
        `Assoc [
          ("jsonrpc", `String "2.0");
          ("method", `String n.method_name);
          ("params", n.params);
        ]

  let of_yojson = function
    | `Assoc fields ->
        let jsonrpc = match List.assoc_opt "jsonrpc" fields with
          | Some (`String "2.0") -> true
          | _ -> false
        in
        if not jsonrpc then None
        else
          let id = match List.assoc_opt "id" fields with
            | Some v -> id_of_yojson v
            | None -> None
          in
          let method_name = match List.assoc_opt "method" fields with
            | Some (`String s) -> Some s
            | _ -> None
          in
          (match id, method_name with
           | Some id, Some method_name ->
               let params = List.assoc_opt "params" fields |> Option.value ~default:`Null in
               Some (Request { id; method_name; params })
           | Some id, None ->
               (match List.assoc_opt "result" fields with
                | Some res -> Some (Response { id; result = Ok res })
                | None ->
                    match (match List.assoc_opt "error" fields with Some v -> error_of_yojson v | None -> None) with
                    | Some e -> Some (Response { id; result = Error e })
                    | None -> None)
           | None, Some method_name ->
               let params = List.assoc_opt "params" fields |> Option.value ~default:`Null in
               Some (Notification { method_name; params })
           | None, None -> None)
    | _ -> None
end

module Message = struct
  module Method = struct
    let initialize = "initialize"
    let initialized = "initialized"
    let tools_list = "tools/list"
    let tools_call = "tools/call"
    let agent_plan = "agent/plan"
    let agent_message = "agent/message"
    let agent_delta = "agent/delta"
    let status = "status"
  end

  type t =
    | Initialize of { capabilities : Yojson.Safe.t }
    | Initialized
    | Agent_message of { content : string; chat_id : int option }
    | Agent_plan of { steps : string list }
    | Agent_delta of { content : string }
    | Tool_call of { name : string; arguments : Yojson.Safe.t }
    | Tool_result of { name : string; content : string; is_error : bool }
    | Status of { status : string; message : string option }
    | Error of { message : string; code : int }
    | Done

  let to_json_rpc ~id t =
    match t with
    | Initialize { capabilities } ->
        Json_rpc.Request { id; method_name = Method.initialize; params = `Assoc [("capabilities", capabilities)] }
    | Initialized ->
        Json_rpc.Notification { method_name = Method.initialized; params = `Null }
    | Agent_message { content; chat_id } ->
        let params = [("content", `String content)] in
        let params = match chat_id with
          | Some cid -> ("chat_id", `Int cid) :: params
          | None -> params
        in
        Json_rpc.Request { id; method_name = Method.agent_message; params = `Assoc params }
    | Agent_plan { steps } ->
        Json_rpc.Notification { method_name = Method.agent_plan; params = `Assoc [("steps", `List (List.map (fun s -> `String s) steps))] }
    | Agent_delta { content } ->
        Json_rpc.Notification { method_name = Method.agent_delta; params = `Assoc [("content", `String content)] }
    | Tool_call { name; arguments } ->
        Json_rpc.Request { id; method_name = Method.tools_call; params = `Assoc [("name", `String name); ("arguments", arguments)] }
    | Tool_result { name; content; is_error } ->
        Json_rpc.Response { id; result = Ok (`Assoc [("name", `String name); ("content", `String content); ("is_error", `Bool is_error)]) }
    | Status { status; message } ->
        let params = [("status", `String status)] in
        let params = match message with
          | Some m -> ("message", `String m) :: params
          | None -> params
        in
        Json_rpc.Notification { method_name = Method.status; params = `Assoc params }
    | Error { message; code } ->
        Json_rpc.Response { id; result = Error { code; message; data = None } }
    | Done ->
        Json_rpc.Notification { method_name = "done"; params = `Null }

  let of_json_rpc m =
    match m with
    | Json_rpc.Request { method_name; params; _ } ->
        (match method_name, params with
         | "initialize", `Assoc fields ->
             let capabilities = List.assoc_opt "capabilities" fields |> Option.value ~default:`Null in
             Some (Initialize { capabilities })
         | "agent/message", `Assoc fields ->
             let content = match List.assoc_opt "content" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             let chat_id = match List.assoc_opt "chat_id" fields with
               | Some (`Int i) -> Some i
               | _ -> None
             in
             Some (Agent_message { content; chat_id })
         | "tools/call", `Assoc fields ->
             let name = match List.assoc_opt "name" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             let arguments = List.assoc_opt "arguments" fields |> Option.value ~default:`Null in
             Some (Tool_call { name; arguments })
         | _ -> None)
    | Json_rpc.Notification { method_name; params } ->
        (match method_name, params with
         | "initialized", _ -> Some Initialized
         | "agent/plan", `Assoc fields ->
             let steps = match List.assoc_opt "steps" fields with
               | Some (`List l) -> List.filter_map (function `String s -> Some s | _ -> None) l
               | _ -> []
             in
             Some (Agent_plan { steps })
         | "agent/delta", `Assoc fields ->
             let content = match List.assoc_opt "content" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             Some (Agent_delta { content })
         | "status", `Assoc fields ->
             let status = match List.assoc_opt "status" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             let message = match List.assoc_opt "message" fields with
               | Some (`String s) -> Some s
               | _ -> None
             in
             Some (Status { status; message })
         | "done", _ -> Some Done
         | _ -> None)
    | Json_rpc.Response { result; _ } ->
        (match result with
         | Ok (`Assoc fields) ->
             let name = match List.assoc_opt "name" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             let content = match List.assoc_opt "content" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             let is_error = match List.assoc_opt "is_error" fields with
               | Some (`Bool b) -> b
               | _ -> false
             in
             Some (Tool_result { name; content; is_error })
         | Error e -> Some (Error { message = e.message; code = e.code })
         | _ -> None)
end

module Channel = struct
  type 'a t = {
    mutex : Mutex.t;
    condition : Condition.t;
    queue : 'a Queue.t;
  }

  let create () = {
    mutex = Mutex.create ();
    condition = Condition.create ();
    queue = Queue.create ();
  }

  let send chan msg =
    Mutex.lock chan.mutex;
    Queue.add msg chan.queue;
    Condition.signal chan.condition;
    Mutex.unlock chan.mutex

  let recv chan =
    Mutex.lock chan.mutex;
    while Queue.is_empty chan.queue do
      Condition.wait chan.condition chan.mutex
    done;
    let msg = Queue.take chan.queue in
    Mutex.unlock chan.mutex;
    msg

  let try_recv chan =
    Mutex.lock chan.mutex;
    let result =
      if Queue.is_empty chan.queue then None
      else Some (Queue.take chan.queue)
    in
    Mutex.unlock chan.mutex;
    result

  let recv_timeout chan timeout =
    Mutex.lock chan.mutex;
    let deadline = Unix.gettimeofday () +. timeout in
    let rec wait () =
      if not (Queue.is_empty chan.queue) then
        Some (Queue.take chan.queue)
      else
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then None
        else begin
          Condition.wait chan.condition chan.mutex;
          let now = Unix.gettimeofday () in
          if now >= deadline then None
          else wait ()
        end
    in
    let result = wait () in
    Mutex.unlock chan.mutex;
    result
end

module Stdio_frontend = struct
  type config = {
    input : in_channel;
    output : out_channel;
  }

  type t = {
    input : in_channel;
    output : out_channel;
    output_mutex : Mutex.t;
  }

  let create ?(config = { input = stdin; output = stdout }) () = {
    input = config.input;
    output = config.output;
    output_mutex = Mutex.create ();
  }

  let send t msg =
    Mutex.lock t.output_mutex;
    output_string t.output (Yojson.Safe.to_string (Json_rpc.to_yojson msg));
    output_char t.output '\n';
    flush t.output;
    Mutex.unlock t.output_mutex

  let recv t =
    try
      let line = input_line t.input in
      if String.trim line = "" then None
      else Json_rpc.of_yojson (Yojson.Safe.from_string line)
    with _ -> None

  let recv_timeout t timeout =
    let fd = Unix.descr_of_in_channel t.input in
    let readable, _, _ = Unix.select [fd] [] [] timeout in
    if readable = [] then None
    else recv t

  let close t =
    Mutex.lock t.output_mutex;
    flush t.output;
    Mutex.unlock t.output_mutex
end
