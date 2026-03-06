(** Task API types and JSON helpers *)

open Protocol_conv_json
open Yojson.Safe
open Yojson.Safe.Util

type task = {
  id : string;
  kind : string;
  title : string;
  description : string;
  status : string;
  priority : int;
  assignee : string option;
  created_by : string option;
  created_at : float;
  updated_at : float;
  started_at : float option;
  closed_at : float option;
  close_reason : string option;
  result : string option;
  error : string option;
  parent_id : string option;
  session_id : string option;
}
[@@deriving protocol ~driver:(module Json)]

type dependency = {
  from_task_id : string;
  to_task_id : string;
  dep_type : string;
}
[@@deriving protocol ~driver:(module Json)]

type task_event = {
  seq : int;
  task_id : string;
  event_type : string;
  status : string option;
  actor : string;
  payload_json : string;
  created_at : float;
}
[@@deriving protocol ~driver:(module Json)]

type task_create_request = {
  kind : string option;
  title : string;
  description : string option;
  status : string option;
  priority : int option;
  assignee : string option;
  created_by : string option;
  parent_id : string option;
  session_id : string option;
}
[@@deriving protocol ~driver:(module Json)]

type task_update_request = {
  title : string option;
  description : string option;
  status : string option;
  priority : int option;
  assignee : string option;
  close_reason : string option;
  result : string option;
  error : string option;
}
[@@deriving protocol ~driver:(module Json)]

type task_list_request = {
  status : string option;
  assignee : string option;
  kind : string option;
  limit : int option;
  cursor : float option;
}
[@@deriving protocol ~driver:(module Json)]

let valid_statuses = ["open"; "in_progress"; "blocked"; "deferred"; "closed"; "failed"; "canceled"]
let terminal_statuses = ["closed"; "failed"; "canceled"]
let valid_dep_types = ["blocks"; "related"; "parent_child"; "discovered_from"]

let now () = Unix.gettimeofday ()

let normalize_status status =
  String.lowercase_ascii (String.trim status)

let is_valid_status status =
  List.exists (String.equal (normalize_status status)) valid_statuses

let is_terminal_status status =
  List.exists (String.equal (normalize_status status)) terminal_statuses

let normalize_dep_type dep_type =
  String.lowercase_ascii (String.trim dep_type)

let is_valid_dep_type dep_type =
  List.exists (String.equal (normalize_dep_type dep_type)) valid_dep_types

let make_task_id () =
  Random.self_init ();
  let a = Random.bits () in
  let b = Random.bits () in
  Printf.sprintf "tsk-%08x%08x" a b

let opt_assoc_string fields key =
  match List.assoc_opt key fields with
  | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let opt_assoc_int fields key =
  match List.assoc_opt key fields with
  | Some (`Int i) -> Some i
  | Some (`Intlit s) -> (try Some (int_of_string s) with _ -> None)
  | Some (`String s) -> (try Some (int_of_string (String.trim s)) with _ -> None)
  | _ -> None

let opt_assoc_float fields key =
  match List.assoc_opt key fields with
  | Some (`Float f) -> Some f
  | Some (`Int i) -> Some (float_of_int i)
  | Some (`String s) -> (try Some (float_of_string (String.trim s)) with _ -> None)
  | _ -> None

let parse_create_request = function
  | `Assoc fields ->
      let title = Option.value ~default:"" (opt_assoc_string fields "title") in
      if title = "" then Error "title is required"
      else
        Ok {
          kind = opt_assoc_string fields "kind";
          title;
          description = opt_assoc_string fields "description";
          status = opt_assoc_string fields "status";
          priority = opt_assoc_int fields "priority";
          assignee = opt_assoc_string fields "assignee";
          created_by = opt_assoc_string fields "created_by";
          parent_id = opt_assoc_string fields "parent_id";
          session_id = opt_assoc_string fields "session_id";
        }
  | _ -> Error "Expected JSON object"

let parse_update_request = function
  | `Assoc fields ->
      Ok {
        title = opt_assoc_string fields "title";
        description = opt_assoc_string fields "description";
        status = opt_assoc_string fields "status";
        priority = opt_assoc_int fields "priority";
        assignee = opt_assoc_string fields "assignee";
        close_reason = opt_assoc_string fields "close_reason";
        result = opt_assoc_string fields "result";
        error = opt_assoc_string fields "error";
      }
  | _ -> Error "Expected JSON object"

let parse_list_request ?status ?assignee ?kind ?limit ?cursor () =
  {
    status;
    assignee;
    kind;
    limit;
    cursor;
  }

let task_to_json (t : task) : Yojson.Safe.t =
  `Assoc [
    ("id", `String t.id);
    ("kind", `String t.kind);
    ("title", `String t.title);
    ("description", `String t.description);
    ("status", `String t.status);
    ("priority", `Int t.priority);
    ("assignee", match t.assignee with Some s -> `String s | None -> `Null);
    ("created_by", match t.created_by with Some s -> `String s | None -> `Null);
    ("created_at", `Float t.created_at);
    ("updated_at", `Float t.updated_at);
    ("started_at", match t.started_at with Some s -> `Float s | None -> `Null);
    ("closed_at", match t.closed_at with Some s -> `Float s | None -> `Null);
    ("close_reason", match t.close_reason with Some s -> `String s | None -> `Null);
    ("result", match t.result with Some s -> `String s | None -> `Null);
    ("error", match t.error with Some s -> `String s | None -> `Null);
    ("parent_id", match t.parent_id with Some s -> `String s | None -> `Null);
    ("session_id", match t.session_id with Some s -> `String s | None -> `Null);
  ]

let dependency_to_json (d : dependency) : Yojson.Safe.t =
  `Assoc [
    ("from_task_id", `String d.from_task_id);
    ("to_task_id", `String d.to_task_id);
    ("dep_type", `String d.dep_type);
  ]

let event_to_json (e : task_event) : Yojson.Safe.t =
  `Assoc [
    ("seq", `Int e.seq);
    ("task_id", `String e.task_id);
    ("event_type", `String e.event_type);
    ("status", match e.status with Some s -> `String s | None -> `Null);
    ("actor", `String e.actor);
    ("payload_json", `String e.payload_json);
    ("created_at", `Float e.created_at);
  ]

let parse_dependency_request = function
  | `Assoc fields ->
      let from_task_id = Option.value ~default:"" (opt_assoc_string fields "from_task_id") in
      let dep_type = Option.value ~default:"blocks" (opt_assoc_string fields "dep_type") in
      if from_task_id = "" then Error "from_task_id is required"
      else Ok (from_task_id, dep_type)
  | _ -> Error "Expected JSON object"

let parse_actor = function
  | `Assoc fields -> opt_assoc_string fields "actor"
  | _ -> None

let parse_close_reason = function
  | `Assoc fields -> opt_assoc_string fields "close_reason"
  | _ -> None

let parse_limit ?(default=50) v =
  match v with
  | Some n when n > 0 -> n
  | _ -> default

let parse_after_seq v =
  match v with
  | Some n when n >= 0 -> Some n
  | _ -> None

let list_response_json tasks =
  `Assoc [
    ("count", `Int (List.length tasks));
    ("tasks", `List (List.map task_to_json tasks));
  ]

let dependencies_response_json task_id deps =
  `Assoc [
    ("task_id", `String task_id);
    ("count", `Int (List.length deps));
    ("dependencies", `List (List.map dependency_to_json deps));
  ]

let events_response_json task_id events =
  `Assoc [
    ("task_id", `String task_id);
    ("count", `Int (List.length events));
    ("events", `List (List.map event_to_json events));
  ]
