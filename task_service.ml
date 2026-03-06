(** Task service: business API over Task_store *)

open Task_types

module Store = Task_store
module Log = (val Logs.src_log (Logs.Src.create "task_service") : Logs.LOG)

type t = {
  store : Store.t;
}

let active_service : t option ref = ref None
let active_mutex = Mutex.create ()

let create (cfg : Store.config) =
  { store = Store.create cfg }

let set_default (service : t) =
  Mutex.lock active_mutex;
  active_service := Some service;
  Mutex.unlock active_mutex

let get_default () =
  Mutex.lock active_mutex;
  let service = !active_service in
  Mutex.unlock active_mutex;
  service

let initialize service =
  Store.initialize service.store

let create_task service ~(actor : string) (req : task_create_request) =
  let kind = Option.value ~default:"general" req.kind in
  let status = Option.value ~default:"open" req.status |> normalize_status in
  if not (is_valid_status status) then Error (Printf.sprintf "Invalid status: %s" status)
  else
    let priority = Option.value ~default:2 req.priority |> max 0 |> min 4 in
    let description = Option.value ~default:"" req.description in
    Store.insert_task
      service.store
      ~kind
      ~title:req.title
      ~description
      ~status
      ~priority
      ~assignee:req.assignee
      ~created_by:req.created_by
      ~parent_id:req.parent_id
      ~session_id:req.session_id
      ~actor

let get_task service id =
  Store.get_task service.store id

let list_tasks service (req : task_list_request) =
  Store.list_tasks service.store req

let ready_tasks service ?limit () =
  Store.list_ready_tasks service.store ?limit ()

let update_task service ~(id : string) ~(req : task_update_request) ~(actor : string) =
  let requested_status = Option.map normalize_status req.status in
  (match requested_status with
   | Some s when not (is_valid_status s) -> Error (Printf.sprintf "Invalid status: %s" s)
   | _ ->
       let req = { req with status = requested_status } in
       Store.update_task service.store ~id ~req ~actor ~event_type:"updated")

let claim_task service ~(id : string) ~(assignee : string option) ~(actor : string) =
  Store.claim_task service.store ~id ~assignee ~actor

let close_task service ~(id : string) ~(reason : string option) ~(actor : string) =
  Store.close_task service.store ~id ~reason ~actor

let cancel_task service ~(id : string) ~(actor : string) =
  Store.cancel_task service.store ~id ~actor

let complete_task service ~(id : string) ~(result : string option) ~(actor : string) =
  Store.complete_task service.store ~id ~result ~actor

let fail_task service ~(id : string) ~(error : string) ~(actor : string) =
  Store.fail_task service.store ~id ~error ~actor

let add_dependency service ~(from_task_id : string) ~(to_task_id : string) ~(dep_type : string) ~(actor : string) =
  if not (is_valid_dep_type dep_type) then Error (Printf.sprintf "Invalid dependency type: %s" dep_type)
  else Store.add_dependency service.store ~from_task_id ~to_task_id ~dep_type ~actor

let list_dependencies service task_id =
  Store.list_dependencies service.store task_id

let list_events service task_id ?after_seq ?limit () =
  Store.list_events service.store task_id ?after_seq ?limit ()

let task_success_json task =
  `Assoc [
    ("status", `String "ok");
    ("task", task_to_json task);
  ]

let dep_success_json dep =
  `Assoc [
    ("status", `String "ok");
    ("dependency", dependency_to_json dep);
  ]

let list_success_json tasks =
  `Assoc [
    ("status", `String "ok");
    ("count", `Int (List.length tasks));
    ("tasks", `List (List.map task_to_json tasks));
  ]

let events_success_json task_id events =
  `Assoc [
    ("status", `String "ok");
    ("task_id", `String task_id);
    ("count", `Int (List.length events));
    ("events", `List (List.map event_to_json events));
  ]

let deps_success_json task_id deps =
  `Assoc [
    ("status", `String "ok");
    ("task_id", `String task_id);
    ("count", `Int (List.length deps));
    ("dependencies", `List (List.map dependency_to_json deps));
  ]

let with_default_service f =
  match get_default () with
  | Some service -> f service
  | None -> Error "Task service is not initialized"

let dispatch_json service ~operation ~(payload : Yojson.Safe.t) : (Yojson.Safe.t, string) result =
  try
    match operation with
    | "task_create" ->
        (match parse_create_request payload with
         | Error e -> Error e
         | Ok req ->
             let actor = Option.value ~default:"agent" (parse_actor payload) in
             create_task service ~actor req |> Result.map task_success_json)
    | "task_list" ->
        let status =
          match payload with
          | `Assoc fields -> opt_assoc_string fields "status"
          | _ -> None
        in
        let assignee =
          match payload with
          | `Assoc fields -> opt_assoc_string fields "assignee"
          | _ -> None
        in
        let kind =
          match payload with
          | `Assoc fields -> opt_assoc_string fields "kind"
          | _ -> None
        in
        let limit =
          match payload with
          | `Assoc fields -> opt_assoc_int fields "limit"
          | _ -> None
        in
        let cursor =
          match payload with
          | `Assoc fields -> opt_assoc_float fields "cursor"
          | _ -> None
        in
        list_tasks service (parse_list_request ?status ?assignee ?kind ?limit ?cursor ())
        |> Result.map list_success_json
    | "task_get" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "id" with
              | None -> Error "id is required"
              | Some id ->
                  (match get_task service id with
                   | Error e -> Error e
                   | Ok None -> Error (Printf.sprintf "Task not found: %s" id)
                   | Ok (Some task) -> Ok (task_success_json task)))
         | _ -> Error "Expected JSON object")
    | "task_update" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "id" with
              | None -> Error "id is required"
              | Some id ->
                  (match parse_update_request payload with
                   | Error e -> Error e
                   | Ok req ->
                       let actor = Option.value ~default:"agent" (parse_actor payload) in
                       update_task service ~id ~req ~actor |> Result.map task_success_json))
         | _ -> Error "Expected JSON object")
    | "task_claim" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "id" with
              | None -> Error "id is required"
              | Some id ->
                  let assignee = opt_assoc_string fields "assignee" in
                  let actor = Option.value ~default:"agent" (opt_assoc_string fields "actor") in
                  claim_task service ~id ~assignee ~actor |> Result.map task_success_json)
         | _ -> Error "Expected JSON object")
    | "task_close" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "id" with
              | None -> Error "id is required"
              | Some id ->
                  let reason = opt_assoc_string fields "close_reason" in
                  let actor = Option.value ~default:"agent" (opt_assoc_string fields "actor") in
                  close_task service ~id ~reason ~actor |> Result.map task_success_json)
         | _ -> Error "Expected JSON object")
    | "task_cancel" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "id" with
              | None -> Error "id is required"
              | Some id ->
                  let actor = Option.value ~default:"agent" (opt_assoc_string fields "actor") in
                  cancel_task service ~id ~actor |> Result.map task_success_json)
         | _ -> Error "Expected JSON object")
    | "task_ready" ->
        let limit =
          match payload with
          | `Assoc fields -> opt_assoc_int fields "limit"
          | _ -> None
        in
        ready_tasks service ?limit () |> Result.map list_success_json
    | "task_dep_add" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "to_task_id", opt_assoc_string fields "from_task_id" with
              | None, _ -> Error "to_task_id is required"
              | _, None -> Error "from_task_id is required"
              | Some to_task_id, Some from_task_id ->
                  let dep_type = Option.value ~default:"blocks" (opt_assoc_string fields "dep_type") in
                  let actor = Option.value ~default:"agent" (opt_assoc_string fields "actor") in
                  add_dependency service ~from_task_id ~to_task_id ~dep_type ~actor |> Result.map dep_success_json)
         | _ -> Error "Expected JSON object")
    | "task_dep_list" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "id" with
              | None -> Error "id is required"
              | Some id -> list_dependencies service id |> Result.map (deps_success_json id))
         | _ -> Error "Expected JSON object")
    | "task_events" ->
        (match payload with
         | `Assoc fields ->
             (match opt_assoc_string fields "id" with
              | None -> Error "id is required"
              | Some id ->
                  let after_seq = opt_assoc_int fields "after_seq" in
                  let limit = opt_assoc_int fields "limit" in
                  list_events service id ?after_seq ?limit () |> Result.map (events_success_json id))
         | _ -> Error "Expected JSON object")
    | other -> Error (Printf.sprintf "Unknown task operation: %s" other)
  with exn ->
    let msg = Printexc.to_string exn in
    Log.err (fun m -> m "Task operation failed %s: %s" operation msg);
    Error msg

let dispatch_with_default ~operation ~payload =
  with_default_service (fun service -> dispatch_json service ~operation ~payload)
