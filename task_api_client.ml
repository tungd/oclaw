(** HTTP client for OClaw task API *)

open Yojson.Safe

module Http = Http_client

type t = {
  base_url : string;
  timeout : int;
}

let default_client =
  ref {
    base_url = "http://127.0.0.1:8080";
    timeout = 30;
  }

let set_default_base_url base_url =
  default_client := { !default_client with base_url }

let set_default_timeout timeout =
  default_client := { !default_client with timeout }

let get_default () = !default_client

let trim_trailing_slash s =
  if String.length s > 1 && s.[String.length s - 1] = '/' then
    String.sub s 0 (String.length s - 1)
  else s

let percent_encode s =
  let is_unreserved = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '~' -> true
    | _ -> false
  in
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_unreserved c then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let build_query params =
  params
  |> List.filter_map (fun (k, v_opt) -> Option.map (fun v -> (k, v)) v_opt)
  |> List.map (fun (k, v) -> percent_encode k ^ "=" ^ percent_encode v)
  |> String.concat "&"

let request_json ?(client=get_default ()) ~method_ ~path ?(query=[]) ?body () =
  let base = trim_trailing_slash client.base_url in
  let query_s = build_query query in
  let url =
    if query_s = "" then base ^ path
    else base ^ path ^ "?" ^ query_s
  in
  let headers = [ ("content-type", "application/json") ] in
  let req =
    Http.HttpRequest.create
      ~method_
      ~url
      ~headers
      ?body
      ~timeout:client.timeout
      ()
  in
  let resp = Http.make_request req in
  match resp.Http.HttpResponse.error with
  | Some e -> Error (Printf.sprintf "HTTP request failed: %s" e)
  | None ->
      if resp.Http.HttpResponse.status < 200 || resp.Http.HttpResponse.status >= 300 then
        Error (Printf.sprintf "Task API HTTP %d: %s" resp.Http.HttpResponse.status resp.Http.HttpResponse.body)
      else
        (try Ok (Yojson.Safe.from_string resp.Http.HttpResponse.body)
         with _ -> Error (Printf.sprintf "Task API returned invalid JSON: %s" resp.Http.HttpResponse.body))

let create_task ?client payload =
  request_json ?client ~method_:Http.HttpMethod.POST ~path:"/api/tasks" ~body:(Yojson.Safe.to_string payload) ()

let list_tasks ?client ?status ?assignee ?kind ?limit ?cursor () =
  let query =
    [
      ("status", status);
      ("assignee", assignee);
      ("kind", kind);
      ("limit", Option.map string_of_int limit);
      ("cursor", Option.map string_of_float cursor);
    ]
  in
  request_json ?client ~method_:Http.HttpMethod.GET ~path:"/api/tasks" ~query ()

let get_task ?client id =
  request_json ?client ~method_:Http.HttpMethod.GET ~path:("/api/tasks/" ^ percent_encode id) ()

let update_task ?client id payload =
  request_json ?client ~method_:Http.HttpMethod.POST ~path:("/api/tasks/" ^ percent_encode id ^ "/update") ~body:(Yojson.Safe.to_string payload) ()

let ready_tasks ?client ?limit () =
  let query = [ ("limit", Option.map string_of_int limit) ] in
  request_json ?client ~method_:Http.HttpMethod.GET ~path:"/api/tasks/ready" ~query ()

let claim_task ?client id ?assignee ?actor () =
  let payload =
    `Assoc (
      [ ("id", `String id) ]
      @ (match assignee with Some s -> [ ("assignee", `String s) ] | None -> [])
      @ (match actor with Some s -> [ ("actor", `String s) ] | None -> []))
  in
  request_json ?client ~method_:Http.HttpMethod.POST ~path:("/api/tasks/" ^ percent_encode id ^ "/claim") ~body:(Yojson.Safe.to_string payload) ()

let close_task ?client id ?reason ?actor () =
  let payload =
    `Assoc (
      [ ("id", `String id) ]
      @ (match reason with Some s -> [ ("close_reason", `String s) ] | None -> [])
      @ (match actor with Some s -> [ ("actor", `String s) ] | None -> []))
  in
  request_json ?client ~method_:Http.HttpMethod.POST ~path:("/api/tasks/" ^ percent_encode id ^ "/close") ~body:(Yojson.Safe.to_string payload) ()

let cancel_task ?client id ?actor () =
  let payload =
    `Assoc (
      [ ("id", `String id) ]
      @ (match actor with Some s -> [ ("actor", `String s) ] | None -> []))
  in
  request_json ?client ~method_:Http.HttpMethod.POST ~path:("/api/tasks/" ^ percent_encode id ^ "/cancel") ~body:(Yojson.Safe.to_string payload) ()

let add_dependency ?client id ~from_task_id ?dep_type ?actor () =
  let payload =
    `Assoc (
      [ ("to_task_id", `String id); ("from_task_id", `String from_task_id) ]
      @ (match dep_type with Some s -> [ ("dep_type", `String s) ] | None -> [])
      @ (match actor with Some s -> [ ("actor", `String s) ] | None -> []))
  in
  request_json ?client ~method_:Http.HttpMethod.POST ~path:("/api/tasks/" ^ percent_encode id ^ "/dependencies") ~body:(Yojson.Safe.to_string payload) ()

let list_dependencies ?client id =
  request_json ?client ~method_:Http.HttpMethod.GET ~path:("/api/tasks/" ^ percent_encode id ^ "/dependencies") ()

let list_events ?client id ?after_seq ?limit () =
  let query =
    [
      ("after_seq", Option.map string_of_int after_seq);
      ("limit", Option.map string_of_int limit);
    ]
  in
  request_json ?client ~method_:Http.HttpMethod.GET ~path:("/api/tasks/" ^ percent_encode id ^ "/events") ~query ()
