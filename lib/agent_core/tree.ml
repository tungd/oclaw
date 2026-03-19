(** Tree utilities for materialized path management. *)

(** Generate child path from parent path and new node ID. *)
let make_child_path ~parent_path ~child_id =
  Printf.sprintf "%s/%d" parent_path child_id

(** Get depth from path (number of segments). *)
let path_depth path =
  String.split_on_char '/' path 
  |> List.filter (fun s -> s <> "") 
  |> List.length

(** Get parent path. Returns None for root paths like "/1". *)
let parent_path path =
  let parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  match List.rev parts with
  | _ :: rest when rest <> [] -> Some ("/" ^ String.concat "/" (List.rev rest))
  | _ -> None

(** Check if path_a is ancestor of path_b. *)
let is_ancestor path_a path_b =
  String.starts_with ~prefix:path_a path_b && path_a <> path_b

(** Parse "/1/5/12" into [1; 5; 12]. *)
let path_to_ids path =
  String.split_on_char '/' path
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

(** Convert list of IDs to path string. *)
let ids_to_path ids =
  "/" ^ String.concat "/" (List.map string_of_int ids)

(** Check if path represents a root node (only one segment). *)
let is_root_path path =
  path_depth path = 1
