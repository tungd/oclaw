(** Tree utilities for materialized path management. *)

(** Generate child path from parent path and new node ID. *)
val make_child_path : parent_path:string -> child_id:int -> string

(** Get depth from path (number of segments). *)
val path_depth : string -> int

(** Get parent path. Returns None for root paths like "1". *)
val parent_path : string -> string option

(** Check if path_a is ancestor of path_b. *)
val is_ancestor : string -> string -> bool

(** Parse "/1/5/12" into [1; 5; 12]. *)
val path_to_ids : string -> int list

(** Convert list of IDs to path string. *)
val ids_to_path : int list -> string

(** Check if path represents a root node (only one segment). *)
val is_root_path : string -> bool
