(** Local skill discovery and activation. *)

type skill_metadata = {
  name : string;
  description : string;
  path : string;
}

type t

val create : skills_dir:string -> t
val skills_dir : t -> string
val discover_skills : t -> skill_metadata list
val build_skills_catalog : t -> string
val list_skills_formatted : t -> string
val activate_skill : t -> string -> (string, string) result
val sync_skill : t -> ?repo:string -> string -> (string, string) result
