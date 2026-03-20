(** Agent Skills discovery, parsing, trust, activation, and installation. *)

type scope =
  | Builtin
  | User
  | Project

type allowed_tool =
  | Allowed_read
  | Allowed_write
  | Allowed_bash of string
  | Allowed_other of string

type skill_metadata = {
  name : string;
  description : string;
  license : string option;
  compatibility : string option;
  metadata : (string * string) list;
  allowed_tools : allowed_tool list;
  path : string;
  dir : string;
  body : string;
  scope : scope;
  trusted : bool;
  resources : string list;
}

type remote_skill = {
  name : string;
  description : string;
  repo : string;
  git_ref : string;
  root_path : string;
  files : string list;
}

type activation_result = {
  content : string;
  already_activated : bool;
  allowed_tools : allowed_tool list;
  skill_dir : string;
}

type t

val create :
  db_path:string ->
  project_root:string ->
  project_skills_dir:string ->
  user_skills_dir:string ->
  catalog_cache_path:string ->
  ?builtin_skills_dirs:string list ->
  unit ->
  t

val close : t -> unit

val skills_dir : t -> string
val user_skills_dir : t -> string

val discover_skills : ?include_untrusted:bool -> t -> skill_metadata list
val available_skill_names : t -> string list
val build_skills_catalog : t -> string
val list_skills_formatted : ?include_untrusted:bool -> t -> string

val activate_skill : t -> chat_id:int -> string -> (activation_result, string) result

val project_is_trusted : t -> bool
val trust_project : ?path:string -> t -> (string, string) result

val refresh_remote_catalog : t -> (remote_skill list, string) result
val remote_catalog : t -> (remote_skill list, string) result
val search_remote_catalog : t -> query:string -> (remote_skill list, string) result
val install_remote_skill : t -> name:string -> (string, string) result
