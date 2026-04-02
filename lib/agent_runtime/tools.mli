(** CLI tools with approval-gated command and filesystem access. *)

type error_category =
  | FileNotFound
  | PermissionDenied
  | DirectoryMissing
  | PatternNotFound
  | AmbiguousMatch
  | CommandFailed
  | CommandTimeout
  | CommandNotFound
  | InvalidParameters
  | ApprovalRequired
  | Other

type approval_scope =
  | Read
  | Write
  | Execute
  | Install

type approval_request = {
  scope : approval_scope;
  target : string;
  reason : string;
}

type tool_result = {
  content : string;
  is_error : bool;
  status_code : int option;
  bytes : int;
  duration_ms : int option;
  error_type : string option;
  error_category : error_category option;
  recovery_hint : string option;
  approval_request : approval_request option;
}

type t

val success : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> string -> tool_result
val failure : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> ?error_category:error_category -> string -> tool_result
val approval_required : approval_request -> string -> tool_result
val recovery_hint_for_error : category:error_category -> error_message:string -> string

val create_default_registry :
  db_path:string ->
  project_root:string ->
  skills:Agent_skills.Skills.t ->
  unit ->
  t

val close : t -> unit
val pool : t -> Domainslib.Task.pool
val project_root : t -> string
val project_is_trusted : t -> bool
val trust_project : ?path:string -> t -> (string, string) result

val definitions : t -> Llm_types.tool_definition list
val execute : t -> chat_id:int -> string -> Yojson.Safe.t -> tool_result
val requests_parallel_execution : string -> Yojson.Safe.t -> bool
val execute_parallel_batch : t -> chat_id:int -> (string * Yojson.Safe.t) list -> tool_result list
val activate_skill : t -> chat_id:int -> string -> tool_result
val visible_skills : ?include_untrusted:bool -> t -> Agent_skills.Skills.skill_metadata list
val list_skills_formatted : ?include_untrusted:bool -> t -> string
val build_skills_catalog : t -> string

val approve_executable : t -> string -> (string, string) result
val approve_root : t -> scope:approval_scope -> string -> (string, string) result
val approve_install : t -> string -> (string, string) result
val list_approvals_formatted : ?scope:approval_scope -> ?include_project_root:bool -> t -> string
