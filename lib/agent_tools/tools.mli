(** Minimal CLI tools: read_file, write_file, edit_file, bash. No sandbox restrictions. *)

(** Error categories for tool failures - helps LLM understand recovery strategies *)
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
  | Other

type tool_result = {
  content : string;
  is_error : bool;
  status_code : int option;
  bytes : int;
  duration_ms : int option;
  error_type : string option;
  error_category : error_category option;
  recovery_hint : string option;  (** Actionable hint for LLM to recover *)
}

type t

val success : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> string -> tool_result
val failure : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> ?error_category:error_category -> string -> tool_result

(** Classify an error message into an error category - exposed for testing *)
val classify_error : error_message:string -> error_category

(** Generate a recovery hint for an error category - exposed for testing *)
val recovery_hint_for_error : category:error_category -> error_message:string -> string

val create_default_registry : unit -> t
val close : t -> unit
val pool : t -> Domainslib.Task.pool

val definitions : t -> Llm_types.tool_definition list
val execute : t -> chat_id:int -> string -> Yojson.Safe.t -> tool_result
