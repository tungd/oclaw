(** Minimal CLI tools: read_file, write_file, edit_file, bash. No sandbox restrictions. *)

type tool_result = {
  content : string;
  is_error : bool;
  status_code : int option;
  bytes : int;
  duration_ms : int option;
  error_type : string option;
}

type t

val success : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> string -> tool_result
val failure : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> string -> tool_result

val create_default_registry : unit -> t

val definitions : t -> Llm_types.tool_definition list
val execute : t -> chat_id:int -> string -> Yojson.Safe.t -> tool_result
