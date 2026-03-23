(** Layoutz-backed append-only interactive REPL frontend. *)

type command =
  [ `Ignore
  | `Exit
  | `Prompt of string
  ]

type deps = {
  process : emit:(Acp.Message.t -> unit) -> string -> (unit, string) result;
  resolve_permission : Acp.Message.permission_outcome -> (unit, string) result;
  history : unit -> Llm_types.message list;
  project_root : string;
  model_name : string;
  chat_label : string;
}

val classify_input : string -> command
val render_history_message : Llm_types.message -> string list
val render_markdown_lines : supports_ansi:bool -> string -> string list
val select_permission_option :
  Acp.Message.permission_option list ->
  string ->
  (Acp.Message.permission_outcome, string) result

(** Test-oriented loop entrypoint with injectable channels and runtime deps. *)
val run_loop :
  input:in_channel ->
  output:out_channel ->
  persistent:bool ->
  deps ->
  unit

val run : state:Agent_runtime.App.t -> chat_id:int -> persistent:bool -> unit
