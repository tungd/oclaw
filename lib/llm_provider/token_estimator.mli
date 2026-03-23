(** Token estimation for LLM context management. *)

(** Token estimator configuration *)
type t

(** Create a token estimator for a model.
    @param warning_threshold Fraction of context at which to warn (default 0.8)
    @param model Model name (e.g., "gpt-4o", "qwen-plus") *)
val for_model : ?warning_threshold:float -> string -> t

(** Estimate tokens for a string. *)
val count_tokens : t -> string -> int

(** Estimate tokens for a list of messages. *)
val count_messages : t -> (string * string) list -> int

(** Estimate tokens for system prompt. *)
val count_system_prompt : t -> string -> int

(** Estimate tokens for tool definitions (as JSON). *)
val count_tools : t -> string -> int

(** Context usage information. *)
type usage = {
  prompt_tokens : int;
  system_tokens : int;
  tool_tokens : int;
  response_tokens : int;
  total_tokens : int;
  context_limit : int;
  remaining : int;
  percentage : float;
}

(** Calculate context usage.
    All parameters are optional and default to 0. *)
val calculate_usage : 
  ?prompt_tokens:int -> 
  ?system_tokens:int -> 
  ?tool_tokens:int -> 
  ?response_tokens:int -> 
  t -> usage

(** Check context status. *)
val check_context : t -> usage -> [> `Ok | `Warning | `Exceeded ]

(** Format usage as a human-readable string. *)
val format_usage : usage -> string

(** Format a short usage string for TUI display. *)
val format_short : usage -> string

(** Check if we should warn about context usage. *)
val should_warn : t -> usage -> bool

(** Get warning message for context usage. *)
val warning_message : usage -> string option