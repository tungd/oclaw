(** Encoding definitions for different tiktoken models. *)

(** Special tokens for different encodings *)
type special_tokens = {
  endoftext : int option;
  fim_prefix : int option;   (** For fill-in-the-middle completions *)
  fim_middle : int option;
  fim_suffix : int option;
  endofprompt : int option;
}

(** An encoding specification *)
type t = {
  name : string;
  vocab : (bytes, int) Hashtbl.t;
  inverse_vocab : (int, string) Hashtbl.t;
  special_tokens : special_tokens;
}

(** Check if a Unicode code point is a letter *)
val is_letter : int -> bool

(** Check if a Unicode code point is a number/digit *)
val is_number : int -> bool

(** Check if a Unicode code point is whitespace *)
val is_whitespace : int -> bool

(** Split text into chunks for BPE encoding.
    This handles Unicode properly and approximates the tiktoken regex patterns. *)
val split_into_chunks : string -> string list

(** Load the cl100k_base encoding (used by GPT-4, GPT-3.5-turbo, text-embedding-ada-002) *)
val cl100k_base : unit -> t

(** Load the o200k_base encoding (used by GPT-4o, GPT-4o-mini) *)
val o200k_base : unit -> t

(** Get encoding by name.
    Supported: "cl100k_base", "o200k_base" *)
val get_encoding : string -> t

(** Get encoding for a model name *)
val encoding_for_model : string -> t