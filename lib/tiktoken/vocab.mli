(** Vocabulary loading and management for tiktoken BPE tokenizer. *)

(** A vocabulary maps byte sequences to token IDs (ranks) *)
type t = (bytes, int) Hashtbl.t

(** Parse error during vocabulary loading *)
exception Parse_error of string

(** Base64 decode a string to bytes *)
val base64_decode : string -> bytes

(** Parse a single line of the vocabulary file.
    Format: <base64_token><space><rank>
    Returns None for empty/invalid lines. *)
val parse_line : string -> (string * int) option

(** Load vocabulary from a string (file contents) *)
val from_string : string -> t

(** Load vocabulary from a chunked string source (for embedded vocabularies) *)
val from_chunks : (int -> string option) -> t

(** Lookup a byte sequence in the vocabulary.
    Returns the rank (token ID) or None if not found. *)
val lookup : t -> bytes -> int option

(** Get the number of tokens in the vocabulary *)
val size : t -> int

(** Create an inverted vocabulary for decoding (token ID -> bytes) *)
val invert : t -> (int, string) Hashtbl.t