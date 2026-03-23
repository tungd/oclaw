(** Tiktoken - Byte Pair Encoding tokenizer for token counting
    
    This module provides token counting functionality for estimating
    context usage with LLM APIs. It implements the BPE algorithm used
    by OpenAI's tiktoken library.
    
    Example usage:
    {[
      let encoding = Tiktoken.get_encoding "cl100k_base" in
      let tokens = Tiktoken.encode encoding "Hello, world!" in
      let count = List.length tokens in
      Printf.printf "Token count: %d\n" count
    ]}
*)

(** Re-export modules for convenience *)
module Vocab = Vocab
module Bpe = Bpe
module Encoding = Encoding

(** An encoding specification *)
type encoding = Encoding.t

(** Get an encoding by name.
    
    Supported encodings:
    - "cl100k_base" - GPT-4, GPT-3.5-turbo, text-embedding-ada-002
    - "o200k_base" - GPT-4o, GPT-4o-mini
    
    Raises [Failure] if the encoding name is unknown. *)
val get_encoding : string -> encoding

(** Get encoding for a model name.
    
    Maps model names to their appropriate encodings:
    - GPT-4o models -> o200k_base
    - GPT-4, GPT-3.5-turbo -> cl100k_base
    - Qwen models -> cl100k_base (approximate)
    - Unknown models -> cl100k_base (default)
*)
val encoding_for_model : string -> encoding

(** Encode a string into token IDs.
    
    Uses the BPE algorithm to split the input text into tokens
    and return their IDs. *)
val encode : encoding -> string -> int list

(** Decode token IDs back to a string.
    
    Converts a list of token IDs back to the original text.
    Invalid token IDs are replaced with the Unicode replacement character. *)
val decode : encoding -> int list -> string

(** Count the number of tokens in a string. *)
val count_tokens : encoding -> string -> int

(** Count tokens in a string, optimized for speed. *)
val count_tokens_fast : encoding -> string -> int

(** Encode a string and return both token IDs and their text representations. *)
val encode_with_text : encoding -> string -> (int * string) list

(** Estimate token count for chat messages.
    
    @param encoding The encoding to use
    @param messages List of (role, content) pairs
    @return Estimated token count *)
val estimate_messages_tokens : encoding -> (string * string) list -> int

(** Estimate token count for a system prompt. *)
val estimate_system_tokens : encoding -> string -> int

(** Estimate token count for tool definitions (as JSON string). *)
val estimate_tools_tokens : encoding -> string -> int

(** Token usage statistics *)
type token_usage = {
  input_tokens : int;
  output_tokens : int;
  total_tokens : int;
}

(** Context limit information *)
type context_info = {
  used : int;
  limit : int;
  remaining : int;
  percentage : float;
}

(** Create a context info from used tokens and limit *)
val context_info : used:int -> limit:int -> context_info

(** Check if context is approaching limit.
    
    @param used Number of tokens used
    @param limit Maximum context size
    @param threshold Warning threshold (e.g., 0.8 for 80%)
    @return `Ok, `Warning, or `Exceeded *)
val check_context : used:int -> limit:int -> threshold:float -> [`Ok | `Warning | `Exceeded]

(** Format a context info as a human-readable string. *)
val format_context_info : context_info -> string

(** Get context limit for a model name. *)
val get_context_limit : string -> int