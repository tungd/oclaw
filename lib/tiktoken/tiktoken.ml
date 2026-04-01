(** Tiktoken - Byte Pair Encoding tokenizer for token counting
    
    This module provides token counting functionality for estimating
    context usage with LLM APIs. It implements the BPE algorithm used
    by OpenAI's tiktoken library.
    
    Example usage:
    {[
      let encoding = Tiktoken.get_encoding "cl100k_base" in
      let tokens = Tiktoken.encode encoding "Hello, world!" in
      let count = List.length tokens in
      printf "Token count: %d\n" count
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
let get_encoding = Encoding.get_encoding

(** Get encoding for a model name.
    
    Maps model names to their appropriate encodings:
    - GPT-4o models -> o200k_base
    - GPT-4, GPT-3.5-turbo -> cl100k_base
    - Qwen models -> cl100k_base (approximate)
    - Unknown models -> cl100k_base (default)
*)
let encoding_for_model = Encoding.encoding_for_model

(** Encode a string into token IDs.
    
    Uses the BPE algorithm to split the input text into tokens
    and return their IDs.
    
    @param encoding The encoding to use
    @param text The text to encode
    @return A list of token IDs *)
let encode encoding text =
  if String.length text = 0 then []
  else begin
    (* Pre-tokenize: split text into chunks *)
    let chunks = Encoding.split_into_chunks text in
    (* Encode each chunk with BPE *)
    List.concat_map (fun chunk ->
      let bytes = Bytes.of_string chunk in
      Bpe.Optimized.encode encoding.Encoding.vocab bytes
    ) chunks
  end

(** Decode token IDs back to a string.
    
    Converts a list of token IDs back to the original text.
    Invalid token IDs are replaced with the Unicode replacement character.
    
    @param encoding The encoding to use
    @param tokens The token IDs to decode
    @return The decoded string *)
let decode encoding tokens =
  Bpe.decode encoding.Encoding.inverse_vocab tokens

(** Count the number of tokens in a string.
    
    This is a convenience function that encodes the text and returns
    the length of the resulting token list.
    
    @param encoding The encoding to use
    @param text The text to count tokens for
    @return The number of tokens *)
let count_tokens encoding text =
  List.length (encode encoding text)

(** Count tokens in a string, optimized for speed.
    
    For simple token counting, this avoids the overhead of building
    the full token list. Currently equivalent to count_tokens.
    
    @param encoding The encoding to use
    @param text The text to count tokens for
    @return The number of tokens *)
let count_tokens_fast encoding text =
  count_tokens encoding text

(** Encode a string and return both token IDs and their text representations.
    
    Useful for debugging and visualization.
    
    @param encoding The encoding to use
    @param text The text to encode
    @return A list of (token_id, token_text) pairs *)
let encode_with_text encoding text =
  let tokens = encode encoding text in
  List.map (fun token_id ->
    let token_text = 
      match Hashtbl.find_opt encoding.Encoding.inverse_vocab token_id with
      | Some s -> s
      | None -> ""  (* replacement character *)
    in
    (token_id, token_text)
  ) tokens

(** Estimate token count for chat messages.
    
    This estimates the token count for a list of chat messages in the
    format used by LLM APIs. Includes overhead for message formatting.
    
    The estimation follows the format:
    - Each message has role and content
    - Messages are formatted with <|start|>, role, content, <|end|>
    - Additional tokens for the reply priming
    
    @param encoding The encoding to use
    @param messages List of (role, content) pairs
    @return Estimated token count *)
let estimate_messages_tokens encoding messages =
  (* Base overhead for chat format *)
  let base_overhead = 3 in  (* Approximate tokens for message formatting *)
  let reply_overhead = 3 in (* Approximate tokens for reply priming *)
  
  let message_tokens = List.fold_left (fun acc (role, content) ->
    let role_tokens = count_tokens encoding role in
    let content_tokens = count_tokens encoding content in
    acc + role_tokens + content_tokens + 4  (* 4 extra tokens per message for formatting *)
  ) 0 messages in
  
  base_overhead + message_tokens + reply_overhead

(** Estimate token count for a system prompt. *)
let estimate_system_tokens encoding system_prompt =
  count_tokens encoding system_prompt

(** Estimate token count for tool definitions.
    
    Tools are serialized to JSON schema, which adds token overhead.
    This provides a rough estimate. *)
let estimate_tools_tokens encoding tools_json =
  count_tokens encoding tools_json

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
let context_info ~used ~limit =
  { used; limit; remaining = limit - used; percentage = float_of_int used /. float_of_int limit *. 100.0 }

(** Check if context is approaching limit.
    
    @param used Number of tokens used
    @param limit Maximum context size
    @param threshold Warning threshold (e.g., 0.8 for 80%)
    @return `Ok, `Warning, or `Exceeded *)
let check_context ~used ~limit ~threshold =
  if used >= limit then `Exceeded
  else if float_of_int used >= float_of_int limit *. threshold then `Warning
  else `Ok

(** Format a context info as a human-readable string. *)
let format_context_info info =
  Printf.sprintf "%d/%d tokens (%.1f%%) - %d remaining"
    info.used info.limit info.percentage info.remaining

(** Common model context limits *)
let model_context_limits = [
  ("gpt-4o", 128000);
  ("gpt-4o-mini", 128000);
  ("gpt-4-turbo", 128000);
  ("gpt-4-turbo-preview", 128000);
  ("gpt-4", 8192);
  ("gpt-4-32k", 32768);
  ("gpt-3.5-turbo", 16385);
  ("gpt-3.5-turbo-16k", 16385);
  ("text-embedding-ada-002", 8191);
  ("text-embedding-3-small", 8191);
  ("text-embedding-3-large", 8191);
  ("glm-5", 128000);
  (* Qwen models *)
  ("qwen3.5-plus", 128000);
  ("qwen-turbo", 131072);
  ("qwen-plus", 131072);
  ("qwen-max", 32768);
  (* Default fallback *)
  ("default", 4096);
]

(** Get context limit for a model name. *)
let get_context_limit model_name =
  let model_lower = String.lowercase_ascii model_name in
  let matches prefix =
    String.length model_lower >= String.length prefix &&
    String.sub model_lower 0 (String.length prefix) = prefix
  in
  let rec find = function
    | [] -> 4096  (* Default fallback *)
    | (prefix, limit) :: rest ->
        if matches prefix then limit
        else find rest
  in
  find model_context_limits
