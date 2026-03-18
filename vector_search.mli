(** Vector similarity search using SQLite with local model-backed embeddings. *)

type vector = float list

type embedding_model = {
  name : string;
  dimensions : int;
  normalize : bool;
}

type search_result = int * int * string * float

type stats = {
  count : int;
  avg_size : float;
  created_min : float option;
  created_max : float option;
}

type tokenized_input = {
  input_ids : int array;
  attention_mask : int array;
  token_type_ids : int array;
}

module Tokenizer : sig
  type t

  val load : string -> (t, string) result
  val tokenize : t -> string -> (tokenized_input, string) result
end

type encoder

val default_model : embedding_model
val load_encoder : model_dir:string -> (encoder, string) result
val embed : encoder -> string -> (vector, string) result
val embed_text : ?model_dir:string -> string -> (vector, string) result

val cosine_similarity : vector -> vector -> float
val vector_to_json : vector -> string
val vector_of_json : string -> vector

val register_udfs : Sqlite3.db -> (unit, string) result
val init_schema : Sqlite3.db -> unit
val store_embedding : Sqlite3.db -> chat_id:int -> memory_id:int -> string -> (unit, string) result
val search_similar :
  Sqlite3.db ->
  chat_id:int ->
  string ->
  limit:int ->
  threshold:float ->
  (search_result list, string) result
val batch_embed_memories : Sqlite3.db -> chat_id:int -> (int * string) list -> (unit, string) result
val get_stats : Sqlite3.db -> chat_id:int -> (stats, string) result
val cleanup_old : Sqlite3.db -> chat_id:int -> before_timestamp:float -> (int, string) result
