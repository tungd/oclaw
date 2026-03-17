(** Vector similarity search using pure SQLite with OCaml UDFs.
    
    Implements cosine similarity as a SQLite user-defined function.
    No external dependencies - pure OCaml + SQLite.
*)

open Sqlite3

module Log = (val Logs.src_log (Logs.Src.create "vector_search") : Logs.LOG)

(* Vector type: list of floats *)
type vector = float list

(* Embedding model types *)
type embedding_model = {
  name : string;
  dimensions : int;
  normalize : bool;
}

(* Default embedding model config *)
let default_model = {
  name = "ocaml-tiny-embed";
  dimensions = 384;  (* Common embedding size *)
  normalize = true;
}

(* Simple word-based embedding (placeholder - replace with real model) *)
(* In production, you'd call an embedding API or load a local model *)
let embed_text_simple ~dimensions text =
  (* Very basic hash-based embedding for demonstration *)
  (* Real implementation would use BERT, sentence-transformers, or API *)
  let hash_char c = float (int_of_char c) /. 255.0 in
  let vec = Array.make dimensions 0.0 in
  let len = String.length text in
  for i = 0 to len - 1 do
    let idx = (int_of_char text.[i]) mod dimensions in
    vec.(idx) <- vec.(idx) +. hash_char text.[i]
  done;
  
  (* Normalize to unit vector *)
  let magnitude = sqrt (Array.fold_left (fun acc x -> acc +. x *. x) 0.0 vec) in
  if magnitude > 0.0 then
    Array.map (fun x -> x /. magnitude) vec
  else
    vec

(* Better embedding using word frequencies + hash *)
let embed_text ~dimensions text =
  let words = String.split_on_char ' ' (String.lowercase_ascii text) in
  let word_counts = Hashtbl.create 100 in
  List.iter (fun word ->
    if String.length word > 2 then (
      let count = try Hashtbl.find word_counts word with Not_found -> 0 in
      Hashtbl.replace word_counts word (count + 1)
    )
  ) words;
  
  let vec = Array.make dimensions 0.0 in
  Hashtbl.iter (fun word count ->
    let hash = ref 0.0 in
    for i = 0 to String.length word - 1 do
      hash := !hash +. (float (int_of_char word.[i]) *. float (i + 1))
    done;
    let idx = int_of_float (abs_float !hash) mod dimensions in
    vec.(idx) <- vec.(idx) +. (float count *. (1.0 +. sin !hash))
  ) word_counts;
  
  (* L2 normalize *)
  let magnitude = sqrt (Array.fold_left (fun acc x -> acc +. x *. x) 0.0 vec) in
  if magnitude > 0.001 then
    Array.map (fun x -> x /. magnitude) vec
  else
    vec

(* Cosine similarity between two vectors *)
let cosine_similarity vec1 vec2 =
  if List.length vec1 <> List.length vec2 then
    0.0
  else
    let dot = List.fold_left2 (fun acc a b -> acc +. (a *. b)) 0.0 vec1 vec2 in
    (* Vectors are already normalized, so dot product = cosine similarity *)
    dot

(* Convert vector to/from JSON string for storage *)
let vector_to_json vec =
  let floats = List.map string_of_float vec in
  "[" ^ String.concat "," floats ^ "]"

let vector_of_json json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `List floats ->
        List.map (function `Float f -> f | `Int i -> float i | _ -> 0.0) floats
    | _ -> []
  with _ -> []

(* SQLite UDF: cosine_similarity(vec1_json, vec2_json) *)
let cosine_similarity_udf arg1 arg2 =
  match arg1, arg2 with
  | Sqlite3.Data.TEXT json1, Sqlite3.Data.TEXT json2 ->
      let vec1 = vector_of_json json1 in
      let vec2 = vector_of_json json2 in
      if vec1 = [] || vec2 = [] then
        Sqlite3.Data.NULL
      else
        let sim = cosine_similarity vec1 vec2 in
        Sqlite3.Data.FLOAT sim
  | _ -> Sqlite3.Data.NULL

(* Register UDFs with database *)
let register_udfs db =
  try
    create_fun2 db "cosine_similarity" cosine_similarity_udf;
    Log.info (fun m -> m "Registered vector search UDFs");
    Ok ()
  with SqliteError msg ->
    Log.err (fun m -> m "Failed to register UDFs: %s" msg);
    Error msg

(* Initialize vector search schema *)
let init_schema db =
  let queries = [
    (* Embeddings table *)
    "CREATE TABLE IF NOT EXISTS embeddings (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      chat_id INTEGER,
      memory_id INTEGER,
      text_content TEXT NOT NULL,
      embedding TEXT NOT NULL,
      dimensions INTEGER NOT NULL,
      model_name TEXT NOT NULL,
      created_at REAL DEFAULT (strftime('%s', 'now')),
      metadata TEXT
    )";
    
    (* Index for fast lookups *)
    "CREATE INDEX IF NOT EXISTS idx_embeddings_chat ON embeddings(chat_id)";
    "CREATE INDEX IF NOT EXISTS idx_embeddings_memory ON embeddings(memory_id)";
    "CREATE INDEX IF NOT EXISTS idx_embeddings_created ON embeddings(created_at)";
  ] in
  
  List.iteri (fun i sql ->
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> Log.debug (fun m -> m "Created schema item %d" i)
    | Sqlite3.Rc.ERROR -> Log.err (fun m -> m "Schema error %d: %s" i (Sqlite3.errmsg db))
    | _ -> ()
  )

(* Generate embedding for text *)
let generate_embedding ?(model=default_model) text =
  Array.to_list (embed_text ~dimensions:model.dimensions text)

(* Store embedding in database *)
let store_embedding db ~chat_id ~memory_id text =
  let embedding = generate_embedding text in
  let embedding_json = vector_to_json embedding in
  let sql = "INSERT INTO embeddings (chat_id, memory_id, text_content, embedding, dimensions, model_name) 
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)" in
  match Db.prepare db sql with
  | Error (`Msg msg) -> Error msg
  | Ok stmt ->
      Db.bind stmt [|
        Db.Data.INT (Int64.of_int chat_id);
        Db.Data.INT (Int64.of_int memory_id);
        Db.Data.TEXT text;
        Db.Data.TEXT embedding_json;
        Db.Data.INT (Int64.of_int model.dimensions);
        Db.Data.TEXT model.name;
      |];
      match Db.step stmt with
      | Result.ROW | Result.DONE -> 
          Db.finalize stmt;
          Ok ()
      | Result.ERROR msg -> 
          Db.finalize stmt;
          Error msg
      | _ -> 
          Db.finalize stmt;
          Error "Unexpected result"

(* Search for similar texts *)
let search_similar db ~chat_id query_text ~limit ~threshold =
  let query_embedding = generate_embedding query_text in
  let query_json = vector_to_json query_embedding in
  
  let sql = Printf.sprintf
    "SELECT id, memory_id, text_content, cosine_similarity(embedding, '%s') as similarity
     FROM embeddings
     WHERE chat_id = ?1
     ORDER BY similarity DESC
     LIMIT ?2" query_json
  in
  
  match Db.prepare db sql with
  | Error (`Msg msg) -> Error msg
  | Ok stmt ->
      Db.bind stmt [|
        Db.Data.INT (Int64.of_int chat_id);
        Db.Data.INT (Int64.of_int limit);
      |];
      
      let rec collect acc =
        match Db.step stmt with
        | Result.ROW ->
            let id = Db.column stmt 0 |> Db.Data.to_int64_exn |> Int64.to_int in
            let memory_id = Db.column stmt 1 |> Db.Data.to_int64_exn |> Int64.to_int in
            let text = Db.column stmt 2 |> Db.Data.to_string_exn in
            let similarity = Db.column stmt 3 |> Db.Data.to_float_exn in
            
            if similarity >= threshold then
              collect ((id, memory_id, text, similarity) :: acc)
            else
              List.rev acc
        | Result.DONE -> List.rev acc
        | _ -> List.rev acc
      in
      
      let results = collect [] in
      Db.finalize stmt;
      Ok results

(* Batch generate embeddings for existing memories *)
let batch_embed_memories db ~chat_id memories =
  let rec process = function
    | [] -> Ok ()
    | (memory_id, text) :: rest ->
        match store_embedding db ~chat_id ~memory_id text with
        | Ok () -> process rest
        | Error msg -> Error msg
  in
  process memories

(* Get embedding statistics *)
let get_stats db ~chat_id =
  let sql = "SELECT COUNT(*), AVG(LENGTH(embedding)), MIN(created_at), MAX(created_at)
             FROM embeddings WHERE chat_id = ?1" in
  match Db.prepare db sql with
  | Error (`Msg msg) -> Error msg
  | Ok stmt ->
      Db.bind stmt [| Db.Data.INT (Int64.of_int chat_id) |];
      match Db.step stmt with
      | Result.ROW ->
          let count = Db.column stmt 0 |> Db.Data.to_int64_exn |> Int64.to_int in
          let avg_size = Db.column stmt 1 |> Db.Data.to_float_exn in
          let created_min = Db.column stmt 2 |> Db.Data.to_float_opt in
          let created_max = Db.column stmt 3 |> Db.Data.to_float_opt in
          Db.finalize stmt;
          Ok { count; avg_size; created_min; created_max }
      | _ -> 
          Db.finalize stmt;
          Ok { count = 0; avg_size = 0.0; created_min = None; created_max = None }

(* Clean up old embeddings *)
let cleanup_old db ~chat_id ~before_timestamp =
  let sql = "DELETE FROM embeddings WHERE chat_id = ?1 AND created_at < ?2" in
  match Db.prepare db sql with
  | Error (`Msg msg) -> Error msg
  | Ok stmt ->
      Db.bind stmt [|
        Db.Data.INT (Int64.of_int chat_id);
        Db.Data.REAL before_timestamp;
      |];
      match Db.step stmt with
      | Result.DONE -> 
          let changes = Db.changes db in
          Db.finalize stmt;
          Ok changes
      | Result.ERROR msg -> 
          Db.finalize stmt;
          Error msg
      | _ -> 
          Db.finalize stmt;
          Error "Unexpected result"
