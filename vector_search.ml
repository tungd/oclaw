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

type search_result = int * int * string * float

type stats = {
  count : int;
  avg_size : float;
  created_min : float option;
  created_max : float option;
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
    Array.map (fun x -> x /. magnitude) vec |> Array.to_list
  else
    Array.to_list vec

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
    Array.map (fun x -> x /. magnitude) vec |> Array.to_list
  else
    Array.to_list vec

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
  ) queries

(* Generate embedding for text *)
let generate_embedding ?(model=default_model) text =
  embed_text ~dimensions:model.dimensions text

let prepare db sql =
  Sqlite3.prepare db sql

let finalize stmt =
  ignore (Sqlite3.finalize stmt)

let bind_text stmt index value =
  ignore (Sqlite3.bind stmt index (Sqlite3.Data.TEXT value))

let bind_int stmt index value =
  ignore (Sqlite3.bind stmt index (Sqlite3.Data.INT (Int64.of_int value)))

let bind_float stmt index value =
  ignore (Sqlite3.bind stmt index (Sqlite3.Data.FLOAT value))

let int_column stmt index =
  match Sqlite3.column stmt index with
  | Sqlite3.Data.INT value -> Int64.to_int value
  | _ -> 0

let text_column stmt index =
  match Sqlite3.column stmt index with
  | Sqlite3.Data.TEXT value -> value
  | _ -> ""

let float_column stmt index =
  match Sqlite3.column stmt index with
  | Sqlite3.Data.FLOAT value -> Some value
  | Sqlite3.Data.INT value -> Some (Int64.to_float value)
  | _ -> None

(* Store embedding in database *)
let store_embedding db ~chat_id ~memory_id text =
  let embedding = generate_embedding text in
  let embedding_json = vector_to_json embedding in
  let sql = "INSERT INTO embeddings (chat_id, memory_id, text_content, embedding, dimensions, model_name) 
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)" in
  let stmt = prepare db sql in
  Fun.protect
    ~finally:(fun () -> finalize stmt)
    (fun () ->
      bind_int stmt 1 chat_id;
      bind_int stmt 2 memory_id;
      bind_text stmt 3 text;
      bind_text stmt 4 embedding_json;
      bind_int stmt 5 default_model.dimensions;
      bind_text stmt 6 default_model.name;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE | Sqlite3.Rc.ROW -> Ok ()
      | rc -> Error (Printf.sprintf "sqlite store embedding failed: %s" (Sqlite3.Rc.to_string rc)))

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
  
  let stmt = prepare db sql in
  Fun.protect
    ~finally:(fun () -> finalize stmt)
    (fun () ->
      bind_int stmt 1 chat_id;
      bind_int stmt 2 limit;
      let rec collect acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let id = int_column stmt 0 in
            let memory_id = int_column stmt 1 in
            let text = text_column stmt 2 in
            let similarity = float_column stmt 3 |> Option.value ~default:0.0 in
            if similarity >= threshold then
              collect ((id, memory_id, text, similarity) :: acc)
            else
              Ok (List.rev acc)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc -> Error (Printf.sprintf "sqlite search_similar failed: %s" (Sqlite3.Rc.to_string rc))
      in
      collect [])

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
  let stmt = prepare db sql in
  Fun.protect
    ~finally:(fun () -> finalize stmt)
    (fun () ->
      bind_int stmt 1 chat_id;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let count = int_column stmt 0 in
          let avg_size = float_column stmt 1 |> Option.value ~default:0.0 in
          let created_min = float_column stmt 2 in
          let created_max = float_column stmt 3 in
          Ok { count; avg_size; created_min; created_max }
      | Sqlite3.Rc.DONE ->
          Ok { count = 0; avg_size = 0.0; created_min = None; created_max = None }
      | rc ->
          Error (Printf.sprintf "sqlite get_stats failed: %s" (Sqlite3.Rc.to_string rc)))

(* Clean up old embeddings *)
let cleanup_old db ~chat_id ~before_timestamp =
  let sql = "DELETE FROM embeddings WHERE chat_id = ?1 AND created_at < ?2" in
  let stmt = prepare db sql in
  Fun.protect
    ~finally:(fun () -> finalize stmt)
    (fun () ->
      bind_int stmt 1 chat_id;
      bind_float stmt 2 before_timestamp;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok (Sqlite3.changes db)
      | rc -> Error (Printf.sprintf "sqlite cleanup_old failed: %s" (Sqlite3.Rc.to_string rc)))
