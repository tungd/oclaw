(** Vector similarity search using SQLite with local model-backed embeddings. *)

open Sqlite3

module Log = (val Logs.src_log (Logs.Src.create "vector_search") : Logs.LOG)

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

module Tokenizer = struct
  type t = {
    vocab : (string, int) Hashtbl.t;
    unk_id : int;
    cls_id : int;
    sep_id : int;
    pad_id : int;
    do_lower_case : bool;
    max_length : int;
  }

  let slurp path =
    Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all

  let file_if_exists path =
    if Sys.file_exists path then Some path else None

  let read_json path =
    try Ok (Yojson.Safe.from_file path) with exn -> Error (Printexc.to_string exn)

  let lookup_token_id vocab token =
    match Hashtbl.find_opt vocab token with
    | Some id -> Ok id
    | None -> Error ("Tokenizer asset missing required token " ^ token)

  let json_member_string name = function
    | `Assoc fields ->
        begin
          match List.assoc_opt name fields with
          | Some (`String value) -> Some value
          | _ -> None
        end
    | _ -> None

  let json_member_bool name = function
    | `Assoc fields ->
        begin
          match List.assoc_opt name fields with
          | Some (`Bool value) -> Some value
          | _ -> None
        end
    | _ -> None

  let json_member_int name = function
    | `Assoc fields ->
        begin
          match List.assoc_opt name fields with
          | Some (`Int value) -> Some value
          | _ -> None
        end
    | _ -> None

  let tokenize_basic ~do_lower_case text =
    let normalize =
      if do_lower_case then String.lowercase_ascii else Fun.id
    in
    let text = normalize text in
    let buffer = Buffer.create 32 in
    let tokens = ref [] in
    let flush () =
      if Buffer.length buffer > 0 then (
        tokens := Buffer.contents buffer :: !tokens;
        Buffer.clear buffer
      )
    in
    let is_word_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
      | _ -> false
    in
    String.iter
      (fun ch ->
        if is_word_char ch then
          Buffer.add_char buffer ch
        else if Char.code ch <= 32 then
          flush ()
        else (
          flush ();
          tokens := String.make 1 ch :: !tokens
        ))
      text;
    flush ();
    List.rev !tokens

  let wordpiece_ids t token =
    match Hashtbl.find_opt t.vocab token with
    | Some id -> [ id ]
    | None ->
        let len = String.length token in
        let rec consume start acc =
          if start >= len then
            Ok (List.rev acc)
          else
            let rec find_piece finish =
              if finish <= start then
                None
              else
                let piece =
                  if start = 0 then
                    String.sub token start (finish - start)
                  else
                    "##" ^ String.sub token start (finish - start)
                in
                match Hashtbl.find_opt t.vocab piece with
                | Some id -> Some (id, finish)
                | None -> find_piece (finish - 1)
            in
            match find_piece len with
            | Some (piece_id, finish) -> consume finish (piece_id :: acc)
            | None -> Error "unknown"
        in
        begin
          match consume 0 [] with
          | Ok ids -> ids
          | Error _ -> [ t.unk_id ]
        end

  let load_vocab_from_txt path : ((string, int) Hashtbl.t, string) result =
    let vocab = Hashtbl.create 50_000 in
    let lines = slurp path |> String.split_on_char '\n' in
    List.iteri
      (fun index token ->
        let token = String.trim token in
        if token <> "" then Hashtbl.replace vocab token index)
      lines;
    Ok vocab

  let load_vocab_from_tokenizer_json path : ((string, int) Hashtbl.t, string) result =
    match read_json path with
    | Error _ as err -> err
    | Ok json ->
        let vocab = Hashtbl.create 50_000 in
        begin
          match json with
          | `Assoc fields ->
              begin
                match List.assoc_opt "model" fields with
                | Some (`Assoc model_fields) ->
                    begin
                      match List.assoc_opt "vocab" model_fields with
                      | Some (`Assoc vocab_fields) ->
                          List.iter
                            (fun (token, value) ->
                              match value with
                              | `Int id -> Hashtbl.replace vocab token id
                              | _ -> ())
                            vocab_fields
                      | _ -> ()
                    end
                | _ -> ()
              end
          | _ -> ()
        end;
        if Hashtbl.length vocab = 0 then
          Error ("Tokenizer JSON does not contain a usable vocab: " ^ path)
        else
          Ok vocab

  let read_tokenizer_settings model_dir =
    let tokenizer_config =
      file_if_exists (Filename.concat model_dir "tokenizer_config.json")
    in
    let model_config = file_if_exists (Filename.concat model_dir "config.json") in
    let read_setting f path =
      match path with
      | None -> None
      | Some path ->
          begin
            match read_json path with
            | Ok json -> f json
            | Error _ -> None
          end
    in
    let do_lower_case =
      match read_setting (json_member_bool "do_lower_case") tokenizer_config with
      | Some value -> value
      | None -> false
    in
    let max_length =
      match read_setting (json_member_int "model_max_length") tokenizer_config with
      | Some value when value > 0 -> value
      | _ ->
          begin
            match read_setting (json_member_int "max_position_embeddings") model_config with
            | Some value when value > 0 -> value
            | _ -> 512
          end
    in
    (do_lower_case, max_length)

  let load model_dir : (t, string) result =
    let tokenizer_json = Filename.concat model_dir "tokenizer.json" in
    let vocab_txt = Filename.concat model_dir "vocab.txt" in
    let vocab_result =
      if Sys.file_exists tokenizer_json then
        load_vocab_from_tokenizer_json tokenizer_json
      else if Sys.file_exists vocab_txt then
        load_vocab_from_txt vocab_txt
      else
        Error ("Tokenizer assets not found in " ^ model_dir)
    in
    match vocab_result with
    | Error _ as err -> err
    | Ok vocab ->
        let do_lower_case, max_length = read_tokenizer_settings model_dir in
        let find_required tokens =
          let rec loop = function
            | [] -> Stdlib.Result.Error "missing token"
            | token :: rest ->
                begin
                  match Hashtbl.find_opt vocab token with
                  | Some id -> Stdlib.Result.Ok id
                  | None -> loop rest
                end
          in
          loop tokens
        in
        begin
          match
            find_required [ "[UNK]"; "<unk>" ],
            find_required [ "[CLS]"; "<s>" ],
            find_required [ "[SEP]"; "</s>" ],
            find_required [ "[PAD]"; "<pad>" ]
          with
          | Ok unk_id, Ok cls_id, Ok sep_id, Ok pad_id ->
              Ok { vocab; unk_id; cls_id; sep_id; pad_id; do_lower_case; max_length }
          | _ -> Error "Tokenizer vocab is missing one of [UNK]/[CLS]/[SEP]/[PAD]"
        end

  let tokenize t text : (tokenized_input, string) result =
    let wordpieces =
      tokenize_basic ~do_lower_case:t.do_lower_case text
      |> List.filter (fun token -> token <> "")
      |> List.concat_map (wordpiece_ids t)
    in
    let max_content = max 0 (t.max_length - 2) in
    let wordpieces =
      if List.length wordpieces > max_content then
        List.filteri (fun idx _ -> idx < max_content) wordpieces
      else
        wordpieces
    in
    let ids = [ t.cls_id ] @ wordpieces @ [ t.sep_id ] in
    let padded_len = max 2 t.max_length in
    let input_ids = Array.make padded_len t.pad_id in
    let attention_mask = Array.make padded_len 0 in
    let token_type_ids = Array.make padded_len 0 in
    List.iteri
      (fun index id ->
        if index < padded_len then (
          input_ids.(index) <- id;
          attention_mask.(index) <- 1
        ))
      ids;
    Ok { input_ids; attention_mask; token_type_ids }
end

module Onnx_loader = struct
  module Proto = Onnx__Onnx_protoc.Onnx

  type tensor = {
    dims : int array;
    data : float array;
  }

  type model = {
    metadata : Onnx.t;
    graph : Proto.GraphProto.t;
    input_names : string list;
    output_names : string list;
    operator_types : string list;
    initializers : (string, tensor) Hashtbl.t;
  }

  let product dims = Array.fold_left ( * ) 1 dims

  let int32_of_bytes_le raw offset =
    let b0 = Char.code (Bytes.get raw offset) in
    let b1 = Char.code (Bytes.get raw (offset + 1)) in
    let b2 = Char.code (Bytes.get raw (offset + 2)) in
    let b3 = Char.code (Bytes.get raw (offset + 3)) in
    Int32.logor
      (Int32.of_int b0)
      (Int32.logor
         (Int32.shift_left (Int32.of_int b1) 8)
         (Int32.logor
            (Int32.shift_left (Int32.of_int b2) 16)
            (Int32.shift_left (Int32.of_int b3) 24)))

  let int64_of_bytes_le raw offset =
    let open Int64 in
    let byte index = of_int (Char.code (Bytes.get raw (offset + index))) in
    let result = ref zero in
    for index = 0 to 7 do
      result := logor !result (shift_left (byte index) (8 * index))
    done;
    !result

  let floats_of_raw_data ~elem_size ~expected_count decode raw : (float array, string) result =
    if Bytes.length raw <> expected_count * elem_size then
      Error "Raw tensor data length does not match tensor shape"
    else
      let data = Array.init expected_count (fun idx -> decode raw (idx * elem_size)) in
      Ok data

  let tensor_of_proto (proto : Proto.TensorProto.t) : (tensor, string) result =
    let dims = Array.of_list proto.dims in
    let expected_count = product dims in
    let name = Option.value ~default:"<unnamed>" proto.name in
    match proto.data_type with
    | Some 1 ->
        if proto.float_data <> [] then
          Ok { dims; data = Array.of_list proto.float_data }
        else
          begin
            match proto.raw_data with
            | Some raw ->
                floats_of_raw_data
                  ~elem_size:4
                  ~expected_count
                  (fun bytes offset -> Int32.float_of_bits (int32_of_bytes_le bytes offset))
                  raw
                |> Result.map (fun data -> { dims; data })
            | None -> Error ("Tensor " ^ name ^ " has no float payload")
          end
    | Some 6 ->
        if proto.int32_data <> [] then
          Ok { dims; data = Array.of_list (List.map float_of_int proto.int32_data) }
        else
          begin
            match proto.raw_data with
            | Some raw ->
                floats_of_raw_data
                  ~elem_size:4
                  ~expected_count
                  (fun bytes offset -> Int32.to_float (int32_of_bytes_le bytes offset))
                  raw
                |> Result.map (fun data -> { dims; data })
            | None -> Error ("Tensor " ^ name ^ " has no int32 payload")
          end
    | Some 7 ->
        if proto.int64_data <> [] then
          Ok { dims; data = Array.of_list (List.map float_of_int proto.int64_data) }
        else
          begin
            match proto.raw_data with
            | Some raw ->
                floats_of_raw_data
                  ~elem_size:8
                  ~expected_count
                  (fun bytes offset -> Int64.to_float (int64_of_bytes_le bytes offset))
                  raw
                |> Result.map (fun data -> { dims; data })
            | None -> Error ("Tensor " ^ name ^ " has no int64 payload")
          end
    | Some 11 ->
        if proto.double_data <> [] then
          Ok { dims; data = Array.of_list proto.double_data }
        else
          Error ("Tensor " ^ name ^ " uses unsupported raw double payload")
    | Some other ->
        Error (Printf.sprintf "Tensor %s uses unsupported ONNX data_type %d" name other)
    | None -> Error ("Tensor " ^ name ^ " is missing data_type")

  let parse_model_proto path : (Proto.ModelProto.t, string) result =
    let in_channel = Stdlib.open_in_bin path in
    Fun.protect
      ~finally:(fun () -> Stdlib.close_in in_channel)
      (fun () ->
        let buffer = Stdlib.In_channel.input_all in_channel in
        let reader = Ocaml_protoc_plugin.Reader.create buffer in
        Onnx__Onnx_protoc.Onnx.ModelProto.from_proto reader
        |> function
        | Ok model -> Ok model
        | Error _ -> Error ("Failed to parse ONNX protobuf from " ^ path))

  let load model_path : (model, string) result =
    match Onnx.parse model_path, parse_model_proto model_path with
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok metadata, Ok proto_model ->
        begin
          match proto_model.graph with
          | None -> Error ("ONNX model has no graph: " ^ model_path)
          | Some graph ->
              let initializers = Hashtbl.create 128 in
              let rec load_initializers = function
                | [] -> Ok ()
                | tensor_proto :: rest ->
                    begin
                      match tensor_of_proto tensor_proto with
                      | Ok tensor ->
                          begin
                            match tensor_proto.name with
                            | Some name -> Hashtbl.replace initializers name tensor
                            | None -> ()
                          end;
                          load_initializers rest
                      | Error err -> Error err
                    end
              in
              begin
                match load_initializers graph.initializer' with
                | Error _ as err -> err
                | Ok () ->
                    let input_names =
                      List.filter_map (fun (info : Proto.ValueInfoProto.t) -> info.name) graph.input
                    in
                    let output_names =
                      List.filter_map (fun (info : Proto.ValueInfoProto.t) -> info.name) graph.output
                    in
                    let operator_types =
                      List.filter_map (fun (node : Proto.NodeProto.t) -> node.op_type) graph.node
                    in
                    Ok { metadata; graph; input_names; output_names; operator_types; initializers }
              end
        end
end

type encoder = {
  model_name : string;
  dimensions : int;
  embed_text : string -> (vector, string) result;
}

let default_model =
  {
    name = "bge-small-en-v1.5";
    dimensions = 384;
    normalize = true;
  }

module Bge_encoder = struct
  type t = {
    tokenizer : Tokenizer.t;
    model_name : string;
    dimensions : int;
    word_embeddings : Onnx_loader.tensor;
    position_embeddings : Onnx_loader.tensor option;
    token_type_embeddings : Onnx_loader.tensor option;
    layer_norm_weight : Onnx_loader.tensor option;
    layer_norm_bias : Onnx_loader.tensor option;
  }

  let tensor_rank tensor = Array.length tensor.Onnx_loader.dims

  let row_width tensor : (int, string) result =
    if tensor_rank tensor <> 2 then
      Error "Expected a rank-2 embedding tensor"
    else
      Ok tensor.Onnx_loader.dims.(1)

  let find_initializer_by_suffix initializers suffixes =
    let result = ref None in
    Hashtbl.iter
      (fun name tensor ->
        if !result = None && List.exists (fun suffix -> Filename.check_suffix name suffix) suffixes then
          result := Some tensor)
      initializers;
    !result

  let zero_row dims = Array.make dims 0.0

  let extract_row tensor row_index : (float array, string) result =
    match row_width tensor with
    | Error _ as err -> err
    | Ok width ->
        if row_index < 0 || row_index >= tensor.Onnx_loader.dims.(0) then
          Error (Printf.sprintf "Embedding row index %d is out of bounds" row_index)
        else
          let start = row_index * width in
          Ok (Array.init width (fun idx -> tensor.Onnx_loader.data.(start + idx)))

  let add_in_place target source =
    for idx = 0 to Array.length target - 1 do
      target.(idx) <- target.(idx) +. source.(idx)
    done

  let layer_norm ?weight ?bias vector =
    let length = Array.length vector in
    if length = 0 then
      vector
    else
      let mean =
        Array.fold_left ( +. ) 0.0 vector /. float_of_int length
      in
      let variance =
        Array.fold_left
          (fun acc value ->
            let centered = value -. mean in
            acc +. (centered *. centered))
          0.0
          vector
        /. float_of_int length
      in
      let denom = sqrt (variance +. 1e-12) in
      Array.mapi
        (fun idx value ->
          let normalized = (value -. mean) /. denom in
          let scaled =
            match weight with
            | Some gamma when idx < Array.length gamma -> normalized *. gamma.(idx)
            | _ -> normalized
          in
          match bias with
          | Some beta when idx < Array.length beta -> scaled +. beta.(idx)
          | _ -> scaled)
        vector

  let l2_normalize vector =
    let magnitude =
      sqrt (Array.fold_left (fun acc value -> acc +. (value *. value)) 0.0 vector)
    in
    if magnitude <= 1e-12 then
      vector
    else
      Array.map (fun value -> value /. magnitude) vector

  let load ~model_dir : (t, string) result =
    let model_path = Filename.concat model_dir "model.onnx" in
    if not (Sys.file_exists model_path) then
      Error ("Embedding model asset not found: " ^ model_path)
    else
      match Tokenizer.load model_dir, Onnx_loader.load model_path with
      | Error err, _ | _, Error err -> Error err
      | Ok tokenizer, Ok model ->
          let word_embeddings =
            find_initializer_by_suffix
              model.Onnx_loader.initializers
              [ "embeddings.word_embeddings.weight"; "word_embeddings.weight" ]
          in
          begin
            match word_embeddings with
            | None ->
                Error "ONNX model does not contain embeddings.word_embeddings.weight"
            | Some word_embeddings ->
                begin
                  match row_width word_embeddings with
                  | Error _ as err -> err
                  | Ok dimensions ->
                      let model_name =
                        let base = Filename.basename model_dir in
                        if base = "" || base = "." then default_model.name else base
                      in
                      let extract_vector tensor_opt =
                        match tensor_opt with
                        | None -> None
                        | Some tensor when Array.length tensor.Onnx_loader.dims = 1 ->
                            Some tensor.Onnx_loader.data
                        | Some _ -> None
                      in
                      Ok
                        {
                          tokenizer;
                          model_name;
                          dimensions;
                          word_embeddings;
                          position_embeddings =
                            find_initializer_by_suffix
                              model.Onnx_loader.initializers
                              [ "embeddings.position_embeddings.weight"; "position_embeddings.weight" ];
                          token_type_embeddings =
                            find_initializer_by_suffix
                              model.Onnx_loader.initializers
                              [ "embeddings.token_type_embeddings.weight"; "token_type_embeddings.weight" ];
                          layer_norm_weight =
                            find_initializer_by_suffix
                              model.Onnx_loader.initializers
                              [ "embeddings.LayerNorm.weight"; "LayerNorm.weight" ];
                          layer_norm_bias =
                            find_initializer_by_suffix
                              model.Onnx_loader.initializers
                              [ "embeddings.LayerNorm.bias"; "LayerNorm.bias" ];
                        }
                end
          end

  let embed t text : (vector, string) result =
    match Tokenizer.tokenize t.tokenizer text with
    | Error _ as err -> err
    | Ok tokens ->
        let total = zero_row t.dimensions in
        let valid_tokens = ref 0 in
        let layer_norm_weight =
          match t.layer_norm_weight with
          | Some tensor when Array.length tensor.Onnx_loader.dims = 1 -> Some tensor.Onnx_loader.data
          | _ -> None
        in
        let layer_norm_bias =
          match t.layer_norm_bias with
          | Some tensor when Array.length tensor.Onnx_loader.dims = 1 -> Some tensor.Onnx_loader.data
          | _ -> None
        in
        let rec process index =
          if index >= Array.length tokens.input_ids then
            Ok ()
          else if tokens.attention_mask.(index) = 0 then
            process (index + 1)
          else
            match extract_row t.word_embeddings tokens.input_ids.(index) with
            | Error _ as err -> err
            | Ok vector ->
                begin
                  let with_position =
                    match t.position_embeddings with
                    | Some tensor ->
                        begin
                          match extract_row tensor index with
                          | Ok row ->
                              add_in_place vector row;
                              Ok ()
                          | Error _ as err -> err
                        end
                    | None -> Ok ()
                  in
                  match with_position with
                  | Error _ as err -> err
                  | Ok () ->
                      begin
                        match t.token_type_embeddings with
                        | Some tensor ->
                            begin
                              match extract_row tensor tokens.token_type_ids.(index) with
                              | Ok row -> add_in_place vector row
                              | Error _ -> ()
                            end
                        | None -> ()
                      end;
                      let vector = layer_norm ?weight:layer_norm_weight ?bias:layer_norm_bias vector in
                      add_in_place total vector;
                      incr valid_tokens;
                      process (index + 1)
                end
        in
        begin
          match process 0 with
          | Error _ as err -> err
          | Ok () ->
              if !valid_tokens = 0 then
                Error "Cannot embed empty token sequence"
              else
                let averaged =
                  Array.map (fun value -> value /. float_of_int !valid_tokens) total
                  |> l2_normalize
                in
                Ok (Array.to_list averaged)
        end
end

let cosine_similarity vec1 vec2 =
  if List.length vec1 <> List.length vec2 then
    0.0
  else
    List.fold_left2 (fun acc a b -> acc +. (a *. b)) 0.0 vec1 vec2

let vector_to_json vec =
  let floats = List.map string_of_float vec in
  "[" ^ String.concat "," floats ^ "]"

let vector_of_json json_str =
  try
    match Yojson.Safe.from_string json_str with
    | `List values ->
        List.map
          (function
            | `Float value -> value
            | `Int value -> float_of_int value
            | _ -> 0.0)
          values
    | _ -> []
  with _ -> []

let cosine_similarity_udf arg1 arg2 =
  match arg1, arg2 with
  | Sqlite3.Data.TEXT json1, Sqlite3.Data.TEXT json2 ->
      let vec1 = vector_of_json json1 in
      let vec2 = vector_of_json json2 in
      if vec1 = [] || vec2 = [] then
        Sqlite3.Data.NULL
      else
        Sqlite3.Data.FLOAT (cosine_similarity vec1 vec2)
  | _ -> Sqlite3.Data.NULL

let register_udfs db : (unit, string) result =
  try
    create_fun2 db "cosine_similarity" cosine_similarity_udf;
    Log.info (fun m -> m "Registered vector search UDFs");
    Ok ()
  with
  | Sqlite3.SqliteError msg ->
      Log.err (fun m -> m "Failed to register UDFs: %s" msg);
      Error msg
  | exn ->
      Log.err (fun m -> m "Failed to register UDFs: %s" (Printexc.to_string exn));
      Error (Printexc.to_string exn)

let init_schema db =
  let queries =
    [
      "CREATE TABLE IF NOT EXISTS embeddings (\
       id INTEGER PRIMARY KEY AUTOINCREMENT,\
       chat_id INTEGER,\
       memory_id INTEGER,\
       text_content TEXT NOT NULL,\
       embedding TEXT NOT NULL,\
       dimensions INTEGER NOT NULL,\
       model_name TEXT NOT NULL,\
       created_at REAL DEFAULT (strftime('%s', 'now')),\
       metadata TEXT\
       )";
      "CREATE INDEX IF NOT EXISTS idx_embeddings_chat ON embeddings(chat_id)";
      "CREATE INDEX IF NOT EXISTS idx_embeddings_memory ON embeddings(memory_id)";
      "CREATE INDEX IF NOT EXISTS idx_embeddings_created ON embeddings(created_at)";
    ]
  in
  List.iter
    (fun sql ->
      try
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | Sqlite3.Rc.ERROR -> Log.err (fun m -> m "Schema error: %s" (Sqlite3.errmsg db))
        | rc -> Log.err (fun m -> m "Schema error (%s): %s" (Rc.to_string rc) (Sqlite3.errmsg db))
      with Sqlite3.SqliteError msg ->
        Log.err (fun m -> m "Schema exception: %s" msg))
    queries

let configured_model_dir () =
  match Sys.getenv_opt "OCLAW_EMBEDDING_MODEL_DIR" with
  | Some path when String.trim path <> "" -> Some path
  | _ ->
      let candidates =
        [
          Filename.concat "workspace" (Filename.concat "models" default_model.name);
          Filename.concat "models" default_model.name;
        ]
      in
      List.find_opt Sys.file_exists candidates

let cached_encoder : (encoder, string) result option ref = ref None

let load_encoder ~model_dir : (encoder, string) result =
  match Bge_encoder.load ~model_dir with
  | Error _ as err -> err
  | Ok backend ->
      Ok
        {
          model_name = backend.Bge_encoder.model_name;
          dimensions = backend.Bge_encoder.dimensions;
          embed_text = Bge_encoder.embed backend;
        }

let embed encoder text : (vector, string) result = encoder.embed_text text

let get_default_encoder () : (encoder, string) result =
  match !cached_encoder with
  | Some cached -> cached
  | None ->
      let result =
        match configured_model_dir () with
        | Some model_dir -> load_encoder ~model_dir
        | None ->
            Error
              "No embedding model configured. Set OCLAW_EMBEDDING_MODEL_DIR to a BGE model bundle."
      in
      cached_encoder := Some result;
      result

let embed_text ?model_dir text : (vector, string) result =
  match model_dir with
  | Some model_dir ->
      begin
        match load_encoder ~model_dir with
        | Ok encoder -> embed encoder text
        | Error _ as err -> err
      end
  | None ->
      begin
        match get_default_encoder () with
        | Ok encoder -> embed encoder text
        | Error _ as err -> err
      end

let generate_embedding ?model_dir text : (vector, string) result = embed_text ?model_dir text

let bind_all stmt values =
  Array.iteri (fun idx data -> ignore (Sqlite3.bind stmt (idx + 1) data)) values

let finalize_ignore stmt =
  ignore (Sqlite3.finalize stmt)

let store_embedding db ~chat_id ~memory_id text : (unit, string) result =
  match get_default_encoder (), generate_embedding text with
  | Error err, _ -> Error err
  | _, Error err -> Error err
  | Ok encoder, Ok embedding ->
      let embedding_json = vector_to_json embedding in
      let sql =
        "INSERT INTO embeddings (chat_id, memory_id, text_content, embedding, dimensions, model_name) \
         VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
      in
      try
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            bind_all stmt
              [|
                Data.INT (Int64.of_int chat_id);
                Data.INT (Int64.of_int memory_id);
                Data.TEXT text;
                Data.TEXT embedding_json;
                Data.INT (Int64.of_int encoder.dimensions);
                Data.TEXT encoder.model_name;
              |];
            match Sqlite3.step stmt with
            | Rc.DONE | Rc.ROW -> Ok ()
            | rc ->
                Error (Printf.sprintf "sqlite step failed (%s): %s" (Rc.to_string rc) (Sqlite3.errmsg db)))
      with Sqlite3.SqliteError msg ->
        Error (Printf.sprintf "sqlite store_embedding exception: %s" msg)

let search_similar db ~chat_id query_text ~limit ~threshold : (search_result list, string) result =
  match generate_embedding query_text with
  | Error _ as err -> err
  | Ok query_embedding ->
      let query_json = vector_to_json query_embedding in
      let sql =
        "SELECT id, memory_id, text_content, cosine_similarity(embedding, ?2) as similarity \
         FROM embeddings \
         WHERE chat_id = ?1 \
         ORDER BY similarity DESC \
         LIMIT ?3"
      in
      try
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            bind_all stmt
              [|
                Data.INT (Int64.of_int chat_id);
                Data.TEXT query_json;
                Data.INT (Int64.of_int limit);
              |];
            let int_column stmt index =
              match Sqlite3.column stmt index with
              | Data.INT value -> Int64.to_int value
              | _ -> 0
            in
            let text_column stmt index =
              match Sqlite3.column stmt index with
              | Data.TEXT value -> value
              | _ -> ""
            in
            let float_column stmt index =
              match Sqlite3.column stmt index with
              | Data.FLOAT value -> value
              | Data.INT value -> Int64.to_float value
              | _ -> 0.0
            in
            let rec collect acc =
              match Sqlite3.step stmt with
              | Rc.ROW ->
                  let id = int_column stmt 0 in
                  let memory_id = int_column stmt 1 in
                  let text = text_column stmt 2 in
                  let similarity = float_column stmt 3 in
                  let acc =
                    if similarity >= threshold then
                      (id, memory_id, text, similarity) :: acc
                    else
                      acc
                  in
                  collect acc
              | Rc.DONE -> Ok (List.rev acc)
              | rc -> Error (Printf.sprintf "sqlite search step failed (%s)" (Rc.to_string rc))
            in
            collect [])
      with Sqlite3.SqliteError msg ->
        Error (Printf.sprintf "sqlite search_similar exception: %s" msg)

let batch_embed_memories db ~chat_id memories : (unit, string) result =
  let rec process = function
    | [] -> Ok ()
    | (memory_id, text) :: rest ->
        begin
          match store_embedding db ~chat_id ~memory_id text with
          | Ok () -> process rest
          | Error _ as err -> err
        end
  in
  process memories

let get_stats db ~chat_id : (stats, string) result =
  let sql =
    "SELECT COUNT(*), AVG(LENGTH(embedding)), MIN(created_at), MAX(created_at) \
     FROM embeddings WHERE chat_id = ?1"
  in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        bind_all stmt [| Data.INT (Int64.of_int chat_id) |];
        match Sqlite3.step stmt with
        | Rc.ROW ->
            let count =
              match Sqlite3.column stmt 0 with
              | Data.INT value -> Int64.to_int value
              | _ -> 0
            in
            let avg_size =
              match Sqlite3.column stmt 1 with
              | Data.FLOAT value -> value
              | Data.INT value -> Int64.to_float value
              | _ -> 0.0
            in
            let opt_float index =
              match Sqlite3.column stmt index with
              | Data.FLOAT value -> Some value
              | Data.INT value -> Some (Int64.to_float value)
              | _ -> None
            in
            let created_min = opt_float 2 in
            let created_max = opt_float 3 in
            Ok { count; avg_size; created_min; created_max }
        | _ ->
            Ok { count = 0; avg_size = 0.0; created_min = None; created_max = None })
  with Sqlite3.SqliteError msg ->
    Error (Printf.sprintf "sqlite get_stats exception: %s" msg)

let cleanup_old db ~chat_id ~before_timestamp : (int, string) result =
  let sql = "DELETE FROM embeddings WHERE chat_id = ?1 AND created_at < ?2" in
  try
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        bind_all stmt
          [|
            Data.INT (Int64.of_int chat_id);
            Data.FLOAT before_timestamp;
          |];
        match Sqlite3.step stmt with
        | Rc.DONE ->
            Ok (Sqlite3.changes db)
        | rc ->
            Error (Printf.sprintf "sqlite cleanup step failed (%s): %s" (Rc.to_string rc) (Sqlite3.errmsg db)))
  with Sqlite3.SqliteError msg ->
    Error (Printf.sprintf "sqlite cleanup_old exception: %s" msg)
