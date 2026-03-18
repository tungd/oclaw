let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let temp_dir () =
  let path = Filename.temp_file "oclaw-vector-search-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let write_text path content =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p (Filename.dirname path);
  Stdlib.Out_channel.with_open_bin path (fun channel -> output_string channel content)

let write_binary path content =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p (Filename.dirname path);
  Stdlib.Out_channel.with_open_bin path (fun channel -> output_string channel content)

let make_dim value =
  Onnx__Onnx_protoc.Onnx.TensorShapeProto.Dimension.make ~value:(`Dim_value value) ()

let make_tensor_info name dims =
  let module P = Onnx__Onnx_protoc.Onnx in
  P.ValueInfoProto.make
    ~name
    ~type':
      (P.TypeProto.make
         ~value:(`Tensor_type (P.TypeProto.Tensor.make ~elem_type:1 ~shape:(List.map make_dim dims) ()))
         ())
    ()

let make_float_tensor name dims data =
  let module P = Onnx__Onnx_protoc.Onnx in
  P.TensorProto.make ~name ~dims ~data_type:1 ~float_data:data ()

let write_test_model model_dir =
  let module P = Onnx__Onnx_protoc.Onnx in
  let vocab =
    [
      "[PAD]";
      "[UNK]";
      "[CLS]";
      "[SEP]";
      "cat";
      "animal";
      "pet";
      "finance";
      "money";
    ]
  in
  write_text (Filename.concat model_dir "vocab.txt") (String.concat "\n" vocab ^ "\n");
  write_text
    (Filename.concat model_dir "tokenizer_config.json")
    {|{"do_lower_case": true, "model_max_length": 8}|};
  write_text
    (Filename.concat model_dir "config.json")
    {|{"max_position_embeddings": 8, "hidden_size": 3}|};
  let word_embeddings =
    make_float_tensor
      "embeddings.word_embeddings.weight"
      [ List.length vocab; 3 ]
      [
        0.; 0.; 0.;
        0.; 0.; 0.;
        0.; 0.; 0.;
        0.; 0.; 0.;
        1.; 0.; 0.;
        1.; 0.1; 0.;
        1.; 0.1; 0.05;
        0.; 1.; 0.;
        0.; 1.; 0.1;
      ]
  in
  let position_embeddings =
    make_float_tensor
      "embeddings.position_embeddings.weight"
      [ 8; 3 ]
      (List.init (8 * 3) (fun _ -> 0.))
  in
  let token_type_embeddings =
    make_float_tensor
      "embeddings.token_type_embeddings.weight"
      [ 2; 3 ]
      (List.init (2 * 3) (fun _ -> 0.))
  in
  let graph =
    P.GraphProto.make
      ~name:"tiny-bge"
      ~initializer':[ word_embeddings; position_embeddings; token_type_embeddings ]
      ~input:
        [
          make_tensor_info "input_ids" [ 1; 8 ];
          make_tensor_info "attention_mask" [ 1; 8 ];
          make_tensor_info "token_type_ids" [ 1; 8 ];
        ]
      ~output:[ make_tensor_info "sentence_embedding" [ 1; 3 ] ]
      ()
  in
  let model =
    P.ModelProto.make
      ~producer_name:"oclaw-test"
      ~opset_import:[ P.OperatorSetIdProto.make ~version:17 () ]
      ~graph
      ()
  in
  let bytes = P.ModelProto.to_proto model |> Ocaml_protoc_plugin.Writer.contents in
  write_binary (Filename.concat model_dir "model.onnx") bytes

let unwrap = function
  | Ok value -> value
  | Error err -> fail err

let dot a b =
  List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 a b

let test_tokenizer_golden model_dir =
  let tokenizer = unwrap (Vector_search.Tokenizer.load model_dir) in
  let encoded = unwrap (Vector_search.Tokenizer.tokenize tokenizer "Cat pet") in
  let ids = Array.to_list encoded.Vector_search.input_ids in
  let mask = Array.to_list encoded.Vector_search.attention_mask in
  expect (List.length ids = 8) "tokenizer should pad to max length";
  expect (List.filteri (fun idx _ -> idx < 4) ids = [ 2; 4; 6; 3 ]) "unexpected token ids";
  expect (List.filteri (fun idx _ -> idx < 4) mask = [ 1; 1; 1; 1 ]) "unexpected attention mask"

let test_encoder_semantics model_dir =
  let encoder = unwrap (Vector_search.load_encoder ~model_dir) in
  let cat_animal = unwrap (Vector_search.embed encoder "cat animal") in
  let cat_pet = unwrap (Vector_search.embed encoder "cat pet") in
  let finance_money = unwrap (Vector_search.embed encoder "finance money") in
  expect (List.length cat_animal = 3) "embedding dimension should match hidden size";
  List.iter
    (fun value -> expect (Float.is_finite value) "embedding contains non-finite value")
    cat_animal;
  let repeat = unwrap (Vector_search.embed encoder "cat animal") in
  expect (cat_animal = repeat) "embeddings should be stable across calls";
  expect (dot cat_animal cat_pet > dot cat_animal finance_money) "related text should be more similar than unrelated text"

let test_db_search model_dir =
  Unix.putenv "OCLAW_EMBEDDING_MODEL_DIR" model_dir;
  let db_path = Filename.concat model_dir "vectors.db" in
  let db = Sqlite3.db_open db_path in
  ignore (Vector_search.register_udfs db);
  Vector_search.init_schema db;
  ignore (unwrap (Vector_search.store_embedding db ~chat_id:1 ~memory_id:10 "cat animal"));
  ignore (unwrap (Vector_search.store_embedding db ~chat_id:1 ~memory_id:20 "finance money"));
  let results = unwrap (Vector_search.search_similar db ~chat_id:1 "cat pet" ~limit:5 ~threshold:(-1.0)) in
  expect (List.length results >= 1) "search should return stored embeddings";
  let _, memory_id, _, _ = List.hd results in
  expect (memory_id = 10) "semantic search should rank the related memory first";
  ignore (Sqlite3.db_close db)

let test_missing_assets () =
  let broken = temp_dir () in
  write_text (Filename.concat broken "vocab.txt") "[PAD]\n[UNK]\n[CLS]\n[SEP]\n";
  match Vector_search.load_encoder ~model_dir:broken with
  | Ok _ -> fail "load_encoder should fail when model.onnx is missing"
  | Error _ -> ()

let () =
  let model_dir = temp_dir () in
  write_test_model model_dir;
  test_tokenizer_golden model_dir;
  test_encoder_semantics model_dir;
  test_db_search model_dir;
  test_missing_assets ();
  Printf.printf "[PASS] vector search tests\n"
