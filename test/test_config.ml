(** Configuration tests: precedence, validation, and parse failures. *)

module Config = Agent_runtime.Config

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let expect_ok = function
  | Ok value -> value
  | Error errors ->
      let rendered = String.concat "; " (List.map Config.string_of_error errors) in
      fail ("expected Ok, got Error: " ^ rendered)

let expect_error = function
  | Ok _ -> fail "expected Error, got Ok"
  | Error errors -> errors

let config_env_vars = [
  "OCLAW_MODEL";
  "OCLAW_API_KEY";
  "OCLAW_API_BASE";
  "OCLAW_DATA_DIR";
  "OCLAW_MAX_TOOL_ITERATIONS";
  "OCLAW_DEBUG";
  "OCLAW_API_RETRY_ENABLED";
  "OCLAW_API_RETRY_MAX_RETRIES";
  "OCLAW_API_RETRY_BASE_DELAY_MS";
  "OCLAW_API_RETRY_MAX_DELAY_MS";
]

let with_env bindings f =
  let original =
    List.map (fun key -> (key, Sys.getenv_opt key)) config_env_vars
  in
  let restore () =
    List.iter
      (fun (key, value_opt) ->
        match value_opt with
        | Some value -> Unix.putenv key value
        | None -> Unix.putenv key "")
      original
  in
  List.iter (fun key -> Unix.putenv key "") config_env_vars;
  List.iter (fun (key, value) -> Unix.putenv key value) bindings;
  try
    let result = f () in
    restore ();
    result
  with exn ->
    restore ();
    raise exn

let with_temp_file contents f =
  let path = Filename.temp_file "oclaw_config_test" ".yaml" in
  let cleanup () =
    try Unix.unlink path with _ -> ()
  in
  Out_channel.with_open_text path (fun ch -> output_string ch contents);
  try
    let result = f path in
    cleanup ();
    result
  with exn ->
    cleanup ();
    raise exn

(* ============================================================================
   Defaults and Validation
   ============================================================================ *)

let test_defaults () =
  let cfg = Config.default_config in
  expect (cfg.llm_model = "qwen3.5-plus") "default model wrong";
  expect (cfg.max_tool_iterations = 256) "default tool iterations wrong";
  expect (not cfg.debug) "debug should default to false";
  print_endline "  ✓ defaults"

let test_validation () =
  let config1 = { Config.default_config with llm_api_key = "" } in
  begin
    match Config.validate config1 with
    | Ok _ -> fail "expected missing API key to fail validation"
    | Error errors -> expect (List.length errors >= 1) "expected API key error"
  end;

  let config2 = { Config.default_config with max_tool_iterations = 0 } in
  begin
    match Config.validate config2 with
    | Ok _ -> fail "expected zero max_tool_iterations to fail validation"
    | Error errors -> expect (List.length errors >= 1) "expected max_tool_iterations error"
  end;

  let config3 = { Config.default_config with llm_api_key = ""; max_tool_iterations = 0 } in
  begin
    match Config.validate config3 with
    | Ok _ -> fail "expected invalid config to fail validation"
    | Error errors -> expect (List.length errors >= 2) "expected multiple validation errors"
  end;
  print_endline "  ✓ validation"

(* ============================================================================
   Parsing and Precedence
   ============================================================================ *)

let test_env_overrides () =
  with_env
    [
      ("OCLAW_MODEL", "env-model");
      ("OCLAW_DATA_DIR", "/tmp/oclaw-data");
      ("OCLAW_MAX_TOOL_ITERATIONS", "500");
      ("OCLAW_DEBUG", "true");
    ]
    (fun () ->
      let config = expect_ok (Config.from_env ()) in
      expect (String.equal config.llm_model "env-model") "model env override failed";
      expect (String.equal config.data_dir "/tmp/oclaw-data") "data_dir env override failed";
      expect (config.max_tool_iterations = 500) "max_tool_iterations env override failed";
      expect config.debug "debug env override failed");
  print_endline "  ✓ env_overrides"

let test_save_load_round_trip () =
  with_env [] (fun () ->
    with_temp_file "" (fun path ->
      let original = {
        Config.default_config with
        llm_model = "test-model-123";
        llm_api_key = "test-key";
        max_tool_iterations = 500;
        debug = true;
      } in
      expect (Config.save path original) "save returned false";
      let loaded = expect_ok (Config.load ~config_file:path ()) in
      expect (loaded.llm_model = "test-model-123") "model not preserved";
      expect (loaded.llm_api_key = "test-key") "api key not preserved";
      expect (loaded.max_tool_iterations = 500) "max_tool_iterations not preserved";
      expect loaded.debug "debug flag not preserved"));
  print_endline "  ✓ save_load_round_trip"

let test_precedence_file_env_cli () =
  with_temp_file
    "llm_model: file-model\nllm_api_key: file-key\ndata_dir: /file/data\n"
    (fun path ->
      with_env
        [
          ("OCLAW_MODEL", "env-model");
          ("OCLAW_DATA_DIR", "/env/data");
        ]
        (fun () ->
          let config =
            expect_ok
              (Config.load
                 ~config_file:path
                 ~cli_args:["--model"; "args-model"; "--debug"]
                 ())
          in
          expect (config.llm_model = "args-model") "CLI should override env and file";
          expect (config.llm_api_key = "file-key") "file value should remain when not overridden";
          expect (config.data_dir = "/env/data") "env should override file";
          expect config.debug "CLI debug should apply"));
  print_endline "  ✓ precedence_file_env_cli"

let test_explicit_default_override_is_preserved () =
  with_temp_file "llm_model: custom-model\nllm_api_key: file-key\n" (fun path ->
    with_env
      [("OCLAW_MODEL", Config.default_config.llm_model)]
      (fun () ->
        let config = expect_ok (Config.load ~config_file:path ()) in
        expect
          (config.llm_model = Config.default_config.llm_model)
          "explicit env default should override lower-precedence non-default value"));
  print_endline "  ✓ explicit_default_override"

let test_same_non_default_across_layers () =
  with_temp_file "llm_model: shared-model\nllm_api_key: file-key\n" (fun path ->
    with_env
      [("OCLAW_MODEL", "shared-model")]
      (fun () ->
        let config = expect_ok (Config.load ~config_file:path ()) in
        expect (config.llm_model = "shared-model") "same non-default value should remain stable"));
  print_endline "  ✓ same_non_default_across_layers"

(* ============================================================================
   Parse Failures
   ============================================================================ *)

let test_cli_invalid_int_is_error () =
  let errors = expect_error (Config.from_args ["--max-tool-iterations"; "abc"]) in
  let err = List.hd errors in
  expect (List.length errors = 1) "expected one CLI parse error";
  expect (String.equal err.key "--max-tool-iterations") "expected max-tool-iterations key";
  print_endline "  ✓ cli_invalid_int_is_error"

let test_cli_missing_value_is_error () =
  let errors = expect_error (Config.from_args ["--api-base"]) in
  let err = List.hd errors in
  expect (List.length errors = 1) "expected one missing-value error";
  expect (String.equal err.key "--api-base") "expected api-base key";
  print_endline "  ✓ cli_missing_value_is_error"

let test_env_invalid_bool_is_error () =
  with_env
    [("OCLAW_DEBUG", "maybe")]
    (fun () ->
      let errors = expect_error (Config.from_env ()) in
      let err = List.hd errors in
      expect (List.length errors = 1) "expected one env parse error";
      expect (String.equal err.key "OCLAW_DEBUG") "expected OCLAW_DEBUG key");
  print_endline "  ✓ env_invalid_bool_is_error"

let test_file_invalid_field_is_error () =
  with_temp_file "max_tool_iterations: nope\n" (fun path ->
    let errors = expect_error (Config.from_file path) in
    let err = List.hd errors in
    expect (List.length errors = 1) "expected one file parse error";
    expect (String.equal err.key "max_tool_iterations") "expected max_tool_iterations key");
  print_endline "  ✓ file_invalid_field_is_error"

let test_explicit_missing_file_is_error () =
  with_env [] (fun () ->
    let path = Filename.concat (Filename.get_temp_dir_name ()) "oclaw_missing_config.yaml" in
    let errors = expect_error (Config.load ~config_file:path ()) in
    expect (List.length errors = 1) "expected missing file error");
  print_endline "  ✓ explicit_missing_file_is_error"

let test_load_accumulates_errors () =
  with_env
    [("OCLAW_MAX_TOOL_ITERATIONS", "bad")]
    (fun () ->
      let errors =
        expect_error
          (Config.load
             ~config_file:"/tmp/does-not-exist-oclaw-config.yaml"
             ~cli_args:["--api-retry-enabled"; "wat"]
             ())
      in
      expect (List.length errors = 3) "expected errors from file, env, and CLI");
  print_endline "  ✓ load_accumulates_errors"

(* ============================================================================
   Main
   ============================================================================ *)

let () =
  print_endline "Running config tests...";
  test_defaults ();
  test_env_overrides ();
  test_validation ();
  test_save_load_round_trip ();
  test_precedence_file_env_cli ();
  test_explicit_default_override_is_preserved ();
  test_same_non_default_across_layers ();
  test_cli_invalid_int_is_error ();
  test_cli_missing_value_is_error ();
  test_env_invalid_bool_is_error ();
  test_file_invalid_field_is_error ();
  test_explicit_missing_file_is_error ();
  test_load_accumulates_errors ();
  print_endline "[PASS] all config tests ✓"
