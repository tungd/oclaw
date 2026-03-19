(** Configuration tests: env overrides, validation, save/load. *)

module Config = Agent_core.Config

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

(* ============================================================================
   Environment Overrides
   ============================================================================ *)

let test_env_overrides () =
  Unix.putenv "OCLAW_MODEL" "env-model";
  Unix.putenv "OCLAW_DATA_DIR" "/tmp/oclaw-data";
  Unix.putenv "OCLAW_MAX_TOOL_ITERATIONS" "500";
  Unix.putenv "OCLAW_DEBUG" "true";
  let config = Config.from_env () in
  expect (String.equal config.llm_model "env-model") "model env override failed";
  expect (String.equal config.data_dir "/tmp/oclaw-data") "data_dir env override failed";
  expect (config.max_tool_iterations = 500) "max_tool_iterations env override failed";
  expect config.debug "debug env override failed";
  print_endline "  ✓ env_overrides"

(* ============================================================================
   Validation
   ============================================================================ *)

let test_validation () =
  (* Missing API key *)
  let config1 = { Config.default_config with llm_api_key = "" } in
  match Config.validate config1 with
  | Ok _ -> fail "expected missing API key to fail validation"
  | Error errors -> expect (List.length errors >= 1) "expected API key error";
  
  (* Invalid max_tool_iterations *)
  let config2 = { Config.default_config with max_tool_iterations = 0 } in
  match Config.validate config2 with
  | Ok _ -> fail "expected zero max_tool_iterations to fail validation"
  | Error errors -> expect (List.length errors >= 1) "expected max_tool_iterations error";
  
  (* Multiple errors *)
  let config3 = { Config.default_config with llm_api_key = ""; max_tool_iterations = 0 } in
  match Config.validate config3 with
  | Ok _ -> fail "expected invalid config to fail validation"
  | Error errors -> expect (List.length errors >= 2) "expected multiple validation errors";
  print_endline "  ✓ validation"

(* ============================================================================
   Save/Load Round-trip
   ============================================================================ *)

let test_save_load () =
  print_string "  Running save_load... "; flush stdout;
  let temp_file = Filename.temp_file "oclaw_config_test" ".yaml" in
  try
    let original = {
      Config.default_config with
      llm_model = "test-model-123";
      max_tool_iterations = 500;
      debug = true;
    } in
    let saved = Config.save temp_file original in
    if not saved then fail "save returned false";
    let loaded = Config.from_file temp_file in
    expect (loaded.llm_model = "test-model-123") "model not preserved";
    expect (loaded.max_tool_iterations = 500) "max_tool_iterations not preserved";
    expect loaded.debug "debug flag not preserved";
    Unix.unlink temp_file;
    print_endline "✓"
  with exn ->
    (try Unix.unlink temp_file with _ -> ());
    fail (Printf.sprintf "save/load test exception: %s" (Printexc.to_string exn))

(* ============================================================================
   Merge Priority
   ============================================================================ *)

let test_merge () =
  let file_config = {
    Config.default_config with
    llm_model = "file-model";
    llm_api_key = "file-key";
  } in
  let env_config = {
    Config.default_config with
    llm_model = "env-model";
    data_dir = "/env/data";
  } in
  let args_config = {
    Config.default_config with
    llm_model = "args-model";
    debug = true;
  } in
  let merged = Config.merge [file_config; env_config; args_config] in
  expect (merged.llm_model = "args-model") "args should override env and file";
  expect (merged.llm_api_key = "file-key") "file value preserved when not overridden";
  expect (merged.data_dir = "/env/data") "env value preserved when not overridden";
  expect merged.debug "debug from args";
  print_endline "  ✓ merge"

(* ============================================================================
   Default Config Sanity
   ============================================================================ *)

let test_defaults () =
  let cfg = Config.default_config in
  expect (cfg.llm_model = "qwen3.5-plus") "default model wrong";
  expect (cfg.max_tool_iterations = 256) "default tool iterations wrong";
  expect (not cfg.debug) "debug should default to false";
  print_endline "  ✓ defaults"

(* ============================================================================
   Main
   ============================================================================ *)

let () =
  print_endline "Running config tests...";
  test_defaults ();
  test_env_overrides ();
  test_validation ();
  test_save_load ();
  test_merge ();
  print_endline "[PASS] all config tests ✓"
