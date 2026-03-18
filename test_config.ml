(** Configuration tests: env overrides, validation, save/load. *)

module Config = Oclaw_config.Config

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
  Unix.putenv "OCLAW_WORKSPACE" "/tmp/oclaw-workspace";
  Unix.putenv "OCLAW_ALLOW_READ_PATHS" "/tmp/read-a:/tmp/read-b";
  Unix.putenv "OCLAW_WEB_SEARCH_MAX_RESULTS" "7";
  Unix.putenv "OCLAW_DEBUG" "true";
  let config = Config.apply_env_overrides Config.default_config in
  expect (String.equal config.llm_model "env-model") "model env override failed";
  expect (String.equal config.tools_workspace "/tmp/oclaw-workspace") "workspace env override failed";
  expect (config.tools_allow_read_paths = [ "/tmp/read-a"; "/tmp/read-b" ]) "allow_read_paths env override failed";
  expect (config.web_search_max_results = 7) "web_search_max_results env override failed";
  expect config.debug "debug env override failed";
  print_endline "  ✓ env_overrides"

(* ============================================================================
   Validation
   ============================================================================ *)

let test_validation () =
  (* Missing API key *)
  let config1 = { Config.default_config with llm_api_key = "" } in
  match Config.validate_config config1 with
  | Ok _ -> fail "expected missing API key to fail validation"
  | Error errors -> expect (List.length errors >= 1) "expected API key error";
  
  (* Invalid timeout *)
  let config2 = { Config.default_config with llm_timeout = 0 } in
  match Config.validate_config config2 with
  | Ok _ -> fail "expected zero timeout to fail validation"
  | Error errors -> expect (List.length errors >= 1) "expected timeout error";
  
  (* Multiple errors *)
  let config3 = { Config.default_config with llm_api_key = ""; llm_timeout = 0 } in
  match Config.validate_config config3 with
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
      llm_timeout = 99;
      max_history_messages = 50;
      debug = true;
    } in
    let saved = Config.save_config temp_file original in
    if not saved then fail "save_config returned false";
    let loaded = Config.load_config temp_file in
    expect (loaded.llm_model = "test-model-123") "model not preserved";
    expect (loaded.llm_timeout = 99) "timeout not preserved";
    expect (loaded.max_history_messages = 50) "history limit not preserved";
    expect loaded.debug "debug flag not preserved";
    Unix.unlink temp_file;
    print_endline "✓"
  with exn ->
    (try Unix.unlink temp_file with _ -> ());
    fail (Printf.sprintf "save/load test exception: %s" (Printexc.to_string exn))

(* ============================================================================
   Default Config Sanity
   ============================================================================ *)

let test_defaults () =
  let cfg = Config.default_config in
  expect (cfg.llm_provider = "dashscope") "default provider wrong";
  expect (cfg.max_tool_iterations = 256) "default tool iterations wrong";
  expect cfg.tools_restrict_to_workspace "workspace restriction should default to true";
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
  print_endline "[PASS] all config tests ✓"
