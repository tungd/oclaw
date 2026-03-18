let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let test_env_overrides () =
  Unix.putenv "OCLAW_MODEL" "env-model";
  Unix.putenv "OCLAW_WORKSPACE" "/tmp/oclaw-workspace";
  Unix.putenv "OCLAW_ALLOW_READ_PATHS" "/tmp/read-a:/tmp/read-b";
  Unix.putenv "OCLAW_WEB_SEARCH_MAX_RESULTS" "7";
  Unix.putenv "OCLAW_DEBUG" "true";
  let config = Oclaw_config.Config.apply_env_overrides Oclaw_config.Config.default_config in
  expect (String.equal config.llm_model "env-model") "model env override failed";
  expect (String.equal config.tools_workspace "/tmp/oclaw-workspace") "workspace env override failed";
  expect (config.tools_allow_read_paths = [ "/tmp/read-a"; "/tmp/read-b" ]) "allow_read_paths env override failed";
  expect (config.web_search_max_results = 7) "web_search_max_results env override failed";
  expect config.debug "debug env override failed"

let test_validation () =
  let config = { Oclaw_config.Config.default_config with llm_api_key = ""; llm_timeout = 0 } in
  match Oclaw_config.Config.validate_config config with
  | Ok _ -> fail "expected invalid config to fail validation"
  | Error errors ->
      expect (List.length errors >= 2) "expected multiple validation errors"

let () =
  test_env_overrides ();
  test_validation ();
  Printf.printf "[PASS] config tests\n"
