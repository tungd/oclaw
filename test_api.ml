(* Simple API test *)
let () =
  let api_key = "sk-sp-cd2e1665a2c44028bdfbdd2d039d42cb" in
  let api_base = "https://coding-intl.dashscope.aliyuncs.com/v1" in
  let model = "qwen3.5-plus" in
  
  Printf.printf "Testing connection to: %s\n" api_base;
  Printf.printf "Model: %s\n" model;
  Printf.printf "API Key: %s...\n" (String.sub api_key 0 10);
  Printf.printf "\nTry running: curl -H \"Authorization: Bearer %s\" %s/models\n" api_key api_base
