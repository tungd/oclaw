type t = {
  state : Runtime.app_state;
  chat_id : int;
  (* The linear sequence of messages from root to current leaf *)
  mutable active_branch : Llm_types.message list;
  (* The ID of the current leaf node in the tree *)
  mutable current_node_id : int option;
}

type llm_call =
  Runtime.llm_call

let create ?llm_call ?system_prompt ?(chat_id=1) ?data_dir ~provider_config () =
  let config =
    {
      Oclaw_config.Config.default_config with
      llm_model = provider_config.Llm_provider.model.name;
      llm_api_key = provider_config.api_key;
      llm_api_base = provider_config.api_base;
      data_dir = Option.value ~default:"workspace" data_dir;
    }
  in
  match Runtime.create_app_state ?llm_call ?system_prompt_override:system_prompt config with
  | Ok state ->
      let transcript = state.Runtime.transcript in
      (* Load active branch from DB on startup *)
      let active_branch, current_node_id =
        match Transcript.get_latest_node transcript ~chat_id with
        | Some node_id ->
            (Transcript.get_branch transcript node_id, Some node_id)
        | None ->
            ([], None)
      in
      { state; chat_id; active_branch; current_node_id }
  | Error err -> failwith err

let query runtime prompt =
  Agent_engine.process runtime.state ~chat_id:runtime.chat_id ~persistent:true prompt

let history runtime =
  runtime.active_branch

let get_active_branch runtime =
  runtime.active_branch

let get_current_node_id runtime =
  runtime.current_node_id
