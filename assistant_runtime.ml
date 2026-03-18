type t = {
  state : Runtime.app_state;
  chat_id : int;
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
  | Ok state -> { state; chat_id }
  | Error err -> failwith err

let query runtime prompt =
  Agent_engine.process runtime.state ~chat_id:runtime.chat_id prompt

let history runtime =
  match Db.get_all_messages runtime.state.db ~chat_id:runtime.chat_id with
  | Ok messages -> messages
  | _ -> []
