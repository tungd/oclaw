type t = {
  app : App.t;
  chat_id : int;
  active_branch : Llm_types.message list;
  current_node_id : int option;
}

type llm_call =
  App.llm_call

let create ?llm_call ?system_prompt ?(chat_id=1) ?data_dir ~provider_config () =
  let config =
    {
      Config.default_config with
      llm_model = provider_config.Llm_provider.model_name;
      llm_api_key = provider_config.api_key;
      llm_api_base = provider_config.api_base;
      data_dir = Option.value ~default:"workspace" data_dir;
    }
  in
  match App.create ?llm_call ?system_prompt_override:system_prompt config with
  | Ok app ->
      let active_branch = Session.history app ~chat_id in
      let current_node_id = Session.latest_node_id app ~chat_id in
      { app; chat_id; active_branch; current_node_id }
  | Error err -> failwith err

let query runtime prompt =
  let final_message = ref None in
  let runtime_error = ref None in
  let emit = function
    | Acp.Message.Agent_message { content; _ } -> final_message := Some content
    | Acp.Message.Error { message; _ } -> runtime_error := Some message
    | _ -> ()
  in
  match Session.process ~emit runtime.app ~chat_id:runtime.chat_id ~persistent:true prompt with
  | Error err -> Error err
  | Ok () ->
      begin
        match !runtime_error, !final_message with
        | Some err, _ -> Error err
        | None, Some text -> Ok text
        | None, None -> Error "Assistant completed without emitting a final message"
      end

let history runtime =
  runtime.active_branch

let get_active_branch runtime =
  runtime.active_branch

let get_current_node_id runtime =
  runtime.current_node_id
