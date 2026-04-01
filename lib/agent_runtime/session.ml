let process ~emit app ~chat_id ?(persistent=false) prompt =
  Agent_engine.process ~emit (App.internal_state app) ~chat_id ~persistent prompt

let resolve_permission app ~chat_id outcome =
  Agent_engine.resolve_permission (App.internal_state app) ~chat_id outcome

let history (app : App.t) ~chat_id =
  let state = App.internal_state app in
  match Transcript.get_latest_node state.Runtime.transcript ~chat_id with
  | Some node_id -> Transcript.get_branch state.Runtime.transcript node_id
  | None -> []

let latest_node_id (app : App.t) ~chat_id =
  Transcript.get_latest_node (App.internal_state app).Runtime.transcript ~chat_id

let create_conversation (app : App.t) ?title () =
  Transcript.create_conversation (App.internal_state app).Runtime.transcript ?title ()

let fork_latest_conversation (app : App.t) ~chat_id ?title () =
  let transcript = (App.internal_state app).Runtime.transcript in
  match Transcript.get_latest_node transcript ~chat_id with
  | None -> Error "No conversation history to fork."
  | Some node_id -> Ok (Transcript.fork_conversation transcript ~chat_id node_id ?title ())

let approve_read (app : App.t) path =
  Tools.approve_root (App.internal_state app).Runtime.tools ~scope:Tools.Read path

let approve_write (app : App.t) path =
  Tools.approve_root (App.internal_state app).Runtime.tools ~scope:Tools.Write path

let approve_exec (app : App.t) path =
  Tools.approve_executable (App.internal_state app).Runtime.tools path

let approve_install (app : App.t) name =
  Tools.approve_install (App.internal_state app).Runtime.tools name

let list_permissions ?scope (app : App.t) =
  Tools.list_approvals_formatted ?scope (App.internal_state app).Runtime.tools

let trust_project ?path (app : App.t) =
  Tools.trust_project ?path (App.internal_state app).Runtime.tools

let available_skills (app : App.t) =
  Tools.visible_skills (App.internal_state app).Runtime.tools
  |> List.map (fun (skill : Agent_skills.Skills.skill_metadata) -> skill.name)

let list_skills ?(include_untrusted=false) (app : App.t) =
  Tools.list_skills_formatted ~include_untrusted (App.internal_state app).Runtime.tools

let activate_skill (app : App.t) ~chat_id name =
  let result = Tools.activate_skill (App.internal_state app).Runtime.tools ~chat_id name in
  if result.Tools.is_error then Error result.content else Ok result.content
