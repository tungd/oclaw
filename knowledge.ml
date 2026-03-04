(** Knowledge store for OClaw *)

open Yojson.Basic.Util

module Log = (val Logs.src_log (Logs.Src.create "knowledge") : Logs.LOG)

(* Knowledge type *)
module Knowledge = struct
  type t = {
    mutable identity : Yojson.Basic.t;
    mutable agents : Yojson.Basic.t option;
    mutable soul : Yojson.Basic.t option;
    mutable user : Yojson.Basic.t option;
    mutable skills : Yojson.Basic.t list;
    mutable coding_guidelines : Yojson.Basic.t list;
    mutable last_updated : float;
    mutex : Mutex.t;
  }

  let empty () =
    {
      identity = `Assoc [];
      agents = None;
      soul = None;
      user = None;
      skills = [];
      coding_guidelines = [];
      last_updated = 0.0;
      mutex = Mutex.create ();
    }

  let md_to_json content =
    `Assoc [
      ("content", `String content);
      ("length", `Int (String.length content));
    ]

  let load_file path =
    if Sys.file_exists path then
      try
        let channel = open_in path in
        let content = really_input_string channel (in_channel_length channel) in
        close_in channel;
        Some (md_to_json content)
      with exn ->
        Log.warn (fun m -> m "Failed to load %s: %s" path (Printexc.to_string exn));
        Some (md_to_json "")
    else
      None

  let load_skills_json workspace_dir =
    let skills_dir = Filename.concat workspace_dir "skills" in
    if Sys.file_exists skills_dir && Sys.is_directory skills_dir then begin
      (* Load skills from workspace/skills directory *)
      let skills = Skills.load_skills ~skills_dir () in
      List.map (fun (skill : Skills.Skill.t) ->
        `Assoc [
          ("name", `String skill.Skills.Skill.name);
          ("description", `String skill.Skills.Skill.description);
          ("version", `String skill.Skills.Skill.version);
          ("always", `Bool skill.Skills.Skill.always);
          ("location", `String skill.Skills.Skill.location);
          ("prompts", `List (List.map (fun p -> `String p) skill.Skills.Skill.prompts));
        ]
      ) skills
    end else begin
      (* Fallback to project skills directory *)
      let skills = Skills.load_skills () in
      List.map (fun (skill : Skills.Skill.t) ->
        `Assoc [
          ("name", `String skill.Skills.Skill.name);
          ("description", `String skill.Skills.Skill.description);
          ("version", `String skill.Skills.Skill.version);
          ("always", `Bool skill.Skills.Skill.always);
          ("location", `String skill.Skills.Skill.location);
          ("prompts", `List (List.map (fun p -> `String p) skill.Skills.Skill.prompts));
        ]
      ) skills
    end

  let load ~workspace_dir =
    Log.info (fun m -> m "Loading knowledge from %s" workspace_dir);

    (* Ensure workspace directory exists *)
    if not (Sys.file_exists workspace_dir) then begin
      Log.warn (fun m -> m "Workspace directory does not exist: %s" workspace_dir);
    end;

    let identity = load_file (Filename.concat workspace_dir "IDENTITY.md")
      |> Option.value ~default:(`Assoc [("content", `String "You are OClaw, an AI assistant.")]) in

    let agents = load_file (Filename.concat workspace_dir "AGENTS.md") in
    let soul = load_file (Filename.concat workspace_dir "SOUL.md") in
    let user = load_file (Filename.concat workspace_dir "USER.md") in

    let skills = load_skills_json workspace_dir in

    let knowledge = {
      identity;
      agents;
      soul;
      user;
      skills;
      coding_guidelines = [];
      last_updated = Unix.gettimeofday ();
      mutex = Mutex.create ();
    } in

    Log.info (fun m -> m "Loaded %d skills" (List.length skills));
    knowledge

  let refresh knowledge ~workspace_dir =
    Log.info (fun m -> m "Refreshing knowledge from %s" workspace_dir);

    Mutex.lock knowledge.mutex;

    let identity = load_file (Filename.concat workspace_dir "IDENTITY.md")
      |> Option.value ~default:(`Assoc [("content", `String "You are OClaw, an AI assistant.")]) in

    knowledge.identity <- identity;
    knowledge.agents <- load_file (Filename.concat workspace_dir "AGENTS.md");
    knowledge.soul <- load_file (Filename.concat workspace_dir "SOUL.md");
    knowledge.user <- load_file (Filename.concat workspace_dir "USER.md");
    knowledge.skills <- load_skills_json workspace_dir;

    knowledge.last_updated <- Unix.gettimeofday ();
    Mutex.unlock knowledge.mutex;

    Log.info (fun m -> m "Knowledge refreshed")

  let get_identity knowledge =
    Mutex.lock knowledge.mutex;
    let result = knowledge.identity in
    Mutex.unlock knowledge.mutex;
    result

  let get_agents knowledge =
    Mutex.lock knowledge.mutex;
    let result = knowledge.agents in
    Mutex.unlock knowledge.mutex;
    result

  let get_soul knowledge =
    Mutex.lock knowledge.mutex;
    let result = knowledge.soul in
    Mutex.unlock knowledge.mutex;
    result

  let get_user knowledge =
    Mutex.lock knowledge.mutex;
    let result = knowledge.user in
    Mutex.unlock knowledge.mutex;
    result

  let get_skills knowledge =
    Mutex.lock knowledge.mutex;
    let result = knowledge.skills in
    Mutex.unlock knowledge.mutex;
    result

  let extract_content json =
    try json |> member "content" |> to_string
    with Not_found -> ""

  let get_system_prompt knowledge =
    Mutex.lock knowledge.mutex;

    let parts = ref [] in

    (* Add identity *)
    (try
      let identity_content = extract_content knowledge.identity in
      parts := identity_content :: !parts;
    with _ -> ());

    (* Add agents *)
    (match knowledge.agents with
    | Some agents ->
        let agents_content = extract_content agents in
        if agents_content <> "" then
          parts := agents_content :: !parts
    | None -> ());

    (* Add soul *)
    (match knowledge.soul with
    | Some soul ->
        let soul_content = extract_content soul in
        if soul_content <> "" then
          parts := soul_content :: !parts
    | None -> ());

    (* Add user preferences *)
    (match knowledge.user with
    | Some user ->
        let user_content = extract_content user in
        if user_content <> "" then
          parts := user_content :: !parts
    | None -> ());

    (* Add skills *)
    if knowledge.skills <> [] then
      parts := Skills.skills_to_prompt (
        List.map (fun skill_json ->
          let name = try skill_json |> member "name" |> to_string with _ -> "Unknown" in
          let description = try skill_json |> member "description" |> to_string with _ -> "" in
          let version = try skill_json |> member "version" |> to_string with _ -> "0.1.0" in
          let always = try skill_json |> member "always" |> to_bool with _ -> false in
          let prompts = try skill_json |> member "prompts" |> to_list |> List.map (fun p -> p |> to_string) with _ -> [] in
          let location = try skill_json |> member "location" |> to_string with _ -> "" in
          {
            Skills.Skill.name;
            description;
            version;
            always;
            prompts;
            location;
          }
        ) knowledge.skills
      ) :: !parts;

    Mutex.unlock knowledge.mutex;

    (* Build final prompt *)
    if !parts = [] then
      "You are OClaw, a helpful AI assistant."
    else
      String.concat "\n\n---\n\n" (List.rev !parts)

  let last_updated knowledge =
    Mutex.lock knowledge.mutex;
    let result = knowledge.last_updated in
    Mutex.unlock knowledge.mutex;
    result
end
