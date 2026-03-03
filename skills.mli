(** Skills module for OClaw - loading and prompt injection of SKILL.md files. *)

module Skill : sig
  type t = {
    name : string;
    description : string;
    version : string;
    always : bool;
    prompts : string list;
    location : string;
  }
end

(** Parse YAML-like front matter from a string.
    Returns a list of key-value pairs and the remaining body. *)
val parse_front_matter : string -> (string * string) list * string

(** Load a skill from a SKILL.md file path. *)
val load_skill_md : string -> Skill.t option

(** Load all skills from a directory.
    Scans {skills_dir}/* /SKILL.md. *)
val load_skills : ?skills_dir:string -> unit -> Skill.t list

(** Convert a list of skills into an XML-style string for the system prompt. *)
val skills_to_prompt : Skill.t list -> string
