type t = {
  project_root : string;
  model_name : string;
  chat_label : string;
  git_branch : string option;
  token_usage : string;
}

val text : t -> string
val render : supports_ansi:bool -> t -> string
