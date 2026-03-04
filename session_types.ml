(** Session and API types *)

open Protocol_conv_json

type chat_request = {
  session_id : string;
  content : string;
  model : string option;
  temperature : float option;
}
[@@deriving protocol ~driver:(module Json)]

type chat_response = {
  response : string;
  session_id : string;
  created_at : float;
}
[@@deriving protocol ~driver:(module Json)]

type session_info = {
  id : string;
  directory : string;
  created_at : float;
  last_active : float;
  message_count : int;
}
[@@deriving protocol ~driver:(module Json)]

type sessions_response = {
  sessions : session_info list;
}
[@@deriving protocol ~driver:(module Json)]

type health_response = {
  status : string;
  timestamp : float;
  version : string;
}
[@@deriving protocol ~driver:(module Json)]

type tool_response = {
  name : string;
  result : string;
  error : string option;
}
[@@deriving protocol ~driver:(module Json)]