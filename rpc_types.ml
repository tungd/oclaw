(** RPC types for client-server communication *)

open Protocol_conv_json

type rpc_request = {
  jsonrpc : string option;
  method_ : string;
  id : string option;
  params : Json.t option;
}
[@@deriving protocol ~driver:(module Json)]

type rpc_response = {
  jsonrpc : string option;
  result : Json.t option;
  id : string option;
  error : rpc_error option;
}
[@@deriving protocol ~driver:(module Json)]

and rpc_error = {
  code : int;
  message : string;
  data : Json.t option;
}
[@@deriving protocol ~driver:(module Json)]

type notification = {
  method_ : string;
  params : Json.t option;
}
[@@deriving protocol ~driver:(module Json)]
