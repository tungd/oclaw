(** Agent Client Protocol (ACP) for OClaw.

    This implementation follows the Agent Client Protocol (ACP) specification,
    which is built on JSON-RPC 2.0 and re-uses Model Context Protocol (MCP)
    representations where possible.

    {1:jsonrpc JSON-RPC 2.0}

    All messages follow the JSON-RPC 2.0 specification.
*)

open Yojson.Safe

module Json_rpc : sig
  type id = [ `Int of int | `String of string | `Null ]

  type request = {
    id : id;
    method_name : string;
    params : Yojson.Safe.t;
  }

  type response = {
    id : id;
    result : (Yojson.Safe.t, error) result;
  }

  and error = {
    code : int;
    message : string;
    data : Yojson.Safe.t option;
  }

  type notification = {
    method_name : string;
    params : Yojson.Safe.t;
  }

  type message =
    | Request of request
    | Response of response
    | Notification of notification

  val to_yojson : message -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> message option
end

(** {1:acp_messages ACP Message Types} *)

(** ACP Specific Methods and Types *)
module Message : sig
  (** Methods for ACP/MCP *)
  module Method : sig
    val initialize : string
    val initialized : string
    val tools_list : string
    val tools_call : string
    val agent_plan : string
    val agent_message : string
    val agent_delta : string
    val status : string
  end

  type t =
    | Initialize of { capabilities : Yojson.Safe.t }
    | Initialized
    | Agent_message of { content : string; chat_id : int option }
    | Agent_plan of { steps : string list }
    | Agent_delta of { content : string }
    | Tool_call of { name : string; arguments : Yojson.Safe.t }
    | Tool_result of { name : string; content : string; is_error : bool }
    | Status of { status : string; message : string option }
    | Error of { message : string; code : int }
    | Done

  val to_json_rpc : id:Json_rpc.id -> t -> Json_rpc.message
  val of_json_rpc : Json_rpc.message -> t option
end

(** {1:channels Thread-safe Channels} *)

module Channel : sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit
  val recv : 'a t -> 'a
  val try_recv : 'a t -> 'a option
  val recv_timeout : 'a t -> float -> 'a option
end

(** {1:stdio_frontend Stdio Frontend} *)

module Stdio_frontend : sig
  type config = {
    input : in_channel;
    output : out_channel;
  }
  type t
  val create : ?config:config -> unit -> t
  val send : t -> Json_rpc.message -> unit
  val recv : t -> Json_rpc.message option
  val recv_timeout : t -> float -> Json_rpc.message option
  val close : t -> unit
end
