(** Agent Client Protocol (ACP) for OClaw using jsonrpc library. *)

module Message : sig
  module Method : sig
    val initialize : string
    val initialized : string
    val session_update : string
    val request_permission : string
    val agent_plan : string
    val agent_message : string
    val agent_delta : string
    val status : string
  end

  type tool_kind =
    | Read
    | Edit
    | Delete
    | Move
    | Search
    | Execute
    | Think
    | Fetch
    | Other

  type tool_status =
    | Pending
    | In_progress
    | Completed
    | Failed

  type content_block =
    | Text of string

  type tool_call_content =
    | Content of content_block

  type tool_call = {
    tool_call_id : string;
    title : string option;
    kind : tool_kind option;
    status : tool_status option;
    content : tool_call_content list option;
    raw_input : Yojson.Safe.t option;
    raw_output : Yojson.Safe.t option;
  }

  type session_update =
    | Tool_call of tool_call
    | Tool_call_update of tool_call

  type permission_option_kind =
    | Allow_once
    | Allow_always
    | Reject_once
    | Reject_always

  type permission_option = {
    option_id : string;
    name : string;
    kind : permission_option_kind;
  }

  type permission_outcome =
    | Selected of string
    | Cancelled

  type t =
    | Initialize of { capabilities : Yojson.Safe.t }
    | Initialized
    | Session_update of { session_id : string; update : session_update }
    | Request_permission of {
        session_id : string;
        tool_call : tool_call;
        options : permission_option list;
      }
    | Agent_message of { content : string; chat_id : int option }
    | Agent_plan of { steps : string list }
    | Agent_delta of { content : string }
    | Status of { status : string; message : string option }
    | Error of { message : string; code : int }
    | Done

  val tool_kind_to_string : tool_kind -> string
  val tool_status_to_string : tool_status -> string
  val permission_option_kind_to_string : permission_option_kind -> string

  val to_jsonrpc : ?id:Jsonrpc.Id.t -> t -> Jsonrpc.Packet.t
  val of_jsonrpc_packet : Jsonrpc.Packet.t -> t option
  val permission_outcome_of_packet : Jsonrpc.Packet.t -> (Jsonrpc.Id.t * permission_outcome) option
end

module Channel : sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit
  val recv : 'a t -> 'a
  val try_recv : 'a t -> 'a option
  val recv_timeout : 'a t -> float -> 'a option
end

module Stdio_frontend : sig
  type config = {
    input : in_channel;
    output : out_channel;
  }
  type t
  val create : ?config:config -> unit -> t
  val send : t -> Jsonrpc.Packet.t -> unit
  val recv : t -> Jsonrpc.Packet.t option
  val recv_timeout : t -> float -> Jsonrpc.Packet.t option
  val close : t -> unit
end
