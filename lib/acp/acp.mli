(** Agent Client Protocol (ACP) for OClaw using jsonrpc library.

    This implementation follows the Agent Client Protocol (ACP) specification,
    which is built on JSON-RPC 2.0 and re-uses Model Context Protocol (MCP)
    representations where possible.

    {1:jsonrpc JSON-RPC 2.0}

    All messages follow the JSON-RPC 2.0 specification using the jsonrpc library.
*)

open Yojson.Safe

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

  val to_jsonrpc : ?id:Jsonrpc.Id.t -> t -> Jsonrpc.Packet.t
  val of_jsonrpc_packet : Jsonrpc.Packet.t -> t option
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
  val send : t -> Jsonrpc.Packet.t -> unit
  val recv : t -> Jsonrpc.Packet.t option
  val recv_timeout : t -> float -> Jsonrpc.Packet.t option
  val close : t -> unit
end
