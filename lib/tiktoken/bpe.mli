(** Byte Pair Encoding (BPE) algorithm implementation. *)

(** Encode a byte sequence using BPE.
    
    Takes a vocabulary mapping byte sequences to ranks, and an input byte
    sequence. Returns a list of token IDs. *)
val encode : Vocab.t -> bytes -> int list

(** Encode with byte fallback for unknown bytes. *)
val encode_with_fallback : Vocab.t -> bytes -> int list

(** Optimized BPE encoding module *)
module Optimized : sig
  (** Encode using optimized algorithm with O(n log n) complexity. *)
  val encode : Vocab.t -> bytes -> int list
end

(** Decode a list of token IDs back to bytes using the inverse vocabulary. *)
val decode : (int, string) Hashtbl.t -> int list -> string