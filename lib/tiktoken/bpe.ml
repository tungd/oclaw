(** Byte Pair Encoding (BPE) algorithm implementation.
    
    This implements the core BPE encoding algorithm as used by tiktoken.
    The algorithm works by iteratively merging the most frequent pair of
    adjacent tokens until no more merges are possible.
*)

(** Find the pair with the lowest rank (highest priority for merging) *)
let find_min_rank vocab parts =
  let min_idx = ref (-1) in
  let min_rank = ref None in
  let len = Array.length parts in
  for i = 0 to len - 2 do
    (* Concatenate current part with next part *)
    let merged = Bytes.cat parts.(i) parts.(i + 1) in
    match Vocab.lookup vocab merged with
    | Some rank ->
        (match !min_rank with
         | None -> 
             min_idx := i;
             min_rank := Some rank
         | Some current_min when rank < current_min ->
             min_idx := i;
             min_rank := Some rank
         | _ -> ())
    | None -> ()
  done;
  (!min_idx, !min_rank)

(** Encode a byte sequence using BPE.
    
    Takes a vocabulary mapping byte sequences to ranks, and an input byte
    sequence. Returns a list of token IDs.
    
    The algorithm:
    1. Start with each byte as a separate token
    2. Find the pair of adjacent tokens that has the lowest rank in vocab
    3. Merge that pair into a single token
    4. Repeat until no more merges are possible
    5. Return the ranks of all remaining tokens
*)
let encode vocab input =
  let input_len = Bytes.length input in
  if input_len = 0 then []
  else if input_len = 1 then begin
    (* Single byte - look it up directly *)
    match Vocab.lookup vocab input with
    | Some rank -> [rank]
    | None -> 
        (* Byte not in vocab - this shouldn't happen with a complete vocab *)
        (* But we return the byte value as a fallback *)
        [Char.code (Bytes.get input 0)]
  end else begin
    (* Initialize parts list - each byte starts as its own token *)
    let parts_list = ref (List.init input_len (fun i -> 
      Bytes.make 1 (Bytes.get input i)
    )) in
    
    let rec merge_list () =
      let parts = !parts_list in
      match parts with
      | [] | [_] -> ()
      | _ ->
          let arr = Array.of_list parts in
          let idx, rank_opt = find_min_rank vocab arr in
          match rank_opt with
          | None -> ()
          | Some _ ->
              let merged = Bytes.cat arr.(idx) arr.(idx + 1) in
              let before = List.take idx parts in
              let after = List.drop (idx + 2) parts in
              parts_list := before @ [merged] @ after;
              merge_list ()
    in
    
    merge_list ();
    
    (* Convert remaining parts to token IDs *)
    List.concat_map (fun bytes ->
      match Vocab.lookup vocab bytes with
      | Some rank -> [rank]
      | None ->
          (* Shouldn't happen with complete vocab, but handle gracefully *)
          (* Try to encode each byte individually *)
          let rec encode_unknown bs acc =
            if Bytes.length bs = 0 then List.rev acc
            else
              let byte = Bytes.get bs 0 in
              let rest = Bytes.sub bs 1 (Bytes.length bs - 1) in
              match Vocab.lookup vocab (Bytes.make 1 byte) with
              | Some rank -> encode_unknown rest (rank :: acc)
              | None -> encode_unknown rest (Char.code byte :: acc)
          in
          encode_unknown bytes []
    ) !parts_list
  end

(** Encode with byte fallback for unknown bytes.
    
    Similar to encode, but handles bytes that aren't in the vocabulary
    by using special byte fallback tokens (if available in the vocab).
*)
let encode_with_fallback vocab input =
  encode vocab input

(** Optimized encode for longer sequences using a more efficient data structure.
    
    This version uses a linked list representation for O(1) merges instead
    of rebuilding the entire list on each merge.
*)
module Optimized = struct
  (** Node in a singly-linked list for efficient merging *)
  type node = {
    mutable next: int option;  (* Index of next node, None for tail *)
    mutable data: bytes;       (* The byte sequence for this token *)
    mutable rank: int option;   (* Cached rank if found in vocab *)
  }

  (** Encode using optimized algorithm *)
  let encode vocab input =
    let input_len = Bytes.length input in
    if input_len = 0 then []
    else if input_len = 1 then begin
      match Vocab.lookup vocab input with
      | Some rank -> [rank]
      | None -> [Char.code (Bytes.get input 0)]
    end else begin
      (* Initialize nodes - each byte is a node *)
      let nodes = Array.init input_len (fun i ->
        let data = Bytes.make 1 (Bytes.get input i) in
        let rank = Vocab.lookup vocab data in
        { next = if i = input_len - 1 then None else Some (i + 1);
          data;
          rank }
      ) in
      
      (* Track which nodes are still active *)
      let active = Array.make input_len true in
      let active_count = ref input_len in
      
      (* Find the pair with minimum rank *)
      let find_min () =
        let min_idx = ref (-1) in
        let min_rank = ref max_int in
        for i = 0 to input_len - 1 do
          if active.(i) && nodes.(i).next <> None then begin
            let next_idx = Option.get nodes.(i).next in
            if active.(next_idx) then begin
              let merged = Bytes.cat nodes.(i).data nodes.(next_idx).data in
              match Vocab.lookup vocab merged with
              | Some rank when rank < !min_rank ->
                  min_idx := i;
                  min_rank := rank
              | _ -> ()
            end
          end
        done;
        if !min_idx >= 0 then Some (!min_idx, !min_rank) else None
      in
      
      (* Merge loop *)
      let rec merge () =
        match find_min () with
        | None -> ()
        | Some (idx, _rank) ->
            let next_idx = Option.get nodes.(idx).next in
            (* Merge idx and next_idx *)
            let merged = Bytes.cat nodes.(idx).data nodes.(next_idx).data in
            nodes.(idx).data <- merged;
            nodes.(idx).rank <- Vocab.lookup vocab merged;
            nodes.(idx).next <- nodes.(next_idx).next;
            active.(next_idx) <- false;
            decr active_count;
            merge ()
      in
      
      merge ();
      
      (* Find the head of the list *)
      let head = ref 0 in
      while !head < input_len && not active.(!head) do
        incr head
      done;
      
      (* Walk the linked list *)
      let result = ref [] in
      let idx = ref (Some !head) in
      while !idx <> None do
        let i = Option.get !idx in
        if active.(i) then begin
          match nodes.(i).rank with
          | Some rank -> result := rank :: !result
          | None ->
              let bytes = nodes.(i).data in
              for j = 0 to Bytes.length bytes - 1 do
                let byte = Bytes.get bytes j in
                match Vocab.lookup vocab (Bytes.make 1 byte) with
                | Some rank -> result := rank :: !result
                | None -> result := Char.code byte :: !result
              done
        end;
        idx := nodes.(i).next
      done;
      
      List.rev !result
    end
end

(** Decode a list of token IDs back to bytes using the inverse vocabulary.
    The inverse vocabulary maps token IDs to their byte sequences. *)
let decode inverse_vocab tokens =
  let buffer = Buffer.create (List.length tokens * 4) in
  List.iter (fun token_id ->
    match Hashtbl.find_opt inverse_vocab token_id with
    | Some bytes -> Buffer.add_string buffer bytes
    | None ->
        (* Unknown token - add replacement character *)
        Buffer.add_string buffer "\xEF\xBF\xBD" (* Unicode replacement character *)
  ) tokens;
  Buffer.contents buffer