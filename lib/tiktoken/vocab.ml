(** Vocabulary loading and management for tiktoken BPE tokenizer.
    
    Vocabulary files are in tiktoken format:
    - Each line: <base64_encoded_token> <rank>
    - The rank is the token ID
    - Tokens are UTF-8 bytes encoded in base64
*)

(** A vocabulary maps byte sequences to token IDs (ranks) *)
type t = (bytes, int) Hashtbl.t

(** Parse error during vocabulary loading *)
exception Parse_error of string

(** Base64 decoding table *)
let base64_decode_table =
  let table = Array.make 128 (-1) in
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  String.iteri (fun i c -> table.(Char.code c) <- i) chars;
  table

(** Decode a base64 string to bytes.
    Handles standard base64 encoding (with '=' padding). *)
let base64_decode encoded =
  let len = String.length encoded in
  if len = 0 then Bytes.empty
  else begin
    (* Remove any trailing whitespace/newlines *)
    let encoded = String.trim encoded in
    let len = String.length encoded in
    if len = 0 then Bytes.empty
    else begin
      (* Calculate output length *)
      let padding = 
        if len >= 1 && encoded.[len - 1] = '=' then 1 else 0
      in
      let padding = 
        if len >= 2 && encoded.[len - 2] = '=' then padding + 1 else padding
      in
      let out_len = (len * 3) / 4 - padding in
      let result = Bytes.create out_len in
      
      let get_val i =
        if i >= len then 0
        else
          let c = encoded.[i] in
          if c = '=' then 0  (* Padding character *)
          else begin
            let code = Char.code c in
            if code < 0 || code >= 128 then 
              raise (Parse_error (Printf.sprintf "Invalid base64 character at position %d" i))
            else
              let v = base64_decode_table.(code) in
              if v < 0 then
                raise (Parse_error (Printf.sprintf "Invalid base64 character '%c' at position %d" c i))
              else v
          end
      in
      
      let rec decode_chunk i out_i =
        if i >= len - padding then ()
        else begin
          let v0 = get_val i in
          let v1 = get_val (i + 1) in
          let v2 = get_val (i + 2) in
          let v3 = get_val (i + 3) in
          
          let b0 = (v0 lsl 2) lor (v1 lsr 4) in
          Bytes.set result out_i (Char.chr b0);
          
          if out_i + 1 < out_len then begin
            let b1 = ((v1 land 0x0f) lsl 4) lor (v2 lsr 2) in
            Bytes.set result (out_i + 1) (Char.chr b1);
            
            if out_i + 2 < out_len then begin
              let b2 = ((v2 land 0x03) lsl 6) lor v3 in
              Bytes.set result (out_i + 2) (Char.chr b2);
            end
          end;
          
          decode_chunk (i + 4) (out_i + 3)
        end
      in
      
      decode_chunk 0 0;
      result
    end
  end

(** Parse a single line of the vocabulary file.
    Format: <base64_token><space><rank> *)
let parse_line line =
  let line = String.trim line in
  if String.length line = 0 then None
  else begin
    (* Find the space separating token from rank *)
    let space_idx = 
      try String.index line ' '
      with Not_found -> 
        (* Some formats use tab *)
        try String.index line '\t'
        with Not_found -> -1
    in
    if space_idx < 0 then None
    else begin
      let base64_token = String.sub line 0 space_idx in
      let rank_str = String.trim (String.sub line (space_idx + 1) (String.length line - space_idx - 1)) in
      try
        let rank = int_of_string rank_str in
        let token_bytes = base64_decode base64_token in
        Some (Bytes.to_string token_bytes, rank)
      with 
      | Failure _ -> None
      | Parse_error _ -> None
    end
  end

(** Load vocabulary from a string (file contents) *)
let from_string contents =
  let vocab = Hashtbl.create 150000 in
  let lines = String.split_on_char '\n' contents in
  List.iter (fun line ->
    match parse_line line with
    | None -> () (* Skip empty/invalid lines *)
    | Some (token, rank) ->
        Hashtbl.replace vocab (Bytes.of_string token) rank
  ) lines;
  vocab

(** Load vocabulary from a chunked string source (for embedded vocabularies) *)
let from_chunks get_chunk =
  let vocab = Hashtbl.create 150000 in
  let rec load_chunk i =
    match get_chunk i with
    | None -> ()
    | Some chunk ->
        let lines = String.split_on_char '\n' chunk in
        List.iter (fun line ->
          match parse_line line with
          | None -> ()
          | Some (token, rank) ->
              Hashtbl.replace vocab (Bytes.of_string token) rank
        ) lines;
        load_chunk (i + 1)
  in
  load_chunk 0;
  vocab

(** Lookup a byte sequence in the vocabulary.
    Returns the rank (token ID) or None if not found. *)
let lookup vocab bytes =
  Hashtbl.find_opt vocab bytes

(** Get the number of tokens in the vocabulary *)
let size vocab = Hashtbl.length vocab

(** Create an inverted vocabulary for decoding (token ID -> bytes) *)
let invert vocab =
  let inv = Hashtbl.create (Hashtbl.length vocab) in
  Hashtbl.iter (fun bytes rank ->
    Hashtbl.replace inv rank (Bytes.to_string bytes)
  ) vocab;
  inv