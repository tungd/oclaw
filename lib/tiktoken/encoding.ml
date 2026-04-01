(** Encoding definitions for different tiktoken models.
    
    Each encoding specifies:
    - A name (e.g., "cl100k_base", "o200k_base")
    - A regex pattern for pre-tokenization (splitting text into chunks)
    - A vocabulary mapping byte sequences to token IDs
    - Special tokens (like <|endoftext|>, <|endofprompt|>)
*)

(** Special tokens for different encodings *)
type special_tokens = {
  endoftext : int option;
  fim_prefix : int option;   (* For fill-in-the-middle completions *)
  fim_middle : int option;
  fim_suffix : int option;
  endofprompt : int option;
}

(** An encoding specification *)
type t = {
  name : string;
  vocab : (bytes, int) Hashtbl.t;
  inverse_vocab : (int, string) Hashtbl.t;
  special_tokens : special_tokens;
}

(** Build inverse vocabulary for decoding *)
let build_inverse_vocab vocab =
  let inv = Hashtbl.create (Hashtbl.length vocab) in
  Hashtbl.iter (fun bytes rank ->
    Hashtbl.replace inv rank (Bytes.to_string bytes)
  ) vocab;
  inv

(** Check if a Unicode code point is a letter *)
let is_letter cp =
  (* Basic Unicode letter ranges - covers most common cases *)
  (cp >= 0x0041 && cp <= 0x005A) ||  (* A-Z *)
  (cp >= 0x0061 && cp <= 0x007A) ||  (* a-z *)
  (cp >= 0x00C0 && cp <= 0x00D6) ||  (* Latin Extended-A part 1 *)
  (cp >= 0x00D8 && cp <= 0x00F6) ||  (* Latin Extended-A part 2 *)
  (cp >= 0x00F8 && cp <= 0x00FF) ||  (* Latin Extended-A part 3 *)
  (cp >= 0x0100 && cp <= 0x017F) ||  (* Latin Extended-A *)
  (cp >= 0x0180 && cp <= 0x024F) ||  (* Latin Extended-B *)
  (cp >= 0x0400 && cp <= 0x04FF) ||  (* Cyrillic *)
  (cp >= 0x0370 && cp <= 0x03FF) ||  (* Greek *)
  (cp >= 0x4E00 && cp <= 0x9FFF) ||  (* CJK Unified Ideographs *)
  (cp >= 0x3400 && cp <= 0x4DBF) ||  (* CJK Extension A *)
  (cp >= 0xAC00 && cp <= 0xD7AF) ||  (* Hangul Syllables *)
  (cp >= 0x3040 && cp <= 0x309F) ||  (* Hiragana *)
  (cp >= 0x30A0 && cp <= 0x30FF) ||  (* Katakana *)
  (cp >= 0x0600 && cp <= 0x06FF) ||  (* Arabic *)
  (cp >= 0x0590 && cp <= 0x05FF) ||  (* Hebrew *)
  false  (* Default to false for unknown ranges *)

(** Check if a Unicode code point is a number/digit *)
let is_number cp =
  (cp >= 0x0030 && cp <= 0x0039) ||  (* 0-9 *)
  (cp >= 0xFF10 && cp <= 0xFF19)     (* Fullwidth digits *)

(** Check if a Unicode code point is whitespace *)
let is_whitespace cp =
  cp = 0x0020 ||  (* Space *)
  cp = 0x0009 ||  (* Tab *)
  cp = 0x000A ||  (* Newline *)
  cp = 0x000D ||  (* Carriage return *)
  cp = 0x00A0 ||  (* Non-breaking space *)
  cp = 0x1680 ||  (* Ogham space mark *)
  (cp >= 0x2000 && cp <= 0x200A) ||  (* Various spaces *)
  cp = 0x2028 ||  (* Line separator *)
  cp = 0x2029 ||  (* Paragraph separator *)
  cp = 0x202F ||  (* Narrow no-break space *)
  cp = 0x205F ||  (* Medium mathematical space *)
  cp = 0x3000     (* Ideographic space *)

(** Split text into chunks for BPE encoding.
    This is a simplified pre-tokenization that handles Unicode properly. *)
let split_into_chunks text =
  (* We'll iterate through the string character by character (handling UTF-8)
     and group them into chunks based on character classes.
     
     The original tiktoken pattern does:
     1. Match contractions
     2. Match words (letters)
     3. Match numbers (1-3 digits at a time)
     4. Match punctuation/other
     5. Match whitespace
     
     We'll approximate this with a simpler approach. *)
  let chunks = ref [] in
  let current_chunk = Buffer.create 128 in
  let current_type = ref `None in
  
  let flush () =
    if Buffer.length current_chunk > 0 then begin
      chunks := Buffer.contents current_chunk :: !chunks;
      Buffer.clear current_chunk
    end
  in
  
  (* Use Uutf to decode UTF-8 *)
  let decoder = Uutf.decoder (`String text) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await -> ()
    | `End -> flush ()
    | `Malformed _ ->
        (* On malformed UTF-8, treat as other and continue *)
        flush ();
        Buffer.add_string current_chunk "\xEF\xBF\xBD";  (* replacement char *)
        current_type := `Other;
        loop ()
    | `Uchar uchar ->
        let cp = Uchar.to_int uchar in
        let char_type =
          if is_letter cp then `Letter
          else if is_number cp then `Number
          else if is_whitespace cp then `Whitespace
          else if cp = Char.code '\'' then `Apostrophe
          else `Other
        in
        
        (* Decide whether to flush current chunk *)
        let should_flush =
          match (!current_type, char_type) with
          | (`None, _) -> false
          | (`Letter, `Letter) -> false
          | (`Number, `Number) -> false  (* Numbers can continue *)
          | (`Whitespace, `Whitespace) -> false
          | (`Other, `Other) -> false
          | (`Apostrophe, `Letter) -> false  (* Contractions *)
          | (`Letter, `Apostrophe) -> false
          | _ -> true
        in
        
        if should_flush then flush ();
        
        (* Add character to current chunk *)
        Uutf.Buffer.add_utf_8 current_chunk uchar;
        
        (* Update current type *)
        current_type := char_type;
        loop ()
  in
  loop ();
  
  List.rev !chunks

(** Parse vocabulary from a string content *)
let parse_vocab_content content =
  let vocab = Hashtbl.create 150000 in
  let lines = String.split_on_char '\n' content in
  List.iter (fun line ->
    let line = String.trim line in
    if String.length line > 0 then begin
      try
        let space_idx = String.index line ' ' in
        let base64_token = String.sub line 0 space_idx in
        let rank_str = String.trim (String.sub line (space_idx + 1) (String.length line - space_idx - 1)) in
        let rank = int_of_string rank_str in
        let token_bytes = Vocab.base64_decode base64_token in
        Hashtbl.replace vocab (Bytes.copy token_bytes) rank
      with _ -> ()
    end
  ) lines;
  vocab

(** Get embedded vocabulary data *)
let get_vocab_data name =
  match name with
  | "vocab_cl100k.txt" -> Vocab_cl100k_data.read name
  | "vocab_o200k.txt" -> Vocab_o200k_data.read name
  | _ -> failwith (Printf.sprintf "Unknown vocabulary: %s" name)

(** Load the cl100k_base encoding *)
let cl100k_base () =
  (* Load vocabulary from embedded data *)
  let vocab_data = get_vocab_data "vocab_cl100k.txt" in
  let content = match vocab_data with
    | Some c -> c
    | None -> failwith "Failed to load embedded vocab_cl100k.txt"
  in
  let vocab = parse_vocab_content content in
  let inverse_vocab = build_inverse_vocab vocab in
  {
    name = "cl100k_base";
    vocab;
    inverse_vocab;
    special_tokens = {
      endoftext = Some 100257;
      fim_prefix = Some 100258;
      fim_middle = Some 100259;
      fim_suffix = Some 100260;
      endofprompt = Some 100276;
    }
  }

(** Load the o200k_base encoding (used by GPT-4o, GPT-4o-mini) *)
let o200k_base () =
  let vocab_data = get_vocab_data "vocab_o200k.txt" in
  let content = match vocab_data with
    | Some c -> c
    | None -> failwith "Failed to load embedded vocab_o200k.txt"
  in
  let vocab = parse_vocab_content content in
  let inverse_vocab = build_inverse_vocab vocab in
  {
    name = "o200k_base";
    vocab;
    inverse_vocab;
    special_tokens = {
      endoftext = Some 199999;
      endofprompt = Some 200018;
      fim_prefix = None;
      fim_middle = None;
      fim_suffix = None;
    }
  }

(** Get encoding by name *)
let get_encoding = function
  | "cl100k_base" -> cl100k_base ()
  | "o200k_base" -> o200k_base ()
  | name -> failwith (Printf.sprintf "Unknown encoding: %s" name)

(** Get encoding for a model name *)
let encoding_for_model model =
  if String.length model >= 4 && String.sub model 0 4 = "qwen" then
    cl100k_base ()
  else match model with
  | "gpt-4o" | "gpt-4o-mini" -> o200k_base ()
  | "gpt-4" | "gpt-4-turbo" | "gpt-4-turbo-preview" | "gpt-3.5-turbo" | "gpt-3.5-turbo-16k"
  | "text-embedding-ada-002" | "text-embedding-3-small" | "text-embedding-3-large"
  | "qwen" | "qwen2" | "qwen-turbo" | "qwen-plus" | "qwen-max" ->
      cl100k_base ()
  (* Default to cl100k_base for unknown models *)
  | _ -> cl100k_base ()