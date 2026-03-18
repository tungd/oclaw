(** HTTP Client using curl.multi and iomux for concurrent requests *)

(** Module for HTTP methods *)
module HttpMethod : sig
  (** HTTP method type *)
  type t = GET | POST | PUT | PATCH | DELETE
  
  (** Convert HTTP method to string *)
  val to_string : t -> string
  
  (** Convert string to HTTP method *)
  val of_string : string -> t option
end

(** Module for HTTP requests *)
module HttpRequest : sig
  (** HTTP request type *)
  type t = {
    method_ : HttpMethod.t;
    url : string;
    headers : (string * string) list;
    body : string option;
    timeout : int;
    on_write : (string -> unit) option;
  }
  
  (** Create a new HTTP request *)
  val create : 
    method_:HttpMethod.t -> 
    url:string -> 
    ?headers:(string * string) list -> 
    ?body:string -> 
    ?timeout:int -> 
    ?on_write:(string -> unit) ->
    unit -> t
  
  (** Convert request to curl handle *)
  val to_curl_handle : t -> Curl.t
end

(** Module for HTTP responses *)
module HttpResponse : sig
  (** HTTP response type *)
  type t = {
    status : int;
    headers : (string * string) list;
    body : string;
    error : string option;
  }
  
  (** Create a new HTTP response *)
  val create : 
    status:int -> 
    headers:(string * string) list -> 
    body:string -> 
    ?error:string -> 
    unit -> t
  
  (** Check if response indicates success (2xx status code) *)
  val is_success : t -> bool
  
  (** Get a specific header from response *)
  val get_header : t -> string -> string option
end

(** Exception for HTTP errors *)
exception Http_error of string

(** Perform a single HTTP request *)
val make_request : HttpRequest.t -> HttpResponse.t

(** Perform multiple HTTP requests concurrently using curl.multi *)
val perform_multi_requests : HttpRequest.t list -> HttpResponse.t list

(** Simple GET request *)
val get : string -> (string * string) list -> int -> HttpResponse.t

(** Simple POST request *)
val post : string -> (string * string) list -> string -> int -> HttpResponse.t

(** Simple PUT request *)
val put : string -> (string * string) list -> string -> int -> HttpResponse.t

(** Simple DELETE request *)
val delete : string -> (string * string) list -> int -> HttpResponse.t

(** Streaming POST request with callback *)
val post_streaming : string -> (string * string) list -> string -> int -> (string -> unit) -> (unit, string) result

(** Streaming GET request with callback *)
val get_streaming : string -> (string * string) list -> int -> (string -> unit) -> (unit, string) result
