(** LLM API retry with exponential backoff.
    
    Automatically retries HTTP requests to LLM APIs on transient failures:
    - Rate limits (429)
    - Server errors (5xx)
    - Network timeouts
    - Connection errors
    
    Does NOT retry on:
    - Invalid API key (401)
    - Bad requests (400)
    - Context too long errors
*)

(** Retry configuration *)
type retry_config = {
  (** Maximum number of retry attempts *)
  max_retries : int;
  (** Base delay in milliseconds for exponential backoff *)
  base_delay_ms : int;
  (** Maximum delay cap in milliseconds *)
  max_delay_ms : int;
  (** Exponential base (2.0 = double delay each attempt) *)
  exponential_base : float;
}

(** Result of a retry operation *)
type 'a retry_result =
  | Success of 'a  (** Request succeeded *)
  | Failed of string * int  (** Failed with error message and attempt count *)

(** Default retry configuration: 3 retries, 1s base delay, 30s max *)
val default_config : retry_config

(** Check if an error is retryable based on HTTP status and error body *)
val is_retryable_error : 
  http_status:int option -> 
  error_body:string -> 
  bool

(** Calculate delay for a given attempt number *)
val calculate_delay : 
  config:retry_config -> 
  attempt:int -> 
  int

(** Execute a function with retry logic.
    The function should return a result type ('a, string) result.
    On Error, checks if the error is retryable.
*)
val with_retry : 
  config:retry_config -> 
  (unit -> ('a, string) result) -> 
  'a retry_result
