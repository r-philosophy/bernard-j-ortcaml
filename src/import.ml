open! Core
open! Async
include Reddit_api_async
module Time_ns = Time_ns_unix

let retry_or_fail retry_manager here endpoint =
  Deferred.repeat_until_finished () (fun () ->
      match%bind Retry_manager.call retry_manager endpoint with
      | Ok v -> return (`Finished v)
      | Error
          (Endpoint_error
            (Json_response_errors
              [ { error = "RATELIMIT"
                ; fields = [ "ratelimit" ]
                ; details =
                    "Looks like you've been doing that a lot. Take a break for 5 seconds \
                     before trying again."
                ; error_type = _
                }
              ])) ->
        let%bind () = Clock_ns.after (Time_ns.Span.of_int_sec 6) in
        return (`Repeat ())
      | Error error ->
        raise_s
          [%message
            "Reddit returned error"
              (here : Source_code_position.t)
              ~request:(endpoint.request : Endpoint.Request.t)
              (error : Retry_manager.Permanent_error.t)])
;;

(** A [Scope.t] represents an OAuth2 scope. *)
module Scope : sig
  include Identifiable.S

  val request_parameter : Set.t -> string
end = struct
  let request_parameter set = Set.to_list set |> String.concat ~sep:","

  include String
end
