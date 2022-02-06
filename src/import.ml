open! Core
open! Async
include Reddit_api_async

let retry_or_fail retry_manager here endpoint =
  match%bind Retry_manager.call retry_manager endpoint with
  | Ok v -> return v
  | Error error ->
    raise_s
      [%message
        "Reddit returned error"
          (here : Source_code_position.t)
          ~request:(endpoint.request : Endpoint.Request.t)
          (error : Retry_manager.Permanent_error.t)]
;;

(** A [Scope.t] represents an OAuth2 scope. *)
module Scope : sig
  include Identifiable.S

  val request_parameter : Set.t -> string
end = struct
  include String

  let request_parameter set =
    Set.to_list set |> List.map ~f:to_string |> String.concat ~sep:","
  ;;
end
