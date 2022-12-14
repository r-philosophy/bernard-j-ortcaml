open! Core
open! Async
include Reddit_api_async
module Time_ns = Time_ns_unix

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
