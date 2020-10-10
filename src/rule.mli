open! Core
open! Async
open! Import

module Trigger : sig
  type t =
    { commands : String.Caseless.Set.t
    ; kinds : Action.Target.Kind.Set.t
    }
  [@@deriving sexp]
end

type t =
  { info : string
  ; trigger : Trigger.t
  ; actions : Action.t list
  }
[@@deriving sexp]

val find_matching_report : t -> target:Action.Target.t -> Moderator_report.t option
val will_remove : t -> bool
