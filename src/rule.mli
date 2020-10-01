open! Core
open! Async
open Reddit_api

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

val apply_to_target
  :  t list
  -> target:Action.Target.t
  -> connection:Connection.t
  -> subreddit:Subreddit_name.t
  -> action_buffers:Action.Action_buffers.t
  -> unit Deferred.t
