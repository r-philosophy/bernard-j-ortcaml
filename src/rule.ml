open! Core
open! Async
open! Import

module Trigger = struct
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

let find_matching_report { trigger = { commands; kinds }; _ } ~target =
  let target_kind = Action.Target.kind target in
  match Set.mem kinds target_kind with
  | false -> None
  | true ->
    Action.Target.moderator_reports target
    |> List.find ~f:(fun ({ report; _ } : Moderator_report.t) -> Set.mem commands report)
;;

let will_remove t = List.exists t.actions ~f:Action.will_remove
