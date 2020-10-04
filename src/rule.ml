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

module Mod_report = struct
  type t =
    { moderator : Username.t
    ; report : string
    }

  let of_json json =
    let report, moderator =
      Json.get_pair Json.get_string (Fn.compose Username.of_string Json.get_string) json
    in
    { report; moderator }
  ;;
end

let find_matching_report { trigger = { commands; kinds }; _ } ~target =
  let target_kind = Action.Target.kind target in
  match Set.mem kinds target_kind with
  | false -> None
  | true ->
    let mod_reports =
      let json =
        match target with
        | Link link -> Thing.Link.get_field_exn link "mod_reports"
        | Comment link -> Thing.Comment.get_field_exn link "mod_reports"
      in
      Json.get_list Mod_report.of_json json
    in
    List.find mod_reports ~f:(fun ({ report; _ } : Mod_report.t) ->
        Set.mem commands report)
;;

let will_remove t = List.exists t.actions ~f:Action.will_remove
