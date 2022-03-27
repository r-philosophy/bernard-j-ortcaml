open! Core
open! Async
open! Import

module Trigger = struct
  type t =
    { commands : String.Caseless.Set.t
    ; kinds : Action.Target.Kind.Set.t
    }
  [@@deriving sexp, compare, fields]

  let validate t =
    let v check field = Validate.field check t field in
    let nonempty set =
      Validate.name "set length" (Int.validate_positive (Set.length set))
    in
    Validate.of_list
      (Fields.to_list
         ~commands:
           (v (fun commands ->
                Validate.of_list
                  [ nonempty commands
                  ; Validate.list_indexed
                      (fun command ->
                        Validate.name
                          "length"
                          (Int.validate_bound
                             (String.length command)
                             ~min:(Incl 1)
                             ~max:(Incl 100)))
                      (Set.to_list commands)
                  ]))
         ~kinds:(v nonempty))
  ;;
end

type t =
  { info : string
  ; trigger : Trigger.t
  ; actions : Action.t list
  }
[@@deriving sexp, fields]

let validate t =
  let v check field = Validate.field check t field in
  let pass _ = Validate.pass in
  Validate.of_list
    (Fields.to_list
       ~info:pass
       ~trigger:(v Trigger.validate)
       ~actions:(v (Validate.list_indexed Action.validate)))
;;

let find_matching_report { trigger = { commands; kinds }; _ } ~target =
  let target_kind = Action.Target.kind target in
  match Set.mem kinds target_kind with
  | false -> None
  | true ->
    Action.Target.moderator_reports target
    |> List.find ~f:(fun ({ report; _ } : Moderator_report.t) -> Set.mem commands report)
;;

let will_remove t = List.exists t.actions ~f:Action.will_remove
