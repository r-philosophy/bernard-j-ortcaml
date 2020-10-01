open! Core
open! Async
open Reddit_api

let database = Database.create ()

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

let find_triggering_report { trigger = { commands; kinds }; _ } ~target =
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

let apply_to_target ts ~target ~connection ~subreddit ~action_buffers =
  match Database.already_acted target with
  | true -> return ()
  | false ->
    (match
       List.find_map ts ~f:(fun t ->
           find_triggering_report t ~target
           |> Option.map ~f:(fun ({ moderator; _ } : Mod_report.t) -> t, moderator))
     with
    | None -> return ()
    | Some (t, moderator) ->
      Database.log_rule_application
        database
        ~target
        ~rule_description:t.info
        ~moderator
        ~time:(Time_ns.now ());
      Deferred.List.iter
        t.actions
        ~f:(Action.act ~target ~connection ~subreddit ~action_buffers))
;;
