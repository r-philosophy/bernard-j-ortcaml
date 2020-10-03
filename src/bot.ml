open! Core
open! Async
open! Import

let get_full_listing connection here ~retry_manager ~get_listing =
  let get_one pagination =
    retry_or_fail retry_manager here ~f:(fun () ->
        get_listing ?pagination ~limit:100 connection)
  in
  Deferred.repeat_until_finished (None, []) (fun (after, listings) ->
      let%bind listing = get_one after in
      let listings = listing :: listings in
      match Listing.after listing with
      | None ->
        return (`Finished (List.rev listings |> List.concat_map ~f:Listing.children))
      | Some after -> return (`Repeat (Some (Listing.Pagination.After after), listings)))
;;

let target_of_thing thing : Action.Target.t =
  match thing with
  | `Link link -> Link link
  | `Comment comment -> Comment comment
;;

module Per_subreddit = struct
  type t =
    { rules : Rule.t list
    ; action_buffers : Action.Action_buffers.t
    ; connection : Connection.t
    ; retry_manager : Retry_manager.t
    ; subreddit : Subreddit_name.t
    ; subreddit_id : Thing.Subreddit.Id.t
    ; database : Database.t
    }
  [@@deriving fields]

  let reports { subreddit; retry_manager; connection; _ } =
    get_full_listing
      connection
      [%here]
      ~retry_manager
      ~get_listing:(fun ?pagination ~limit connection ->
        Api.modqueue ?pagination ~limit connection ~subreddit)
  ;;

  let handle_target
      { rules
      ; action_buffers
      ; connection
      ; retry_manager
      ; subreddit
      ; subreddit_id
      ; database
      }
      ~target
    =
    match
      List.find_map rules ~f:(fun rule ->
          Rule.find_matching_report rule ~target
          |> Option.map ~f:(fun ({ moderator; _ } : Rule.Mod_report.t) -> rule, moderator))
    with
    | None -> return ()
    | Some (rule, moderator) ->
      (match%bind Database.already_acted database ~target ~moderator with
      | true -> return ()
      | false ->
        let%bind () =
          Database.log_rule_application
            database
            ~target
            ~action_summary:rule.info
            ~author:(Action.Target.author target)
            ~subreddit:subreddit_id
            ~moderator
            ~time:(Time_ns.now ())
        in
        let%bind () =
          match Rule.will_remove rule with
          | true -> return ()
          | false ->
            retry_or_fail retry_manager [%here] ~f:(fun () ->
                Api.approve ~id:(Action.Target.fullname target) connection)
        in
        Deferred.List.iter
          rule.actions
          ~f:(Action.act ~target ~connection ~retry_manager ~subreddit ~action_buffers))
  ;;

  let run_once ({ action_buffers; connection; subreddit; retry_manager; _ } as t) =
    let%bind () =
      reports t
      >>| List.map ~f:target_of_thing
      >>= Deferred.List.iter ~f:(fun target -> handle_target t ~target)
    in
    Action.Action_buffers.commit_all action_buffers ~connection ~retry_manager ~subreddit
  ;;
end

type t =
  { subreddits : Per_subreddit.t list
  ; connection : Connection.t
  ; retry_manager : Retry_manager.t
  ; database : Database.t
  }

let create ~subreddit_configs ~connection ~database =
  let retry_manager = Retry_manager.create connection in
  let%bind database = Database.create ~database in
  let%bind subreddits =
    Map.to_alist subreddit_configs
    |> Deferred.List.map ~f:(fun (subreddit, rules) ->
           let%bind subreddit_id =
             retry_or_fail retry_manager [%here] ~f:(fun () ->
                 Api.about_subreddit ~subreddit connection)
             >>| Thing.Subreddit.id
           in
           return
             ({ rules
              ; action_buffers = Action.Action_buffers.create ()
              ; connection
              ; retry_manager
              ; subreddit
              ; subreddit_id
              ; database
              }
               : Per_subreddit.t))
  in
  return { subreddits; connection; retry_manager; database }
;;

let refresh_subreddit_tables { subreddits; connection; retry_manager; database } =
  let subreddit_ids = List.map subreddits ~f:Per_subreddit.subreddit_id in
  let%bind subreddits =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.info (Id (List.map subreddit_ids ~f:(fun v -> `Subreddit v))) connection)
    >>| List.map ~f:(function
            | `Subreddit v -> v
            | (`Link _ | `Comment _) as thing ->
              raise_s
                [%message "Unexpected thing in info response" (thing : Thing.Poly.t)])
  in
  let%bind () = Database.update_subscriber_counts database ~subreddits in
  Deferred.List.iter subreddits ~f:(fun subreddit ->
      let subreddit_name = Thing.Subreddit.name subreddit in
      let subreddit_id = Thing.Subreddit.id subreddit in
      let%bind moderators =
        get_full_listing
          connection
          [%here]
          ~retry_manager
          ~get_listing:(fun ?pagination ~limit connection ->
            Api.moderators ?pagination ~limit connection ~subreddit:subreddit_name)
        >>| List.map ~f:Relationship.Moderator.username
      in
      Database.update_moderator_table database ~moderators ~subreddit:subreddit_id)
;;

let run_forever t =
  let%bind () = refresh_subreddit_tables t in
  Clock_ns.every' (Time_ns.Span.of_string "5m") (fun () -> refresh_subreddit_tables t);
  Clock_ns.every' (Time_ns.Span.of_string "30s") (fun () ->
      Deferred.List.iter t.subreddits ~f:Per_subreddit.run_once);
  never ()
;;
