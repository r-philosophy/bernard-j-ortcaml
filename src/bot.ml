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
            ~subreddit_id
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

let run_all t =
  Clock_ns.every' (Time_ns.Span.of_string "30s") (fun () ->
      Deferred.List.iter t.subreddits ~f:Per_subreddit.run_once)
;;
