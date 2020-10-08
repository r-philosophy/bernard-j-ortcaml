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
        let time = Time_ns.now () in
        let%bind () =
          Database.log_rule_application
            database
            ~target
            ~action_summary:rule.info
            ~author:(Action.Target.author target)
            ~subreddit:subreddit_id
            ~moderator
            ~time
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
          ~f:
            (Action.act
               ~target
               ~connection
               ~retry_manager
               ~subreddit
               ~moderator
               ~time
               ~action_buffers))
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

let connection_param =
  let%map_open.Command auth_config_path =
    flag "-credentials" (required string) ~doc:"FILENAME Reddit API credentials"
  in
  let credentials =
    Sexp.load_sexp_conv_exn auth_config_path [%of_sexp: Connection.Credentials.t]
  in
  Connection.create
    credentials
    ~user_agent:"BernardJOrtcutt v2.0 - by /u/L72_Elite_Kraken"
;;

let per_subreddit_param =
  let%map_open.Command config_path =
    flag
      "-subreddits"
      (required Filename.arg_type)
      ~doc:"FILENAME Path to per-subreddit configs"
  in
  let%bind files = Sys.ls_dir config_path in
  List.filter_map files ~f:(fun filename ->
      match String.chop_suffix filename ~suffix:".sexp" with
      | None -> None
      | Some subreddit_name ->
        let absolute_path = Filename.concat config_path filename in
        Some
          ( Subreddit_name.of_string subreddit_name
          , Sexp.load_sexps_conv_exn absolute_path [%of_sexp: Rule.t] ))
  |> Subreddit_name.Map.of_alist_exn
  |> return
;;

let database_param =
  let%map_open.Command database =
    flag "-database" (required string) ~doc:"STRING postgres database"
  in
  Pgx_async.connect ~database ()
;;

let param =
  let%map_open.Command connection = connection_param
  and database = database_param
  and subreddit_configs = per_subreddit_param in
  fun () ->
    let%bind database = database
    and subreddit_configs = subreddit_configs in
    let%bind t = create ~connection ~subreddit_configs ~database in
    run_forever t
;;

let command = Command.async param ~summary:"Bernard J. Ortcutt moderation bot"
