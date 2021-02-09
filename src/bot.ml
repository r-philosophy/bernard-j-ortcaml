open! Core
open! Async
open! Import

let get_full_listing here ~retry_manager ~get_listing =
  let get_one pagination =
    retry_or_fail retry_manager here (get_listing ?pagination ~limit:100 ())
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
    ; retry_manager : Retry_manager.t
    ; subreddit : Subreddit_name.t
    ; subreddit_id : Thing.Subreddit.Id.t
    ; database : Database.t
    }
  [@@deriving fields]

  let reports { subreddit; retry_manager; _ } =
    get_full_listing
      [%here]
      ~retry_manager
      ~get_listing:(fun ?pagination ~limit connection ->
        Api.modqueue ?pagination ~limit connection ~subreddit)
  ;;

  let handle_target
      { rules; action_buffers; retry_manager; subreddit; subreddit_id; database }
      ~target
    =
    match
      List.find_map rules ~f:(fun rule ->
          Rule.find_matching_report rule ~target
          |> Option.map ~f:(fun ({ moderator; _ } : Moderator_report.t) ->
                 rule, moderator))
    with
    | None -> return ()
    | Some (_rule, None) ->
      raise_s
        [%message "A moderator deleted their account. Weird!" (target : Action.Target.t)]
    | Some (rule, Some moderator) ->
      (match%bind Database.already_acted database ~target ~moderator with
      | true -> return ()
      | false ->
        Log.Global.info_s
          [%sexp
            { subreddit : Subreddit_name.t
            ; action_summary : string = rule.info
            ; author : Username.t option = Action.Target.author target
            ; moderator : Username.t
            ; target : Thing.Fullname.t = Action.Target.fullname target
            }];
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
        let%bind (`Ok | `Already_recorded) = Database.record_contents database ~target in
        let%bind () =
          match Rule.will_remove rule with
          | true -> return ()
          | false ->
            retry_or_fail
              retry_manager
              [%here]
              (Api.approve ~id:(Action.Target.fullname target) ())
        in
        Deferred.List.iter
          rule.actions
          ~f:
            (Action.act
               ~target
               ~retry_manager
               ~subreddit
               ~moderator
               ~time
               ~action_buffers))
  ;;

  let run_once ({ action_buffers; subreddit; retry_manager; _ } as t) =
    let%bind () =
      reports t
      >>| List.map ~f:target_of_thing
      >>= Deferred.List.iter ~f:(fun target -> handle_target t ~target)
    in
    Action.Action_buffers.commit_all action_buffers ~retry_manager ~subreddit
  ;;
end

type t =
  { subreddits : Per_subreddit.t list
  ; retry_manager : Retry_manager.t
  ; database : Database.t
  }

let create ~subreddit_configs ~connection ~database =
  let retry_manager = Retry_manager.create connection in
  let%bind subreddits =
    Map.to_alist subreddit_configs
    |> Deferred.List.map ~f:(fun (subreddit, rules) ->
           let%bind subreddit_id =
             retry_or_fail retry_manager [%here] (Api.about_subreddit ~subreddit ())
             >>| Thing.Subreddit.id
           in
           return
             ({ rules
              ; action_buffers = Action.Action_buffers.create ()
              ; retry_manager
              ; subreddit
              ; subreddit_id
              ; database
              }
               : Per_subreddit.t))
  in
  return { subreddits; retry_manager; database }
;;

let refresh_subreddit_tables { subreddits; retry_manager; database } =
  let subreddit_ids = List.map subreddits ~f:Per_subreddit.subreddit_id in
  let%bind subreddits =
    retry_or_fail
      retry_manager
      [%here]
      (Api.info (Id (List.map subreddit_ids ~f:(fun v -> `Subreddit v))) ())
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
          [%here]
          ~retry_manager
          ~get_listing:(fun ?pagination ~limit connection ->
            Api.moderators ?pagination ~limit connection ~subreddit:subreddit_name)
        >>| List.map ~f:Relationship.Moderator.username
      in
      Database.update_moderator_table database ~moderators ~subreddit:subreddit_id)
;;

let run_forever t =
  let stop =
    Deferred.create (fun ivar ->
        Signal.handle Signal.terminating ~f:(fun signal ->
            Log.Global.info_s [%message "Stopping on signal" (signal : Signal.t)];
            Ivar.fill ivar ()))
  in
  let%bind () = refresh_subreddit_tables t in
  let run_until_stop span f =
    Deferred.create (fun finished -> Clock_ns.every' ~stop ~finished span f)
  in
  Deferred.all_unit
    [ run_until_stop (Time_ns.Span.of_string "5m") (fun () -> refresh_subreddit_tables t)
    ; run_until_stop (Time_ns.Span.of_string "30s") (fun () ->
          Deferred.List.iter t.subreddits ~f:Per_subreddit.run_once)
    ]
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

let validate_per_subreddit_configs configs =
  Map.to_alist configs
  |> Validate.list
       ~name:(fun (subreddit, _rules) -> Subreddit_name.to_string subreddit)
       (fun (_subreddit, rules) -> Validate.list_indexed Rule.validate rules)
;;

let per_subreddit_param =
  let%map_open.Command config_path =
    flag
      "-subreddits"
      (required Filename.arg_type)
      ~doc:"FILENAME Path to per-subreddit configs"
  in
  let%bind files = Sys.ls_dir config_path in
  let rules_unvalidated =
    List.filter_map files ~f:(fun filename ->
        match String.chop_suffix filename ~suffix:".sexp" with
        | None -> None
        | Some subreddit_name ->
          let absolute_path = Filename.concat config_path filename in
          Some
            ( Subreddit_name.of_string subreddit_name
            , Sexp.load_sexps_conv_exn absolute_path [%of_sexp: Rule.t] ))
    |> Subreddit_name.Map.of_alist_exn
  in
  return (Validate.valid_or_error rules_unvalidated validate_per_subreddit_configs)
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
    let%bind.Deferred.Or_error subreddit_configs = subreddit_configs
    and database = Deferred.ok database in
    let%bind t = create ~connection ~subreddit_configs ~database in
    let%bind () = run_forever t in
    return (Ok ())
;;

let main_command = Command.async_or_error ~summary:"Run the bot" param

let validate_command =
  Command.async_or_error
    ~summary:"Validate configs"
    (let%map_open.Command subreddit_configs = per_subreddit_param in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind (_ : Rule.t list Subreddit_name.Map.t) = subreddit_configs in
       return ())
;;

let command =
  Command.group
    ~summary:"Bernard J. Ortcutt moderation bot"
    [ "run", main_command; "validate", validate_command ]
;;
