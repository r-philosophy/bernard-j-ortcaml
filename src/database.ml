open! Core
open! Async
open! Import

type t = Pgx_async.t

let target_fullname_params target =
  let kind_int, id_int =
    match Action.Target.fullname target with
    | `Link id -> 3, Thing.Link.Id.to_int id
    | `Comment id -> 1, Thing.Comment.Id.to_int id
  in
  List.map [ kind_int; id_int ] ~f:Pgx_value.of_int
;;

let username_param username = Pgx_value.of_string (Username.to_string username)

let get_or_create_user_id t ~username =
  let params = [ username_param username ] in
  let%bind () =
    Pgx_async.execute_unit ~params t "INSERT OR IGNORE INTO users (username) VALUES($1)"
  in
  let%bind rows =
    Pgx_async.execute ~params t "SELECT id FROM users WHERE username = $1"
  in
  match List.map rows ~f:(List.map ~f:Pgx_value.to_int) with
  | [ [ Some id ] ] -> return (Pgx_value.of_int id)
  | _ ->
    raise_s
      [%message
        "Unexpected database response" (rows : Pgx.row list) (username : Username.t)]
;;

let already_acted t ~target ~moderator =
  let%bind rows =
    Pgx_async.execute
      ~params:(target_fullname_params target @ [ username_param moderator ])
      t
      "SELECT COUNT(1) FROM actions INNER JOIN users ON actions.moderator = users.id \
       WHERE target = ($1, $2)::target_id AND users.username = $3"
  in
  match List.map rows ~f:(List.map ~f:Pgx_value.to_int) with
  | [ [ Some 0 ] ] -> return false
  | [ [ Some 1 ] ] -> return true
  | _ ->
    raise_s
      [%message
        "Unexpected database response" (rows : Pgx.row list) (target : Action.Target.t)]
;;

let log_rule_application t ~target ~action_summary ~author ~moderator ~subreddit ~time =
  let%bind author_id =
    match author with
    | None -> return Pgx_value.null
    | Some username -> get_or_create_user_id t ~username
  in
  let%bind moderator_id = get_or_create_user_id t ~username:moderator in
  let subreddit_id = Thing.Subreddit.Id.to_int subreddit |> Pgx_value.of_int in
  let time = Time_ns.to_string time |> Pgx_value.of_string in
  let params =
    List.concat
      [ target_fullname_params target
      ; [ Pgx_value.of_string action_summary ]
      ; [ author_id ]
      ; [ moderator_id ]
      ; [ time ]
      ; [ subreddit_id ]
      ]
  in
  Pgx_async.execute_unit
    ~params
    t
    "INSERT INTO actions (target, action_summary, author, moderator, time, subreddit) \
     VALUES($1,$2,$3,$4,$5,$6,$7)"
;;

let update_subscriber_counts t ~subreddits =
  Deferred.List.iter subreddits ~f:(fun subreddit ->
      let subreddit_id =
        Thing.Subreddit.id subreddit |> Thing.Subreddit.Id.to_int |> Pgx_value.of_int
      in
      let display_name =
        Thing.Subreddit.name subreddit |> Subreddit_name.to_string |> Pgx_value.of_string
      in
      let%bind () =
        Pgx_async.execute_unit
          ~params:[ subreddit_id; display_name ]
          t
          "INSERT OR IGNORE INTO subreddits (id, display_name) VALUES($1,$2)"
      in
      let subscribers = Thing.Subreddit.subscribers subreddit |> Pgx_value.of_int in
      Pgx_async.execute_unit
        ~params:[ subscribers; subreddit_id ]
        t
        "UPDATE subreddits SET subscribers = $1 WHERE id = $2")
;;

let update_moderator_table t ~moderators ~subreddit =
  Pgx_async.with_transaction t (fun t ->
      let subreddit_id = Thing.Subreddit.Id.to_int subreddit |> Pgx_value.of_int in
      let%bind () =
        Pgx_async.execute_unit
          ~params:[ subreddit_id ]
          t
          "DELETE FROM subreddit_moderator WHERE subreddit_id = $1"
      in
      Deferred.List.iter moderators ~f:(fun moderator ->
          let%bind user_id = get_or_create_user_id t ~username:moderator in
          Pgx_async.execute_unit
            ~params:[ subreddit_id; user_id ]
            t
            "INSERT OR IGNORE INTO subreddit_moderator (subreddit_id, moderator_id) \
             VALUES($1,$2)"))
;;
