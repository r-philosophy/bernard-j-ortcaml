open! Core
open! Async
open! Import

type t = Pgx_async.t

let target_fullname_params target =
  let kind_int, id_int =
    match Action.Target.fullname target with
    | `Link id -> 3, Thing.Link.Id.to_int63 id
    | `Comment id -> 1, Thing.Comment.Id.to_int63 id
  in
  List.map [ kind_int; Int63.to_int_exn id_int ] ~f:Pgx.Value.of_int
;;

let username_param username = Pgx.Value.of_string (Username.to_string username)

let get_or_create_user_id t ~username =
  let params = [ username_param username ] in
  let%bind () =
    Pgx_async.execute_unit
      ~params
      t
      "INSERT INTO users (username) VALUES($1) ON CONFLICT DO NOTHING"
  in
  let%bind rows =
    Pgx_async.execute ~params t "SELECT id FROM users WHERE username = $1"
  in
  match List.map rows ~f:(List.map ~f:Pgx.Value.to_int) with
  | [ [ Some id ] ] -> return (Pgx.Value.of_int id)
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
       WHERE target = ($1, $2)::thing_id AND users.username = $3"
  in
  match List.map rows ~f:(List.map ~f:Pgx.Value.to_int) with
  | [ [ Some 0 ] ] -> return false
  | [ [ Some 1 ] ] -> return true
  | _ ->
    raise_s
      [%message
        "Unexpected database response" (rows : Pgx.row list) (target : Action.Target.t)]
;;

let record_contents t ~target =
  let target_fullname_params = target_fullname_params target in
  let json =
    let json =
      match target with
      | Comment comment -> Thing.Comment.to_json comment
      | Link link -> Thing.Link.to_json link
    in
    Json.value_to_string json |> Pgx.Value.of_string
  in
  let%bind author_id =
    match Action.Target.author target with
    | None -> return Pgx.Value.null
    | Some username -> get_or_create_user_id t ~username
  in
  let%bind subreddit_id =
    let subreddit =
      match target with
      | Comment comment -> Thing.Comment.subreddit comment
      | Link link -> Thing.Link.subreddit link
    in
    let%bind rows =
      Pgx_async.execute
        ~params:[ Pgx.Value.of_string (Subreddit_name.to_string subreddit) ]
        t
        "SELECT id FROM subreddits WHERE display_name = $1"
    in
    match rows with
    | [ [ id ] ] -> return id
    | _ ->
      raise_s [%message "Unexpected subreddit_id rows" (rows : Pgx.Value.t list list)]
  in
  let time =
    (match target with
    | Comment comment -> Thing.Comment.creation_time comment
    | Link link -> Thing.Link.creation_time link)
    |> Time_ns.to_string
    |> Pgx.Value.of_string
  in
  let params =
    List.concat
      [ target_fullname_params; [ author_id ]; [ subreddit_id ]; [ time ]; [ json ] ]
  in
  match%bind
    Monitor.try_with (fun () ->
        Pgx_async.execute_unit
          ~params
          t
          "INSERT INTO contents (id, author, subreddit, time, json) \
           VALUES(($1,$2),$3,$4,$5,$6)")
  with
  | Ok () -> return `Ok
  | Error exn ->
    let exn = Monitor.extract_exn exn in
    (match exn with
    | Pgx.PostgreSQL_Error ((_ : string), { code = "23505"; _ }) ->
      return `Already_recorded
    | _ ->
      raise_s
        [%message
          "Unexpected SQL error inserting contents"
            (exn : Exn.t)
            (target : Action.Target.t)])
;;

let log_rule_application t ~target ~action_summary ~author ~moderator ~subreddit ~time =
  let%bind author_id =
    match author with
    | None -> return Pgx.Value.null
    | Some username -> get_or_create_user_id t ~username
  in
  let%bind moderator_id = get_or_create_user_id t ~username:moderator in
  let target_fullname_params = target_fullname_params target in
  let subreddit_id =
    Thing.Subreddit.Id.to_int63 subreddit |> Int63.to_int_exn |> Pgx.Value.of_int
  in
  let time = Time_ns.to_string time |> Pgx.Value.of_string in
  let%bind () =
    let params =
      List.concat
        [ target_fullname_params
        ; [ Pgx.Value.of_string action_summary ]
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
       VALUES(($1,$2),$3,$4,$5,$6,$7)"
  in
  return ()
;;

let update_subscriber_counts t ~subreddits =
  Deferred.List.iter subreddits ~f:(fun subreddit ->
      let subreddit_id =
        Thing.Subreddit.id subreddit
        |> Thing.Subreddit.Id.to_int63
        |> Int63.to_int_exn
        |> Pgx.Value.of_int
      in
      let display_name =
        Thing.Subreddit.name subreddit |> Subreddit_name.to_string |> Pgx.Value.of_string
      in
      let%bind () =
        Pgx_async.execute_unit
          ~params:[ subreddit_id; display_name ]
          t
          "INSERT INTO subreddits (id, display_name) VALUES($1,$2) ON CONFLICT DO NOTHING"
      in
      let subscribers = Thing.Subreddit.subscribers subreddit |> Pgx.Value.of_int in
      Pgx_async.execute_unit
        ~params:[ subscribers; subreddit_id ]
        t
        "UPDATE subreddits SET subscribers = $1 WHERE id = $2")
;;

let update_moderator_table t ~moderators ~subreddit =
  Pgx_async.with_transaction t (fun t ->
      let subreddit_id =
        Thing.Subreddit.Id.to_int63 subreddit |> Int63.to_int_exn |> Pgx.Value.of_int
      in
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
            "INSERT INTO subreddit_moderator (subreddit_id, moderator_id) VALUES($1,$2) \
             ON CONFLICT DO NOTHING"))
;;
