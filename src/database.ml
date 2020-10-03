open! Core
open! Async
open! Import

type t = Pgx_async.t

let create ~database = Pgx_async.connect ~database ()

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

(*
  val log_rule_application
    :  t
    -> target:Action.Target.t
    -> rule_description:string
    -> moderator:Username.t
    -> time:Time_ns.t
    -> unit
    *)

let log_rule_application t ~target ~action_summary ~author ~moderator ~subreddit_id ~time =
  let%bind author_id = get_or_create_user_id t ~username:author in
  let%bind moderator_id = get_or_create_user_id t ~username:moderator in
  let subreddit_id = Thing.Subreddit.Id.to_int subreddit_id |> Pgx_value.of_int in
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
