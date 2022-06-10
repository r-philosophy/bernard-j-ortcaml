open! Core
open! Async
open! Import

module Types = struct
  let custom_type base ~encode ~decode =
    let lift f v = f v |> Result.map_error ~f:Error.to_string_hum in
    Caqti_type.custom base ~encode:(lift encode) ~decode:(lift decode)
  ;;

  let try_with_or_error f v = Or_error.try_with (fun () -> f v)

  let fullname =
    custom_type
      Caqti_type.(tup2 int int)
      ~encode:(function
        | `Comment c -> Ok (1, Thing.Comment.Id.to_int63 c |> Int63.to_int_exn)
        | `Link l -> Ok (3, Thing.Link.Id.to_int63 l |> Int63.to_int_exn))
      ~decode:(function
        | 1, id -> Ok (`Comment (Int63.of_int id |> Thing.Comment.Id.of_int63))
        | 3, id -> Ok (`Link (Int63.of_int id |> Thing.Link.Id.of_int63))
        | kind, _ -> Or_error.error_s [%message "Unrecognized thing kind" (kind : int)])
  ;;

  let username =
    custom_type
      Caqti_type.string
      ~encode:(try_with_or_error Username.to_string)
      ~decode:(try_with_or_error Username.of_string)
  ;;

  let subreddit_name =
    custom_type
      Caqti_type.string
      ~encode:(try_with_or_error Subreddit_name.to_string)
      ~decode:(try_with_or_error Subreddit_name.of_string)
  ;;

  let subreddit_id =
    custom_type
      Caqti_type.int
      ~encode:
        (try_with_or_error (fun id -> Thing.Subreddit.Id.to_int63 id |> Int63.to_int_exn))
      ~decode:
        (try_with_or_error (fun int -> Int63.of_int int |> Thing.Subreddit.Id.of_int63))
  ;;

  let thing =
    custom_type
      Caqti_type.string
      ~encode:(fun (thing : Action.Target.t) ->
        let json =
          match thing with
          | Comment comment -> [%jsonaf_of: Thing.Comment.t] comment
          | Link link -> [%jsonaf_of: Thing.Link.t] link
        in
        Ok (Jsonaf.to_string json))
      ~decode:(fun s ->
        let open Or_error.Let_syntax in
        let%bind json = Jsonaf.parse s in
        match [%of_jsonaf: Thing.Poly.t] json with
        | `Comment comment -> Ok (Action.Target.Comment comment)
        | `Link link -> Ok (Link link)
        | _ -> Or_error.error_s [%message "Unexpected thing JSON" (json : Jsonaf.t)])
  ;;

  let time =
    custom_type
      Caqti_type.ptime
      ~encode:(fun time_ns ->
        match
          let%bind.Option ptime_span =
            Time_ns.to_time_float_round_nearest time_ns
            |> Time.to_span_since_epoch
            |> Time.Span.to_sec
            |> Ptime.Span.of_float_s
          in
          Ptime.of_span ptime_span
        with
        | Some v -> Ok v
        | None -> Or_error.error_s [%message "Unrepresentable time" (time_ns : Time_ns.t)])
      ~decode:(fun ptime ->
        Ptime.to_span ptime
        |> Ptime.Span.to_float_s
        |> Time.Span.of_sec
        |> Time_ns.Span.of_span_float_round_nearest
        |> Time_ns.of_span_since_epoch
        |> Ok)
  ;;
end

module Build_request = struct
  include Caqti_type.Std
  include Caqti_request.Infix
  include Types
end

type t = (Caqti_async.connection, Caqti_error.t) Caqti_async.Pool.t

let with_t (t : t) ~f =
  match%bind Caqti_async.Pool.use f t with
  | Ok v -> return v
  | Error error -> raise (Caqti_error.Exn (error :> Caqti_error.t))
;;

let target_fullname (target : Action.Target.t) =
  match target with
  | Comment comment -> `Comment (Thing.Comment.id comment)
  | Link link -> `Link (Thing.Link.id link)
;;

let get_or_create_user_id =
  let insert_user_request =
    Build_request.(username ->. unit)
      "INSERT INTO users (username) VALUES($1) ON CONFLICT DO NOTHING"
  in
  let get_id_request =
    Build_request.(username ->! int) "SELECT id FROM users WHERE username = $1"
  in
  fun (module Connection : Caqti_async.CONNECTION) ~username ->
    let open Deferred.Result.Let_syntax in
    let%bind () = Connection.exec insert_user_request username in
    Connection.find get_id_request username
;;

let already_acted =
  let request =
    Build_request.(tup2 fullname username ->! int)
      "SELECT COUNT(1) FROM vw_actions WHERE target = ($1, $2)::thing_id AND moderator = \
       $3"
  in
  fun t ~target ~moderator ->
    with_t t ~f:(fun (module Connection) ->
        let open Deferred.Result.Let_syntax in
        let%bind action_count =
          Connection.find request (target_fullname target, moderator)
        in
        return (action_count > 0))
;;

let record_contents =
  let select_contents_request =
    Build_request.(subreddit_name ->! subreddit_id)
      "SELECT id FROM subreddits WHERE display_name = $1"
  in
  let insert_contents_request =
    Build_request.(tup2 (tup4 fullname (option int) subreddit_id time) thing ->. unit)
      "INSERT INTO contents (id, author, subreddit, time, json) \
       VALUES(($1,$2),$3,$4,$5,$6)"
  in
  fun t ~target ->
    with_t t ~f:(fun ((module Connection) as connection) ->
        let open Deferred.Result.Let_syntax in
        let%bind author_id =
          match Action.Target.author target with
          | None -> return None
          | Some username ->
            let%bind id = get_or_create_user_id connection ~username in
            return (Some id)
        in
        let%bind subreddit_id =
          let subreddit =
            match target with
            | Comment comment -> Thing.Comment.subreddit comment
            | Link link -> Thing.Link.subreddit link
          in
          Connection.find select_contents_request subreddit
        in
        let time =
          match target with
          | Comment comment -> Thing.Comment.creation_time comment
          | Link link -> Thing.Link.creation_time link
        in
        match%bind.Deferred
          Connection.exec
            insert_contents_request
            ((target_fullname target, author_id, subreddit_id, time), target)
        with
        | Ok () -> return `Ok
        | Error error ->
          let raise_error () = raise (Caqti_error.Exn error) in
          (match error with
          | (`Request_failed _ | `Response_failed _) as error ->
            (match Caqti_error.cause error with
            | `Unique_violation -> return `Already_recorded
            | _ -> raise_error ())
          | _ -> raise_error ()))
;;

let log_rule_application =
  let request =
    Build_request.(
      tup2 (tup4 fullname string (option int) int) (tup2 time subreddit_id) ->. unit)
      "INSERT INTO actions (target, action_summary, author, moderator, time, subreddit) \
       VALUES(($1,$2),$3,$4,$5,$6,$7)"
  in
  fun t ~target ~action_summary ~author ~moderator ~subreddit ~time ->
    with_t t ~f:(fun ((module Connection) as connection) ->
        let open Deferred.Result.Let_syntax in
        let%bind author_id =
          match author with
          | None -> return None
          | Some username ->
            let%bind id = get_or_create_user_id connection ~username in
            return (Some id)
        in
        let%bind moderator_id = get_or_create_user_id connection ~username:moderator in
        let target_fullname = target_fullname target in
        Connection.exec
          request
          ((target_fullname, action_summary, author_id, moderator_id), (time, subreddit)))
;;

let update_subscriber_counts =
  let insert_subreddit_request =
    Build_request.(tup2 subreddit_id subreddit_name ->. unit)
      "INSERT INTO subreddits (id, display_name) VALUES($1,$2) ON CONFLICT DO NOTHING"
  in
  let update_subscribers_request =
    Build_request.(tup2 int subreddit_id ->. unit)
      "UPDATE subreddits SET subscribers = $1 WHERE id = $2"
  in
  fun t ~subreddits ->
    with_t t ~f:(fun (module Connection) ->
        Deferred.List.map subreddits ~f:(fun subreddit ->
            let open Deferred.Result.Let_syntax in
            let subreddit_id = Thing.Subreddit.id subreddit in
            let display_name = Thing.Subreddit.name subreddit in
            let%bind () =
              Connection.exec insert_subreddit_request (subreddit_id, display_name)
            in
            let subscribers = Thing.Subreddit.subscribers subreddit in
            Connection.exec update_subscribers_request (subscribers, subreddit_id))
        >>| Result.all_unit)
;;

let update_moderator_table =
  let delete_request =
    Build_request.(subreddit_id ->. unit)
      "DELETE FROM subreddit_moderator WHERE subreddit_id = $1"
  in
  let insert_request =
    Build_request.(tup2 subreddit_id int ->. unit)
      "INSERT INTO subreddit_moderator (subreddit_id, moderator_id) VALUES($1,$2) ON \
       CONFLICT DO NOTHING"
  in
  fun t ~moderators ~subreddit ->
    with_t t ~f:(fun ((module Connection) as connection) ->
        let%bind.Deferred.Result () = Connection.exec delete_request subreddit in
        Deferred.List.map moderators ~f:(fun moderator ->
            let open Deferred.Result.Let_syntax in
            let%bind user_id = get_or_create_user_id connection ~username:moderator in
            Connection.exec insert_request (subreddit, user_id))
        >>| Result.all_unit)
;;
