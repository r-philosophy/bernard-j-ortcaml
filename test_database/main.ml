open! Core
open! Async
open Reddit_api_kernel
module Database = Bernard_j_ortcutt.Database

let database_param =
  let%map_open.Command database =
    flag
      "-database"
      (required (Arg_type.map string ~f:Uri.of_string))
      ~doc:"STRING postgres database"
  in
  match Caqti_async.connect_pool database with
  | Ok v -> v
  | Error error -> raise (Caqti_error.Exn error)
;;

let test_suite database link_file subreddit_file =
  let link =
    Thing.Link.of_json (Or_error.ok_exn (Json.of_string (In_channel.read_all link_file)))
  in
  let subreddit =
    Thing.Subreddit.of_json
      (Or_error.ok_exn (Json.of_string (In_channel.read_all subreddit_file)))
  in
  let target : Bernard_j_ortcutt.Action.Target.t = Link link in
  let author = Thing.Link.author link in
  let subreddit_id = Thing.Subreddit.Id.of_string "2qizd" in
  let moderator = Username.of_string "spez" in
  let%bind () = Database.update_subscriber_counts database ~subreddits:[ subreddit ] in
  let%bind () =
    Database.update_moderator_table
      database
      ~moderators:[ moderator ]
      ~subreddit:subreddit_id
  in
  let%bind already_acted = Database.already_acted database ~target ~moderator in
  assert (not already_acted);
  let%bind () =
    Database.log_rule_application
      database
      ~target:(Link link)
      ~action_summary:"An action"
      ~author
      ~moderator
      ~subreddit:subreddit_id
      ~time:Time_ns.epoch
  in
  let%bind already_acted = Database.already_acted database ~target ~moderator in
  assert already_acted;
  let%bind () =
    match%bind Database.record_contents database ~target with
    | `Ok -> return ()
    | `Already_recorded -> assert false
  in
  let%bind () =
    match%bind Database.record_contents database ~target with
    | `Already_recorded -> return ()
    | `Ok -> assert false
  in
  return ()
;;

let command =
  Command.async
    ~summary:"Test the database"
    (let%map_open.Command database = database_param
     and link_file =
       flag "-link" (required Filename_unix.arg_type) ~doc:"FILENAME Link JSON file"
     and subreddit_file =
       flag
         "-subreddit"
         (required Filename_unix.arg_type)
         ~doc:"FILENAME Subreddit JSON file"
     in
     fun () -> test_suite database link_file subreddit_file)
;;

let () = Command_unix.run command
