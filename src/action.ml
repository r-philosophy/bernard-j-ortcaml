open! Core
open! Async
open! Import

module Target = struct
  module Kind = struct
    module T = struct
      type t =
        | Link
        | Comment
      [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  type t =
    | Link of Thing.Link.t
    | Comment of Thing.Comment.t
  [@@deriving sexp_of]

  let of_thing thing =
    match thing with
    | `Link link -> Link link
    | `Comment comment -> Comment comment
  ;;

  let kind t : Kind.t =
    match t with
    | Link _ -> Link
    | Comment _ -> Comment
  ;;

  let fullname t =
    match t with
    | Link link -> `Link (Thing.Link.id link)
    | Comment comment -> `Comment (Thing.Comment.id comment)
  ;;

  let author t =
    match t with
    | Link link -> Thing.Link.author link
    | Comment comment -> Thing.Comment.author comment
  ;;

  let get_field_exn t =
    match t with
    | Link link -> Thing.Link.get_field_exn link
    | Comment comment -> Thing.Comment.get_field_exn comment
  ;;
end

module Automod_action_buffers = struct
  type t = string Queue.t String.Table.t [@@deriving sexp_of]

  let create () = String.Table.create ()

  let add t ~placeholder ~value =
    let queue = Hashtbl.find_or_add t placeholder ~default:Queue.create in
    Queue.enqueue queue value
  ;;

  let unescape =
    let unsafe_characters =
      [ "&amp", "&"; "&lt", "<"; "&gt", ">" ]
      |> List.map ~f:(Tuple2.map_fst ~f:(Fn.compose Re.compile Re.str))
    in
    fun html_string ->
      List.fold unsafe_characters ~init:html_string ~f:(fun string (regex, replacement) ->
          Re.replace_string regex ~by:replacement string)
  ;;

  let update_wiki_page ?reason page connection ~f =
    let%bind wiki_page =
      retry_or_fail retry_manager [%here] ~f:(fun () -> Api.wiki_page ~page connection)
    in
    let content = Wiki_page.content wiki_page `markdown in
    let revision_id = Wiki_page.revision_id wiki_page in
    let rec loop content =
      let new_content = f content in
      match%bind
        retry_or_fail retry_manager [%here] ~f:(fun () ->
            Api.edit_wiki_page
              ~previous:revision_id
              ~content:new_content
              ?reason
              ~page
              connection)
      with
      | Ok () -> return ()
      | Error conflict -> loop (Wiki_page.Edit_conflict.new_content conflict)
    in
    loop content
  ;;

  let commit_one buffer ~connection ~subreddit ~placeholder =
    let transform_page =
      let regex = Re.compile (Re.str placeholder) in
      let replacement = String.concat (placeholder :: Queue.to_list buffer) ~sep:", " in
      fun content ->
        let content = unescape content in
        Re.replace_string regex ~by:replacement content
    in
    let page : Wiki_page.Id.t =
      { subreddit = Some subreddit; page = "config/automoderator" }
    in
    let%bind () = update_wiki_page page connection ~f:transform_page in
    Queue.clear buffer;
    return ()
  ;;

  let commit_all (t : t) ~connection ~subreddit =
    Hashtbl.fold t ~init:Deferred.unit ~f:(fun ~key:placeholder ~data:buffer _ ->
        commit_one buffer ~connection ~subreddit ~placeholder)
  ;;
end

module Action_buffers = struct
  type t = { automod : Automod_action_buffers.t } [@@deriving sexp_of, fields]

  let create () = { automod = Automod_action_buffers.create () }

  let commit_all t ~connection ~subreddit =
    let commit_one f _ _ buffer = f buffer ~connection ~subreddit in
    Fields.Direct.iter t ~automod:(commit_one Automod_action_buffers.commit_all)
  ;;
end

let lock target ~connection =
  retry_or_fail retry_manager [%here] ~f:(fun () ->
      Api.lock ~id:(Target.fullname target) connection)
;;

let complete_ban_message message (target : Target.t) =
  let kind_string =
    match target with
    | Link _ -> "post"
    | Comment _ -> "comment"
  in
  let permalink = Json.get_string (Target.get_field_exn target "permalink") in
  let footer =
    sprintf
      "\n\nThis action was taken because of the following %s: %s"
      kind_string
      (Uri.pct_encode permalink)
  in
  message ^ footer
;;

let remove target ~connection =
  retry_or_fail retry_manager [%here] ~f:(fun () ->
      Api.remove ~id:(Target.fullname target) connection)
;;

let ban target ~connection ~subreddit ~duration ~message ~reason =
  let ban_message = complete_ban_message message target in
  let author = Target.author target in
  retry_or_fail retry_manager [%here] ~f:(fun () ->
      Api.add_relationship
        ~relationship:Banned
        ~username:author
        ~subreddit
        ~duration
        ~ban_message
        ~ban_reason:reason
        connection)
;;

let nuke (target : Target.t) ~connection =
  let link =
    match target with
    | Link link -> Thing.Link.id link
    | Comment comment -> Thing.Comment.link comment
  in
  let comment =
    match target with
    | Comment comment -> Some (Thing.Comment.id comment)
    | Link _ -> None
  in
  let%bind comment_response =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.comments ?comment connection ~link)
  in
  Iter_comments.iter_comments
    connection
    ~retry_manager
    ~comment_response
    ~f:(fun comment ->
      let id = `Comment (Thing.Comment.id comment) in
      retry_or_fail retry_manager [%here] ~f:(fun () -> Api.remove ~id connection))
;;

let modmail (target : Target.t) ~connection ~subject ~body ~subreddit =
  let%bind (_ : Modmail.Conversation.t) =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.create_modmail_conversation
          ~subject
          ~body
          ~to_:(Target.author target)
          ~subreddit
          ~hide_author:true
          connection)
  in
  return ()
;;

let notification_footer =
  "\n\n\
   -----\n\n\
   This is a shared account that is only used for notifications. Please do not reply, as \
   your message will go unread."
;;

let notify (target : Target.t) ~connection ~text =
  let comment_text = text ^ notification_footer in
  let parent =
    match target with
    | Link link -> raise_s [%message "Unexpected link" (link : Thing.Link.t)]
    | Comment comment -> `Comment (Thing.Comment.id comment)
  in
  (* TODO: Maybe it's too old *)
  let%bind notification =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.add_comment ~parent ~text:comment_text connection)
  in
  let id = `Comment (Thing.Comment.id notification) in
  let%bind (_ : [ `Link of Thing.Link.t | `Comment of Thing.Comment.t ]) =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.distinguish ~id ~how:Mod connection)
  in
  return ()
;;

module Automod_key = struct
  type t =
    | Author
    | Domain
  [@@deriving sexp, compare, equal]
end

let enqueue_automod_action target ~(key : Automod_key.t) ~placeholder ~buffers =
  let string_to_add =
    match key with
    | Author -> Username.to_string (Target.author target)
    | Domain -> Json.get_string (Target.get_field_exn target "domain")
  in
  Automod_action_buffers.add buffers ~placeholder ~value:string_to_add;
  return ()
;;

type t =
  | Ban of
      { message : string
      ; reason : string
      ; duration : Api.Parameters.Relationship.Duration.t
      }
  | Lock
  | Nuke
  | Modmail of
      { subject : string
      ; body : string
      }
  | Notify of { text : string }
  | Remove
  | Watch_via_automod of
      { key : Automod_key.t
      ; placeholder : string
      }
[@@deriving sexp, compare, equal]

let act t ~target ~connection ~subreddit ~(action_buffers : Action_buffers.t) =
  match t with
  | Ban { message; reason; duration } ->
    ban target ~connection ~subreddit ~message ~reason ~duration
  | Lock -> lock target ~connection
  | Nuke -> nuke target ~connection
  | Modmail { subject; body } -> modmail target ~connection ~subject ~body ~subreddit
  | Notify { text } -> notify target ~connection ~text
  | Remove -> remove target ~connection
  | Watch_via_automod { key; placeholder } ->
    let buffers = action_buffers.automod in
    enqueue_automod_action target ~key ~placeholder ~buffers
;;
