open! Core
open! Async
open Reddit_api

module Target = struct
  type t =
    | Link of Thing.Link.t
    | Comment of Thing.Comment.t
  [@@deriving sexp_of]

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

module type S = sig
  module Config : Sexpable

  type t [@@deriving sexp_of]

  val create : Config.t -> t
  val action : t -> Connection.t -> Target.t -> moderator:Username.t -> unit Deferred.t
  val after : (t -> Connection.t -> unit Deferred.t) option
end

module Locker : S = struct
  module Config = Unit

  type t = unit [@@deriving sexp]

  let create = ident

  let action () connection (target : Target.t) ~moderator:_ =
    Api.Exn.lock ~id:(Target.fullname target) connection
  ;;

  let after = None
end

module Banner : S = struct
  module Config = struct
    type t =
      { message : string
      ; reason : string
      ; duration : Api.Parameters.Relationship.Duration.t
      ; subreddit : Subreddit_name.t
      }
    [@@deriving sexp]
  end

  type t = Config.t [@@deriving sexp]

  let create = ident

  let complete_message (t : t) (target : Target.t) =
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
    t.message ^ footer
  ;;

  let action (t : t) connection target ~moderator:_ =
    let ban_message = complete_message t target in
    let author = Target.author target in
    Api.Exn.add_relationship
      ~relationship:Banned
      ~username:author
      ~subreddit:t.subreddit
      ~duration:t.duration
      ~ban_message
      ~ban_reason:t.reason
      connection
  ;;

  let after = None
end

module Modmailer : S = struct
  module Config = struct
    type t =
      { subject : string
      ; body : string
      ; subreddit : Subreddit_name.t
      }
    [@@deriving sexp]
  end

  include Config

  let create = ident

  let action { subject; body; subreddit } connection (target : Target.t) ~moderator:_ =
    let%bind (_ : Modmail.Conversation.t) =
      Api.Exn.create_modmail_conversation
        ~subject
        ~body
        ~to_:(Target.author target)
        ~subreddit
        ~hide_author:true
        connection
    in
    return ()
  ;;

  let after = None
end

module Notifier : S = struct
  module Config = struct
    type t = { text : string } [@@deriving sexp]
  end

  include Config

  let create = ident

  let footer =
    "\n\n\
     -----\n\n\
     This is a shared account that is only used for notifications. Please do not reply, \
     as your message will go unread."
  ;;

  let action { text } connection (target : Target.t) ~moderator:_ =
    let comment_text = text ^ footer in
    let parent =
      match target with
      | Link link -> raise_s [%message "Unexpected link" (link : Thing.Link.t)]
      | Comment comment -> `Comment (Thing.Comment.id comment)
    in
    (* TODO: Maybe it's too old *)
    let%bind notification = Api.Exn.add_comment ~parent ~text:comment_text connection in
    let id = `Comment (Thing.Comment.id notification) in
    let%bind (_ : [ `Link of Thing.Link.t | `Comment of Thing.Comment.t ]) =
      Api.Exn.distinguish ~id ~how:Mod connection
    in
    return ()
  ;;

  let after = None
end

module Automod_watcher : S = struct
  module Target_attribute = struct
    type t =
      | Author
      | Domain
    [@@deriving sexp]
  end

  module Config = struct
    type t =
      { target_attribute : Target_attribute.t
      ; placeholder : string
      ; subreddit : Subreddit_name.t
      }
    [@@deriving sexp]
  end

  type t =
    { target_attribute : Target_attribute.t
    ; placeholder : string
    ; buffer : string Queue.t
    ; subreddit : Subreddit_name.t
    }
  [@@deriving sexp_of]

  let create ({ target_attribute; placeholder; subreddit } : Config.t) =
    { target_attribute; placeholder; subreddit; buffer = Queue.create () }
  ;;

  let action { target_attribute; buffer; _ } _connection target ~moderator:_ =
    let string_to_add =
      match target_attribute with
      | Author -> Username.to_string (Target.author target)
      | Domain -> Json.get_string (Target.get_field_exn target "domain")
    in
    Queue.enqueue buffer string_to_add;
    return ()
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
    let%bind wiki_page = Api.Exn.wiki_page ~page connection in
    let content = Wiki_page.content wiki_page `markdown in
    let revision_id = Wiki_page.revision_id wiki_page in
    let rec loop content =
      let new_content = f content in
      match%bind
        Api.Exn.edit_wiki_page
          ~previous:revision_id
          ~content:new_content
          ?reason
          ~page
          connection
      with
      | Ok () -> return ()
      | Error conflict -> loop (Wiki_page.Edit_conflict.new_content conflict)
    in
    loop content
  ;;

  let write_buffer { subreddit; placeholder; buffer; _ } connection =
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

  let after = Some write_buffer
end
