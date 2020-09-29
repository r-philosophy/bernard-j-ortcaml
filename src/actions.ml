open! Core
open! Async
open Reddit_api

let retry_manager = failwith "asdf"

let retry_or_fail retry_manager here ~f =
  match%bind Retry_manager.call retry_manager f with
  | Ok v -> return v
  | Error (response, body) ->
    let%bind body = Cohttp_async.Body.to_string body in
    raise_s
      [%message
        "Reddit returned error"
          (here : Source_code_position.t)
          (response : Cohttp.Response.t)
          (body : string)]
;;

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

  val tag : string
  val create : Config.t -> t
  val action : t -> Connection.t -> Target.t -> moderator:Username.t -> unit Deferred.t
  val after : (t -> Connection.t -> unit Deferred.t) option
end

module Lock = struct
  module Config = Unit

  type t = unit [@@deriving sexp]

  let tag = "Lock"
  let create = ident

  let action () connection (target : Target.t) ~moderator:_ =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.lock ~id:(Target.fullname target) connection)
  ;;

  let after = None
end

module Ban = struct
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

  let tag = "Ban"
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
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.add_relationship
          ~relationship:Banned
          ~username:author
          ~subreddit:t.subreddit
          ~duration:t.duration
          ~ban_message
          ~ban_reason:t.reason
          connection)
  ;;

  let after = None
end

module Nuke = struct
  module Config = Unit

  type t = unit [@@deriving sexp]

  let tag = "Nuke"
  let create = ident

  let action () connection (target : Target.t) ~moderator:_ =
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

  let after = None
end

module Modmail = struct
  module Config = struct
    type t =
      { subject : string
      ; body : string
      ; subreddit : Subreddit_name.t
      }
    [@@deriving sexp]
  end

  include Config

  let tag = "Modmail"
  let create = ident

  let action { subject; body; subreddit } connection (target : Target.t) ~moderator:_ =
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

  let after = None
end

module Notify = struct
  module Config = struct
    type t = { text : string } [@@deriving sexp]
  end

  include Config

  let tag = "Notify"
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

  let after = None
end

module Watch_via_automod = struct
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

  let tag = "Watch_via_automod"

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

let modules_by_tag =
  let modules : (module S) list =
    [ (module Lock)
    ; (module Ban)
    ; (module Nuke)
    ; (module Modmail)
    ; (module Notify)
    ; (module Watch_via_automod)
    ]
  in
  List.map modules ~f:(fun ((module M : S) as m) -> M.tag, m)
  |> String.Caseless.Map.of_alist_exn
;;

module Config = struct
  type t = T : (module S with type Config.t = 'config) * 'config -> t

  let t_of_sexp sexp =
    let module_tag, config_sexp = [%of_sexp: string * Sexp.t] sexp in
    let (module M) = Map.find_exn modules_by_tag module_tag in
    T ((module M), [%of_sexp: M.Config.t] config_sexp)
  ;;

  let sexp_of_t (T ((module M), config)) = [%sexp_of: string * M.Config.t] (M.tag, config)
end

type t = T : (module S with type t = 't) * 't -> t

let create (T ((module M), config) : Config.t) = T ((module M), M.create config)
