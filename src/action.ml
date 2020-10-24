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

  let usernote_context t : Usernote_page.Note.Context.t =
    match t with
    | Link link -> Link (Thing.Link.id link)
    | Comment comment ->
      let link = Thing.Comment.link comment in
      Comment (link, Thing.Comment.id comment)
  ;;

  let moderator_reports t =
    match t with
    | Link link -> Thing.Link.moderator_reports link
    | Comment comment -> Thing.Comment.moderator_reports comment
  ;;

  let permalink t =
    match t with
    | Link link -> Thing.Link.permalink link
    | Comment comment -> Thing.Comment.permalink comment
  ;;
end

let update_wiki_page ?reason page connection ~retry_manager ~f =
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

  let commit_one buffer ~connection ~retry_manager ~subreddit ~placeholder =
    match Queue.to_list buffer with
    | [] -> return ()
    | to_insert ->
      Queue.clear buffer;
      let transform_page =
        let regex = Re.compile (Re.str placeholder) in
        let replacement = String.concat (placeholder :: to_insert) ~sep:", " in
        fun content ->
          let content = unescape content in
          Re.replace_string regex ~by:replacement content
      in
      let page : Wiki_page.Id.t =
        { subreddit = Some subreddit; page = "config/automoderator" }
      in
      update_wiki_page page connection ~retry_manager ~f:transform_page
  ;;

  let commit_all (t : t) ~connection ~retry_manager ~subreddit =
    Hashtbl.fold t ~init:Deferred.unit ~f:(fun ~key:placeholder ~data:buffer _ ->
        commit_one buffer ~connection ~retry_manager ~subreddit ~placeholder)
  ;;
end

let lock id ~connection ~retry_manager =
  retry_or_fail retry_manager [%here] ~f:(fun () -> Api.lock ~id connection)
;;

let complete_ban_message message (target : Target.t) =
  let kind_string =
    match target with
    | Link _ -> "post"
    | Comment _ -> "comment"
  in
  let permalink = Target.permalink target in
  let footer =
    sprintf
      "\n\nThis action was taken because of the following %s: %s"
      kind_string
      (Uri.to_string permalink)
  in
  message ^ footer
;;

let remove target ~connection ~retry_manager =
  retry_or_fail retry_manager [%here] ~f:(fun () ->
      Api.remove ~id:(Target.fullname target) ~spam:false connection)
;;

let ban target ~connection ~retry_manager ~subreddit ~duration ~message ~reason =
  match Target.author target with
  | None ->
    Log.Global.info_s
      [%message
        "Skipping Ban action due to deleted author"
          ~target:(Target.fullname target : Thing.Fullname.t)];
    return ()
  | Some author ->
    let ban_message = complete_ban_message message target in
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

let nuke (target : Target.t) ~connection ~retry_manager =
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
      retry_or_fail retry_manager [%here] ~f:(fun () ->
          Api.remove ~id ~spam:false connection))
;;

let modmail (target : Target.t) ~connection ~retry_manager ~subject ~body ~subreddit =
  match Target.author target with
  | None ->
    Log.Global.info_s
      [%message
        "Skipping Modmail action due to deleted author"
          ~target:(Target.fullname target : Thing.Fullname.t)];
    return ()
  | Some author ->
    let%bind (_ : Modmail.Conversation.t) =
      retry_or_fail retry_manager [%here] ~f:(fun () ->
          Api.create_modmail_conversation
            ~subject
            ~body
            ~to_:(User author)
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

let notify (target : Target.t) ~connection ~retry_manager ~text =
  let comment_text = text ^ notification_footer in
  let parent = Target.fullname target in
  (* TODO: Maybe it's too old *)
  let%bind notification =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.add_comment ~parent ~text:comment_text connection)
  in
  let notification_id = `Comment (Thing.Comment.id notification) in
  let sticky =
    match Target.kind target with
    | Link -> Some true
    | Comment -> None
  in
  let%bind (_ : [ `Link of Thing.Link.t | `Comment of Thing.Comment.t ]) =
    retry_or_fail retry_manager [%here] ~f:(fun () ->
        Api.distinguish ?sticky ~id:notification_id ~how:Mod connection)
  and () = lock notification_id ~connection ~retry_manager in
  return ()
;;

module Automod_key = struct
  type t =
    | Author
    | Domain
  [@@deriving sexp, compare, equal]
end

let enqueue_automod_action
    (target : Target.t)
    ~(key : Automod_key.t)
    ~placeholder
    ~buffers
  =
  let string_to_add =
    match key with
    | Domain ->
      let domain =
        match target with
        | Link link -> Thing.Link.domain link
        | Comment _ ->
          raise_s
            [%message
              "Got comment in (Watch_via_automod (key Domain))" (target : Target.t)]
      in
      Some domain
    | Author ->
      (match Target.author target with
      | Some author -> Some (Username.to_string author)
      | None ->
        Log.Global.info_s
          [%message
            "Skipping Watch_via_automod action due to deleted author"
              ~target:(Target.fullname target : Thing.Fullname.t)];
        None)
  in
  Option.iter string_to_add ~f:(fun value ->
      Automod_action_buffers.add buffers ~placeholder ~value)
;;

type t =
  | Add_usernote of
      { level : string
      ; text : string
      }
  | Ban of
      { message : string
      ; reason : string
      ; duration : Api.Parameters.Relationship_spec.Duration.t
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

module Usernote_action_buffers = struct
  type t = (Username.t * Usernote_page.Note.Spec.t) Queue.t [@@deriving sexp_of]

  let create () = Queue.create ()
  let add t ~user ~note = Queue.enqueue t (user, note)

  let commit t ~connection ~retry_manager ~subreddit =
    match Queue.to_list t with
    | [] -> return ()
    | notes ->
      Queue.clear t;
      let transform_page page =
        let page = Json.of_string page |> Usernote_page.of_json in
        List.iter notes ~f:(fun (username, spec) ->
            Usernote_page.add_note page ~username ~spec);
        Usernote_page.to_json page |> Json.value_to_string
      in
      let page : Wiki_page.Id.t = { subreddit = Some subreddit; page = "usernotes" } in
      update_wiki_page page connection ~retry_manager ~f:transform_page
  ;;
end

module Action_buffers = struct
  type t =
    { automod : Automod_action_buffers.t
    ; usernote : Usernote_action_buffers.t
    }
  [@@deriving sexp_of, fields]

  let create () =
    { automod = Automod_action_buffers.create ()
    ; usernote = Usernote_action_buffers.create ()
    }
  ;;

  let commit_all { automod; usernote } ~connection ~retry_manager ~subreddit =
    Deferred.all_unit
      [ Automod_action_buffers.commit_all automod ~connection ~retry_manager ~subreddit
      ; Usernote_action_buffers.commit usernote ~connection ~retry_manager ~subreddit
      ]
  ;;
end

let act
    t
    ~target
    ~connection
    ~retry_manager
    ~subreddit
    ~moderator
    ~time
    ~(action_buffers : Action_buffers.t)
  =
  match t with
  | Add_usernote { level; text } ->
    let user = Target.author target in
    (match user with
    | None -> return ()
    | Some user ->
      let buffers = action_buffers.usernote in
      Usernote_action_buffers.add
        buffers
        ~user
        ~note:
          { text
          ; warning = level
          ; moderator
          ; time
          ; context = Target.usernote_context target
          };
      return ())
  | Ban { message; reason; duration } ->
    ban target ~connection ~retry_manager ~subreddit ~message ~reason ~duration
  | Lock -> lock (Target.fullname target) ~connection ~retry_manager
  | Nuke -> nuke target ~connection ~retry_manager
  | Modmail { subject; body } ->
    modmail target ~connection ~retry_manager ~subject ~body ~subreddit
  | Notify { text } -> notify target ~connection ~retry_manager ~text
  | Remove -> remove target ~connection ~retry_manager
  | Watch_via_automod { key; placeholder } ->
    let buffers = action_buffers.automod in
    enqueue_automod_action target ~key ~placeholder ~buffers;
    return ()
;;

let will_remove t =
  match t with
  | Remove | Nuke -> true
  | Add_usernote _ | Ban _ | Lock | Modmail _ | Notify _ | Watch_via_automod _ -> false
;;
