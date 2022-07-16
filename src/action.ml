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

let update_wiki_page ?reason page ~retry_manager ~f =
  let%bind wiki_page =
    retry_or_fail retry_manager [%here] (Endpoint.wiki_page ~page ())
  in
  let content = Wiki_page.content wiki_page `markdown in
  let revision_id = Wiki_page.revision_id wiki_page in
  let rec loop content ~previous =
    let new_content = f content in
    match%bind
      retry_or_fail
        retry_manager
        [%here]
        (Endpoint.edit_wiki_page ~previous ~content:new_content ?reason ~page ())
    with
    | Ok () -> return ()
    | Error conflict ->
      loop
        (Wiki_page.Edit_conflict.new_content conflict)
        ~previous:(Wiki_page.Edit_conflict.new_revision conflict)
  in
  let%bind () = loop content ~previous:revision_id in
  Clock_ns.after Time_ns.Span.second
;;

module Automod_action_buffers = struct
  type t = string Queue.t Hashtbl.M(String).t [@@deriving sexp_of]

  let create () = Hashtbl.create (module String)

  let add t ~placeholder ~value =
    let queue = Hashtbl.find_or_add t placeholder ~default:Queue.create in
    Queue.enqueue queue value
  ;;

  let unescape =
    let unsafe_characters =
      [ "&amp", "&"; "&lt", "<"; "&gt", ">" ]
      |> List.map
           ~f:(Tuple2.map_fst ~f:(String.Search_pattern.create ~case_sensitive:true))
    in
    fun html_string ->
      List.fold
        unsafe_characters
        ~init:html_string
        ~f:(fun string (pattern, replacement) ->
          String.Search_pattern.replace_all pattern ~in_:string ~with_:replacement)
  ;;

  let commit_one buffer ~retry_manager ~subreddit ~placeholder =
    match Queue.to_list buffer with
    | [] -> return ()
    | to_insert ->
      Queue.clear buffer;
      let transform_page =
        let pattern = String.Search_pattern.create ~case_sensitive:true placeholder in
        let replacement = String.concat (placeholder :: to_insert) ~sep:",\n  " in
        fun content ->
          let content = unescape content in
          String.Search_pattern.replace_all pattern ~in_:content ~with_:replacement
      in
      let page : Wiki_page.Id.t =
        { subreddit = Some subreddit; page = "config/automoderator" }
      in
      update_wiki_page page ~retry_manager ~f:transform_page
  ;;

  let commit_all (t : t) ~retry_manager ~subreddit =
    Hashtbl.fold t ~init:Deferred.unit ~f:(fun ~key:placeholder ~data:buffer _ ->
        commit_one buffer ~retry_manager ~subreddit ~placeholder)
  ;;
end

let lock id ~retry_manager =
  match%bind Retry_manager.call retry_manager (Endpoint.lock ~id) with
  | Ok () -> return ()
  | Error
      (Endpoint_error (Http_error { response = { status = `Bad_request; _ }; body = _ }))
    ->
    Log.Global.info_s [%message "Target was to old to lock."];
    return ()
  | Error error ->
    raise_s
      [%message
        "Unexpected error while locking target."
          ((id :> Thing.Fullname.t) : Thing.Fullname.t)
          (error : Retry_manager.Permanent_error.t)]
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

let remove target ~retry_manager =
  retry_or_fail
    retry_manager
    [%here]
    (Endpoint.remove ~id:(Target.fullname target) ~spam:false)
;;

let ban target ~retry_manager ~subreddit ~duration ~message ~reason =
  match Target.author target with
  | None ->
    Log.Global.info_s
      [%message
        "Skipping Ban action due to deleted author"
          ~target:(Target.fullname target : Thing.Fullname.t)];
    return ()
  | Some author ->
    let ban_message =
      Option.map message ~f:(fun message -> complete_ban_message message target)
    in
    retry_or_fail
      retry_manager
      [%here]
      (Endpoint.add_relationship
         ~relationship:Banned
         ~username:author
         ~subreddit
         ~duration
         ?ban_message
         ~ban_reason:reason
         ())
;;

let nuke (target : Target.t) ~retry_manager =
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
    retry_or_fail retry_manager [%here] (Endpoint.comments ?comment () ~link)
  in
  Iter_comments.iter_comments
    retry_manager
    ~log:(force Log.Global.log)
    ~comment_response
    ~f:(fun comment ->
      let id = `Comment (Thing.Comment.id comment) in
      retry_or_fail retry_manager [%here] (Endpoint.remove ~id ~spam:false))
;;

let modmail (target : Target.t) ~retry_manager ~subject ~body ~subreddit =
  match Target.author target with
  | None ->
    Log.Global.info_s
      [%message
        "Skipping Modmail action due to deleted author"
          ~target:(Target.fullname target : Thing.Fullname.t)];
    return ()
  | Some author ->
    let%bind (_ : Modmail.Conversation.t) =
      retry_or_fail
        retry_manager
        [%here]
        (Endpoint.create_modmail_conversation
           ~subject
           ~body
           ~to_:(User author)
           ~subreddit
           ~hide_author:true)
    in
    return ()
;;

let set_flair link ~retry_manager ~template ~subreddit =
  retry_or_fail
    retry_manager
    [%here]
    (Endpoint.select_flair ~flair_template_id:template () ~subreddit ~target:(Link link))
;;

let notification_footer =
  "\n\n\
   -----\n\n\
   This is a shared account that is only used for notifications. Please do not reply, as \
   your message will go unread."
;;

let notify (target : Target.t) ~retry_manager ~text =
  let comment_text = text ^ notification_footer in
  let parent = Target.fullname target in
  (* TODO: Maybe it's too old *)
  let%bind notification =
    retry_or_fail
      retry_manager
      [%here]
      (Endpoint.add_comment ~parent ~text:comment_text ())
  in
  let notification_id = `Comment (Thing.Comment.id notification) in
  let sticky =
    match Target.kind target with
    | Link -> Some true
    | Comment -> None
  in
  let%bind (_ : [ `Link of Thing.Link.t | `Comment of Thing.Comment.t ]) =
    retry_or_fail
      retry_manager
      [%here]
      (Endpoint.distinguish ?sticky ~id:notification_id ~how:Mod ())
  and () = lock notification_id ~retry_manager in
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

module Uuid = struct
  include Uuid
  include (Uuid.Unstable : Sexpable.S with type t := t)
end

type t =
  | Add_usernote of
      { level : string
      ; text : string
      }
  | Ban of
      { message : string option
      ; reason : string
      ; duration : Endpoint.Parameters.Relationship_spec.Duration.t
      }
  | Cleanup_thread
  | Lock
  | Nuke
  | Modmail of
      { subject : string
      ; body : string
      }
  | Notify of { text : string }
  | Remove
  | Set_flair of { template : Uuid.t }
  | Watch_via_automod of
      { key : Automod_key.t
      ; placeholder : string
      }
[@@deriving sexp, compare, equal]

module Usernote_action_buffers = struct
  type t = (Username.t * Usernote_page.Note.Spec.t) Queue.t [@@deriving sexp_of]

  let create () = Queue.create ()
  let add t ~user ~note = Queue.enqueue t (user, note)

  let commit t ~retry_manager ~subreddit =
    match Queue.to_list t with
    | [] -> return ()
    | notes ->
      Queue.clear t;
      let transform_page page =
        match Jsonaf.parse page with
        | Error error ->
          raise_s
            [%message
              "Usernote page contained invalid json" (page : string) (error : Error.t)]
        | Ok json ->
          let page = [%of_jsonaf: Usernote_page.t] json in
          List.iter notes ~f:(fun (username, spec) ->
              Usernote_page.add_note page ~username ~spec);
          [%jsonaf_of: Usernote_page.t] page |> Jsonaf.to_string
      in
      let page : Wiki_page.Id.t = { subreddit = Some subreddit; page = "usernotes" } in
      update_wiki_page page ~retry_manager ~f:transform_page
  ;;
end

module Thread_cleanup_action_buffers = struct
  type t = Thing.Link.Id.Hash_set.t [@@deriving sexp_of]

  let create () = Thing.Link.Id.Hash_set.create ()
  let add t link = Hash_set.add t link

  let commit t ~retry_manager ~subreddit:_ ~(remaining_reports : Target.t list) =
    let%bind () =
      Deferred.List.iter remaining_reports ~f:(fun target ->
          match target with
          | Link _ -> return ()
          | Comment comment ->
            (match Hash_set.mem t (Thing.Comment.link comment) with
            | false -> return ()
            | true -> remove target ~retry_manager))
    in
    Hash_set.clear t;
    return ()
  ;;
end

module Action_buffers = struct
  type t =
    { automod : Automod_action_buffers.t
    ; thread_cleanup : Thread_cleanup_action_buffers.t
    ; usernote : Usernote_action_buffers.t
    }
  [@@deriving sexp_of, fields]

  let create () =
    { automod = Automod_action_buffers.create ()
    ; thread_cleanup = Thread_cleanup_action_buffers.create ()
    ; usernote = Usernote_action_buffers.create ()
    }
  ;;

  let commit_all
      { automod; thread_cleanup; usernote }
      ~retry_manager
      ~subreddit
      ~remaining_reports
    =
    Deferred.all_unit
      [ Automod_action_buffers.commit_all automod ~retry_manager ~subreddit
      ; Thread_cleanup_action_buffers.commit
          thread_cleanup
          ~retry_manager
          ~subreddit
          ~remaining_reports
      ; Usernote_action_buffers.commit usernote ~retry_manager ~subreddit
      ]
  ;;
end

let act
    t
    ~target
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
    ban target ~retry_manager ~subreddit ~message ~reason ~duration
  | Cleanup_thread ->
    (match target with
    | Comment _ -> ()
    | Link link ->
      Thread_cleanup_action_buffers.add action_buffers.thread_cleanup (Thing.Link.id link));
    return ()
  | Lock -> lock (Target.fullname target) ~retry_manager
  | Nuke -> nuke target ~retry_manager
  | Modmail { subject; body } -> modmail target ~retry_manager ~subject ~body ~subreddit
  | Notify { text } -> notify target ~retry_manager ~text
  | Remove -> remove target ~retry_manager
  | Set_flair { template } ->
    (match target with
    | Comment _ -> return ()
    | Link link -> set_flair (Thing.Link.id link) ~retry_manager ~template ~subreddit)
  | Watch_via_automod { key; placeholder } ->
    let buffers = action_buffers.automod in
    enqueue_automod_action target ~key ~placeholder ~buffers;
    return ()
;;

let will_remove t =
  match t with
  | Remove | Nuke -> true
  | Add_usernote _
  | Ban _
  | Cleanup_thread
  | Lock
  | Modmail _
  | Notify _
  | Set_flair _
  | Watch_via_automod _ -> false
;;

let validate =
  let validate_max_length name length string =
    Validate.name
      name
      (Validate.name
         "length"
         (Int.validate_ubound (String.length string) ~max:(Incl length)))
  in
  function
  | Ban { message; reason; duration } ->
    Validate.name
      "Ban"
      (Validate.of_list
         [ Option.value_map
             message
             ~f:(validate_max_length "message" 1000)
             ~default:Validate.pass
         ; validate_max_length "reason" 100 reason
         ; Validate.name
             "duration"
             (match duration with
             | Permanent -> Validate.pass
             | Days n -> Int.validate_bound ~min:(Incl 1) ~max:(Incl 999) n)
         ])
  | Modmail { subject; body = _ } ->
    Validate.name "Modmail" (validate_max_length "subject" 100 subject)
  | Notify { text } -> Validate.name "Notify" (validate_max_length "text" 10_000 text)
  | Add_usernote _
  | Cleanup_thread
  | Lock
  | Nuke
  | Remove
  | Set_flair _
  | Watch_via_automod _ -> Validate.pass
;;

let required_scopes t =
  List.map
    ~f:Scope.of_string
    (match t with
    | Add_usernote _ | Watch_via_automod _ -> [ "wikiedit"; "wikiread" ]
    | Ban _ -> [ "modcontributors" ]
    | Modmail _ -> [ "modmail" ]
    | Cleanup_thread | Lock | Remove -> [ "modposts" ]
    | Nuke -> [ "modposts"; "read" ]
    | Notify _ -> [ "modposts"; "submit" ]
    | Set_flair _ -> [ "flair" ])
;;
