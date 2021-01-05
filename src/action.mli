open! Core
open! Async
open! Import

module Target : sig
  module Kind : sig
    type t =
      | Link
      | Comment
    [@@deriving sexp]

    include Comparable.S with type t := t
  end

  type t =
    | Link of Thing.Link.t
    | Comment of Thing.Comment.t
  [@@deriving sexp_of]

  val of_thing : [< `Link of Thing.Link.t | `Comment of Thing.Comment.t ] -> t
  val kind : t -> Kind.t
  val fullname : t -> [> `Link of Thing.Link.Id.t | `Comment of Thing.Comment.Id.t ]
  val author : t -> Username.t option
  val moderator_reports : t -> Moderator_report.t list
end

module Automod_key : sig
  type t =
    | Author
    | Domain
  [@@deriving sexp, compare]
end

module Action_buffers : sig
  type t [@@deriving sexp_of]

  val create : unit -> t

  val commit_all
    :  t
    -> retry_manager:Retry_manager.t
    -> subreddit:Subreddit_name.t
    -> unit Deferred.t
end

type t =
  | Add_usernote of
      { level : string
      ; text : string
      }
      (** Add a {{:https://www.reddit.com/r/toolbox/wiki/docs/usernotes}Reddit
          Toolbox usernote} to the target author with the corresponding level
          and text. *)
  | Ban of
      { message : string (** Message displayed to the user *)
      ; reason : string (** Reason available to moderators *)
      ; duration : Api.Parameters.Relationship_spec.Duration.t
      } (** Ban the target author. *)
  | Lock (** Lock the target. *)
  | Nuke (** Recursively remove the target and its replies. *)
  | Modmail of
      { subject : string
      ; body : string
      } (** Send a modmail to the target author. *)
  | Notify of { text : string }
      (** Reply to the target and distinguish the resulting comment. *)
  | Remove (** Remove the target. *)
  | Watch_via_automod of
      { key : Automod_key.t
      ; placeholder : string
      }
      (** Add the [key] of the target to the subreddit's AutoMod config.

          This action finds [placeholder] in the AutoMod config and adds the key
          after it, expecting that the placeholder is found in a YAML list.

          For example, if our key is "spez" and our placeholder is "do!not!remove",
          this action assumes a list like the following exists in the AutoMod config:

          {[ [do!not!remove, ketralnis, kn0thing] ]}

          Afterwards, the list will look like this:

          {[ [do!not!remove, spez, ketralnis, kn0thing] ]}

          The change is a simple textual replacement and does not try to parse
          markdown.

          We recommend that placeholders contain a character that is legal
          neither in usernames nor in domains, such as '!'.
      *)
[@@deriving sexp, compare, equal]

val validate : t Validate.check

val act
  :  t
  -> target:Target.t
  -> retry_manager:Retry_manager.t
  -> subreddit:Subreddit_name.t
  -> moderator:Username.t
  -> time:Time_ns.t
  -> action_buffers:Action_buffers.t
  -> unit Deferred.t

val will_remove : t -> bool
