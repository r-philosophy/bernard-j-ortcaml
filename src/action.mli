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
    -> connection:Connection.t
    -> retry_manager:Retry_manager.t
    -> subreddit:Subreddit_name.t
    -> unit Deferred.t
end

type t =
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

val act
  :  t
  -> target:Target.t
  -> connection:Connection.t
  -> retry_manager:Retry_manager.t
  -> subreddit:Subreddit_name.t
  -> action_buffers:Action_buffers.t
  -> unit Deferred.t
