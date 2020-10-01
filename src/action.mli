open! Core
open! Async
open Reddit_api

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
end

module Automod_key : sig
  type t =
    | Author
    | Domain
  [@@deriving sexp]
end

module Action_buffers : sig
  type t [@@deriving sexp_of]

  val create : unit -> t

  val commit_all
    :  t
    -> connection:Connection.t
    -> subreddit:Subreddit_name.t
    -> unit Deferred.t
end

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
  | Watch_via_automod of
      { key : Automod_key.t
      ; placeholder : string
      }
[@@deriving sexp]

val act
  :  t
  -> target:Target.t
  -> connection:Connection.t
  -> subreddit:Subreddit_name.t
  -> action_buffers:Action_buffers.t
  -> unit Deferred.t
