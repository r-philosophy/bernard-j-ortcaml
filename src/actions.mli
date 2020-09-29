open! Core
open! Async
open Reddit_api

module Target : sig
  type t =
    | Link of Thing.Link.t
    | Comment of Thing.Comment.t
  [@@deriving sexp_of]
end

module type S = sig
  module Config : Sexpable

  type t [@@deriving sexp_of]

  val tag : string
  val create : Config.t -> t
  val action : t -> Connection.t -> Target.t -> moderator:Username.t -> unit Deferred.t
  val after : (t -> Connection.t -> unit Deferred.t) option
end

module Config : sig
  type t [@@deriving sexp]
end

type t = T : (module S with type t = 't) * 't -> t

val create : Config.t -> t
