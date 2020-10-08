open! Core
open Reddit_api

module Note : sig
  module Context : sig
    type t =
      | Link of Thing.Link.Id.t
      | Comment of Thing.Link.Id.t * Thing.Comment.Id.t
    [@@deriving sexp_of]
  end

  module Spec : sig
    type t =
      { text : string
      ; context : Context.t
      ; time : Time_ns.t
      ; moderator : Username.t
      ; warning : string
      }
    [@@deriving sexp_of]
  end
end

type t [@@deriving sexp_of]

include Json_object.S with type t := t

val add_note : t -> username:Username.t -> spec:Note.Spec.t -> unit
