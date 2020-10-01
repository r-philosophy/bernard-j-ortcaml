open! Core
open Reddit_api

module type Database = sig
  type t

  val create : unit -> t
  val already_acted : Action.Target.t -> bool

  val log_rule_application
    :  t
    -> target:Action.Target.t
    -> rule_description:string
    -> moderator:Username.t
    -> time:Time_ns.t
    -> unit
end
