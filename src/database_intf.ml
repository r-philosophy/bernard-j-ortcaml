open! Core
open! Async
open! Import

module type Database = sig
  type t

  val create : database:string -> t Deferred.t

  val already_acted
    :  t
    -> target:Action.Target.t
    -> moderator:Username.t
    -> bool Deferred.t

  val log_rule_application
    :  t
    -> target:Action.Target.t
    -> action_summary:string
    -> author:Username.t
    -> moderator:Username.t
    -> subreddit_id:Thing.Subreddit.Id.t
    -> time:Time_ns.t
    -> unit Deferred.t
end
