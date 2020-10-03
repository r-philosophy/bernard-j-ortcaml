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
    -> subreddit:Thing.Subreddit.Id.t
    -> time:Time_ns.t
    -> unit Deferred.t

  val update_subscriber_counts : t -> subreddits:Thing.Subreddit.t list -> unit Deferred.t

  val update_moderator_table
    :  t
    -> moderators:Username.t list
    -> subreddit:Thing.Subreddit.Id.t
    -> unit Deferred.t
end
