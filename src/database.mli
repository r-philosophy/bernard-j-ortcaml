open! Core
open! Async
open! Import

type t = (Caqti_async.connection, Caqti_error.t) Caqti_async.Pool.t

val already_acted : t -> target:Action.Target.t -> moderator:Username.t -> bool Deferred.t

val record_contents
  :  t
  -> target:Action.Target.t
  -> [ `Ok | `Already_recorded ] Deferred.t

val log_rule_application
  :  t
  -> target:Action.Target.t
  -> action_summary:string
  -> author:Username.t option
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

module Types : sig
  val custom_type
    :  'a Caqti_type.t
    -> encode:('b -> ('a, Error.t) result)
    -> decode:('a -> ('b, Error.t) result)
    -> 'b Caqti_type.t

  val fullname
    : [ `Comment of Thing.Comment.Id.t | `Link of Thing.Link.Id.t ] Caqti_type.t

  val subreddit_name : Subreddit_name.t Caqti_type.t
  val subreddit_id : Thing.Subreddit.Id.t Caqti_type.t
  val thing : Action.Target.t Caqti_type.t
  val time : Time_ns.t Caqti_type.t
  val username : Username.t Caqti_type.t
end
