open! Core
open! Async
open Reddit_api_async

val retry_or_fail : Retry_manager.t -> Lexing.position -> 'a Endpoint.t -> 'a Deferred.t

val update_wiki_page
  :  ?reason:string
  -> Wiki_page.Id.t
  -> retry_manager:Retry_manager.t
  -> f:(string -> string)
  -> unit Deferred.t

val add_user_notes
  :  (Username.t * Usernote_page.Note.Spec.t) list
  -> retry_manager:Retry_manager.t
  -> subreddit:Subreddit_name.t
  -> unit Deferred.t

val watch_via_automod
  :  Retry_manager.t
  -> subreddit:Subreddit_name.t
  -> placeholder:string
  -> entries:string Nonempty_list.t
  -> unit Deferred.t
