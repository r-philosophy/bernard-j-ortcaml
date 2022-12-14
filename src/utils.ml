open! Core
open! Async
open Reddit_api_async

let retry_or_fail retry_manager here endpoint =
  Deferred.repeat_until_finished () (fun () ->
      match%bind Retry_manager.call retry_manager endpoint with
      | Ok v -> return (`Finished v)
      | Error
          (Endpoint_error
            (Json_response_errors
              [ { error = "RATELIMIT"
                ; fields = [ "ratelimit" ]
                ; details =
                    "Looks like you've been doing that a lot. Take a break for 5 seconds \
                     before trying again."
                ; error_type = _
                }
              ])) ->
        let%bind () = Clock_ns.after (Time_ns.Span.of_int_sec 6) in
        return (`Repeat ())
      | Error error ->
        raise_s
          [%message
            "Reddit returned error"
              (here : Source_code_position.t)
              ~request:(endpoint.request : Endpoint.Request.t)
              (error : Retry_manager.Permanent_error.t)])
;;

let update_wiki_page ?reason page ~retry_manager ~f =
  let%bind wiki_page =
    retry_or_fail retry_manager [%here] (Endpoint.wiki_page ~page ())
  in
  let content = Wiki_page.content wiki_page `markdown in
  let revision_id = Wiki_page.revision_id wiki_page in
  let rec loop content ~previous =
    let new_content = f content in
    match%bind
      retry_or_fail
        retry_manager
        [%here]
        (Endpoint.edit_wiki_page ~previous ~content:new_content ?reason ~page ())
    with
    | Ok () -> return ()
    | Error conflict ->
      loop
        (Wiki_page.Edit_conflict.new_content conflict)
        ~previous:(Wiki_page.Edit_conflict.new_revision conflict)
  in
  let%bind () = loop content ~previous:revision_id in
  Clock_ns.after Time_ns.Span.second
;;

let add_user_notes notes ~retry_manager ~subreddit =
  match List.is_empty notes with
  | true -> return ()
  | false ->
    let transform_page page =
      match Jsonaf.parse page with
      | Error error ->
        raise_s
          [%message
            "Usernote page contained invalid json" (page : string) (error : Error.t)]
      | Ok json ->
        let page = [%of_jsonaf: Usernote_page.t] json in
        List.iter notes ~f:(fun (username, spec) ->
            Usernote_page.add_note page ~username ~spec);
        [%jsonaf_of: Usernote_page.t] page |> Jsonaf.to_string
    in
    let page : Wiki_page.Id.t = { subreddit = Some subreddit; page = "usernotes" } in
    update_wiki_page page ~retry_manager ~f:transform_page
;;
