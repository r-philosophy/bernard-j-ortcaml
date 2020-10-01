open! Core
open! Async
include Reddit_api

let retry_manager = failwith "asdf"

let retry_or_fail retry_manager here ~f =
  match%bind Retry_manager.call retry_manager f with
  | Ok v -> return v
  | Error (response, body) ->
    let%bind body = Cohttp_async.Body.to_string body in
    raise_s
      [%message
        "Reddit returned error"
          (here : Source_code_position.t)
          (response : Cohttp.Response.t)
          (body : string)]
;;
