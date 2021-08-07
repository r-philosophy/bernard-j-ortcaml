open! Core
open! Async
include Reddit_api_async

let retry_or_fail retry_manager here endpoint =
  match%bind Retry_manager.call retry_manager endpoint with
  | Ok v -> return v
  | Error error ->
    raise_s
      [%message
        "Reddit returned error"
          (here : Source_code_position.t)
          ~request:(endpoint.request : Api.Request.t)
          (error : Retry_manager.Non_transient_error.t)]
;;
