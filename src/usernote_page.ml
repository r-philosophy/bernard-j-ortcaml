open! Core
open! Import

module Index_list (Param : sig
  type t [@@deriving sexp_of]

  include Comparable with type t := t
  include Stringable with type t := t
end) =
struct
  type t = Param.t option Queue.t [@@deriving sexp_of]

  let of_json json =
    let get_element json =
      match json with
      | `Null -> None
      | `String s -> Some (Param.of_string s)
      | _ -> raise_s [%message "Unexpected usernote constant" (json : Json.t)]
    in
    Json.get_list get_element json |> Queue.of_list
  ;;

  let to_json t =
    `A
      (Queue.to_list t
      |> List.map ~f:(function
             | None -> `Null
             | Some v -> `String (Param.to_string v)))
  ;;

  let index t element =
    match Queue.findi t ~f:(fun _ -> [%equal: Param.t option] (Some element)) with
    | Some (i, _) -> i
    | None ->
      let result = Queue.length t in
      Queue.enqueue t (Some element);
      result
  ;;
end

module Moderators = Index_list (Username)
module Warnings = Index_list (String)

module Note = struct
  module Context = struct
    type t =
      | Link of Thing.Link.Id.t
      | Comment of Thing.Link.Id.t * Thing.Comment.Id.t
    [@@deriving sexp_of]

    let to_string t =
      match t with
      | Link id -> sprintf !"l,%{Thing.Link.Id}" id
      | Comment (link, comment) ->
        sprintf !"l,%{Thing.Link.Id},%{Thing.Comment.Id}" link comment
    ;;
  end

  module Spec = struct
    type t =
      { text : string
      ; context : Context.t
      ; time : Time_ns.t
      ; moderator : Username.t
      ; warning : string
      }
    [@@deriving sexp_of]
  end

  type t = Json.t [@@deriving sexp_of]

  let create ({ text; context; time; moderator; warning } : Spec.t) ~moderators ~warnings =
    let moderator_index = Moderators.index moderators moderator in
    let warning_index = Warnings.index warnings warning in
    `O
      [ "n", `String text
      ; "l", `String (Context.to_string context)
      ; "t", Json.int (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_sec)
      ; "m", Json.int moderator_index
      ; "w", Json.int warning_index
      ]
  ;;
end

type t =
  { moderators : Moderators.t
  ; warnings : Warnings.t
  ; notes : Note.t Deque.t Username.Table.t
  }
[@@deriving sexp_of]

let decompress_blob blob =
  match Base64.decode_exn blob |> Ezgzip.Z.decompress ~header:true with
  | Ok s -> Json.of_string s
  | Error error ->
    let error = Format.asprintf "%a" Ezgzip.Z.pp_zlib_error error in
    raise_s [%message "Error decompressing blob" (error : string)]
;;

let of_json json =
  let () =
    match Json.find json [ "ver" ] with
    | `Float 6. -> ()
    | version -> raise_s [%message "Unexpected usernotes version" (version : Json.t)]
  in
  let moderators = Json.find json [ "constants"; "users" ] |> Moderators.of_json in
  let warnings = Json.find json [ "constants"; "warnings" ] |> Warnings.of_json in
  let notes =
    let blob = decompress_blob (Json.find json [ "blob" ] |> Json.get_string) in
    Json.get_dict blob
    |> List.map ~f:(fun (username, json) ->
           ( Username.of_string username
           , Json.find json [ "ns" ]
             |> Json.get_list ident
             |> List.to_array
             |> Deque.of_array ))
    |> Username.Table.of_alist_exn
  in
  { moderators; warnings; notes }
;;

let compress_blob json =
  Json.to_string json |> Ezgzip.Z.compress ~header:true |> Base64.encode_exn
;;

let to_json { moderators; warnings; notes } =
  let notes =
    `O
      (Hashtbl.to_alist notes
      |> List.map ~f:(fun (username, notes) ->
             Username.to_string username, `O [ "ns", `A (Deque.to_list notes) ]))
  in
  `O
    [ "ver", `Float 6.
    ; ( "constants"
      , `O
          [ "users", Moderators.to_json moderators
          ; "warnings", Warnings.to_json warnings
          ] )
    ; "blob", `String (compress_blob notes)
    ]
;;

let add_note { moderators; warnings; notes } ~username ~spec =
  let note = Note.create spec ~moderators ~warnings in
  let deque = Hashtbl.find_or_add notes username ~default:Deque.create in
  Deque.enqueue_front deque note
;;
