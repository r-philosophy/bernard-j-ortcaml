open! Core
open! Import
open Jsonaf.Export

module Index_list (Param : sig
  type t [@@deriving sexp_of]

  include Comparable with type t := t
  include Stringable with type t := t
end) =
struct
  type t = Param.t option Queue.t [@@deriving sexp_of]

  let t_of_jsonaf json =
    let element_of_jsonaf json =
      match json with
      | `Null -> None
      | `String s -> Some (Param.of_string s)
      | _ -> raise_s [%message "Unexpected usernote constant" (json : Jsonaf.t)]
    in
    [%of_jsonaf: element list] json |> Queue.of_list
  ;;

  let jsonaf_of_t t =
    `Array
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

  type t = Jsonaf.t [@@deriving sexp_of]

  let create ({ text; context; time; moderator; warning } : Spec.t) ~moderators ~warnings =
    let moderator_index = Moderators.index moderators moderator in
    let warning_index = Warnings.index warnings warning in
    `Object
      [ "n", `String text
      ; "l", `String (Context.to_string context)
      ; ( "t"
        , [%jsonaf_of: int] (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_sec)
        )
      ; "m", [%jsonaf_of: int] moderator_index
      ; "w", [%jsonaf_of: int] warning_index
      ]
  ;;
end

type t =
  { moderators : Moderators.t
  ; warnings : Warnings.t
  ; notes : Note.t Deque.t Hashtbl.M(Username).t
  }
[@@deriving sexp_of]

let decompress_blob blob =
  match Base64.decode_exn blob |> Ezgzip.Z.decompress ~header:true with
  | Error error ->
    let error = Format.asprintf "%a" Ezgzip.Z.pp_zlib_error error in
    raise_s [%message "Error decompressing blob" (error : string)]
  | Ok s ->
    (match Jsonaf.parse s with
    | Ok json -> json
    | Error error ->
      raise_s [%message "Error converting blob to string" (error : Error.t)])
;;

let t_of_jsonaf json =
  let () =
    match Jsonaf.member_exn "ver" json |> Jsonaf.int_exn with
    | 6 -> ()
    | version -> raise_s [%message "Unexpected usernotes version" (version : int)]
  in
  let rec member_nested fields json =
    match fields with
    | [] -> json
    | field :: rest -> member_nested rest (Jsonaf.member_exn field json)
  in
  let moderators =
    member_nested [ "constants"; "users" ] json |> [%of_jsonaf: Moderators.t]
  in
  let warnings =
    member_nested [ "constants"; "warnings" ] json |> [%of_jsonaf: Warnings.t]
  in
  let notes =
    let blob = decompress_blob (Jsonaf.member_exn "blob" json |> Jsonaf.string_exn) in
    Jsonaf.assoc_list_exn blob
    |> List.map ~f:(fun (username, json) ->
           ( Username.of_string username
           , Jsonaf.member_exn "ns" json
             |> Jsonaf.list_exn
             |> List.to_array
             |> Deque.of_array ))
    |> Hashtbl.of_alist_exn (module Username)
  in
  { moderators; warnings; notes }
;;

let compress_blob json =
  Jsonaf.to_string json |> Ezgzip.Z.compress ~header:true |> Base64.encode_exn
;;

let jsonaf_of_t { moderators; warnings; notes } =
  let notes =
    `Object
      (Hashtbl.to_alist notes
      |> List.map ~f:(fun (username, notes) ->
             Username.to_string username, `Object [ "ns", `Array (Deque.to_list notes) ])
      )
  in
  `Object
    [ "ver", [%jsonaf_of: int] 6
    ; ( "constants"
      , `Object
          [ "users", [%jsonaf_of: Moderators.t] moderators
          ; "warnings", [%jsonaf_of: Warnings.t] warnings
          ] )
    ; "blob", `String (compress_blob notes)
    ]
;;

let add_note { moderators; warnings; notes } ~username ~spec =
  let note = Note.create spec ~moderators ~warnings in
  let deque = Hashtbl.find_or_add notes username ~default:Deque.create in
  Deque.enqueue_front deque note
;;
