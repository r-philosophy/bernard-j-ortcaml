open! Core
open Reddit_api_kernel
module Usernote_page = Bernard_j_ortcutt.Usernote_page

let%expect_test "Roundtrip example from docs" =
  (* This example is from https://github.com/toolbox-team/reddit-moderator-toolbox-legacy/wiki/JSON:-usernotes. *)
  let sample =
    Jsonaf.of_string
      {|
{
    "ver":6,
    "constants": {
        "users": [
            "creesch", "TheEnigmaBlade"
        ],
        "warnings": [
            "none"
        ]
    },
    "blob":"eJyrVkouSk0tTs5QsqpWyitWsooGUkpWSiEZmcUKQJSokJdfkqqko1SiZGVoYmxpZGhuZmmqo5SrZGWgo5QDVJmjY2SQZp6ZA1RTDhSsja2tBQA4HBgB"
}
        |}
  in
  let page = [%of_jsonaf: Usernote_page.t] sample in
  print_s [%sexp (page : Usernote_page.t)];
  [%expect
    {|
    ((moderators ((creesch) (TheEnigmaBlade))) (warnings ((none)))
     (notes
      ((creesch
        ((Object
          ((n (String "This is a note")) (t (Number 1439217695)) (m (Number 0))
           (l (String l,20f7il)) (w (Number 0))))))))) |}];
  print_s [%sexp ([%jsonaf_of: Usernote_page.t] page : Jsonaf.t)];
  [%expect
    {|
    (Object
     ((ver (Number 6))
      (constants
       (Object
        ((users (Array ((String creesch) (String TheEnigmaBlade))))
         (warnings (Array ((String none)))))))
      (blob
       (String
        eJyrVkouSk0tTs5QsqpWyitWsooGUkpWSiEZmcUKQJSokJdfkqqko1SiZGVoYmxpZGhuZmmqo5SrZGWgo5QDVJmjY2SQZp6ZA1RTDhSsja2tBQA4HBgB)))) |}]
;;

let%expect_test "Roundtrip example from testing subreddit" =
  (* This example is from usernotes constructed via the toolbox extension. *)
  let sample =
    Jsonaf.of_string
      {|
{"ver":6,"constants":{"users":["L72_Elite_Kraken"],"warnings":["gooduser"]},"blob":"eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsooGUkpWSiEZmcUKQJSoUFqcWpSXX5KqpKNUomRlaGZgZGhpZmpuoaOUq2RloKOUA1Sdo5OZk2lSlgxUUw4UrI2trQUA1w4dGg=="}
|}
  in
  let page = [%of_jsonaf: Usernote_page.t] sample in
  print_s [%sexp (page : Usernote_page.t)];
  [%expect
    {|
    ((moderators ((L72_Elite_Kraken))) (warnings ((gooduser)))
     (notes
      ((L72_Elite_Kraken
        ((Object
          ((n (String "This is a usernote")) (t (Number 1602196578))
           (m (Number 0)) (l (String l,ili4vc)) (w (Number 0))))))))) |}];
  print_s [%sexp ([%jsonaf_of: Usernote_page.t] page : Jsonaf.t)];
  [%expect
    {|
    (Object
     ((ver (Number 6))
      (constants
       (Object
        ((users (Array ((String L72_Elite_Kraken))))
         (warnings (Array ((String gooduser)))))))
      (blob
       (String
        eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsooGUkpWSiEZmcUKQJSoUFqcWpSXX5KqpKNUomRlaGZgZGhpZmpuoaOUq2RloKOUA1Sdo5OZk2lSlgxUUw4UrI2trQUA1w4dGg==)))) |}]
;;

let%expect_test "Example adding a usernote with new kind" =
  (* This example shows a modification of the usernote page, including a new note kind. *)
  let sample =
    Jsonaf.of_string
      {|
{"ver":6,"constants":{"users":["L72_Elite_Kraken"],"warnings":["gooduser"]},"blob":"eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsooGUkpWSiEZmcUKQJSoUFqcWpSXX5KqpKNUomRlaGZgZGhpZmpuoaOUq2RloKOUA1Sdo5OZk2lSlgxUUw4UrI2trQUA1w4dGg=="}
|}
  in
  let page = [%of_jsonaf: Usernote_page.t] sample in
  Usernote_page.add_note
    page
    ~username:(Username.of_string "L72_Elite_Kraken")
    ~spec:
      { text = "This is a new usernote"
      ; context = Some (Link (Thing.Link.Id.of_string "ili4vc"))
      ; time = Time_ns.epoch
      ; moderator = Username.of_string "spez"
      ; warning = "a_note"
      };
  let s = [%jsonaf_of: Usernote_page.t] page |> Jsonaf.to_string in
  printf "%s\n" s;
  (* We verified that this blob shows the new note when loaded on Reddit. *)
  [%expect
    {|
    {"ver":6,"constants":{"users":["L72_Elite_Kraken","spez"],"warnings":["gooduser","a_note"]},"blob":"eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsoquVspRslLK0cnMyTQpS1bSUQJKKYVkZBYrAFGiQl5quUJpcWpRXn5JKlCyRMnKQEcpV8nKUEepHEjW6lSjaUBVbGhmYGRoaWZqbgHWBdSLZhvQEIPa2NpaAKeiL6c="} |}];
  let page = Jsonaf.of_string s |> [%of_jsonaf: Usernote_page.t] in
  print_s [%sexp (page : Usernote_page.t)];
  [%expect
    {|
    ((moderators ((L72_Elite_Kraken) (spez))) (warnings ((gooduser) (a_note)))
     (notes
      ((L72_Elite_Kraken
        ((Object
          ((l (String l,ili4vc)) (n (String "This is a new usernote"))
           (t (Number 0)) (m (Number 1)) (w (Number 1))))
         (Object
          ((n (String "This is a usernote")) (t (Number 1602196578))
           (m (Number 0)) (l (String l,ili4vc)) (w (Number 0))))))))) |}]
;;
