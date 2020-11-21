open! Core
open Reddit_api_kernel
module Usernote_page = Bernard_j_ortcutt.Usernote_page

let%expect_test "Roundtrip example from docs" =
  (* This example is from https://github.com/toolbox-team/reddit-moderator-toolbox-legacy/wiki/JSON:-usernotes. *)
  let sample =
    Json.of_string
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
  let page = Usernote_page.of_json sample in
  print_s [%sexp (page : Usernote_page.t)];
  [%expect
    {|
    ((moderators ((creesch) (TheEnigmaBlade))) (warnings ((none)))
     (notes
      ((creesch
        ((O
          ((n (String "This is a note")) (t (Float 1439217695)) (m (Float 0))
           (l (String l,20f7il)) (w (Float 0))))))))) |}];
  print_s [%sexp (Usernote_page.to_json page : Json.t)];
  [%expect
    {|
    (O
     ((ver (Float 6))
      (constants
       (O
        ((users (A ((String creesch) (String TheEnigmaBlade))))
         (warnings (A ((String none)))))))
      (blob
       (String
        eJyrVkouSk0tTs5QsqpWyitWsooGUkpWSiEZmcUKQJSokJdfkqqko1SiZGVoYmxpZGhuZmmqo5SrZGWgo5QDVJmjY2SQZp6ZA1RTDhSsja2tBQA4HBgB)))) |}]
;;

let%expect_test "Roundtrip example from testing subreddit" =
  (* This example is from usernotes constructed via the toolbox extension. *)
  let sample =
    Json.of_string
      {|
{"ver":6,"constants":{"users":["L72_Elite_Kraken"],"warnings":["gooduser"]},"blob":"eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsooGUkpWSiEZmcUKQJSoUFqcWpSXX5KqpKNUomRlaGZgZGhpZmpuoaOUq2RloKOUA1Sdo5OZk2lSlgxUUw4UrI2trQUA1w4dGg=="}
|}
  in
  let page = Usernote_page.of_json sample in
  print_s [%sexp (page : Usernote_page.t)];
  [%expect
    {|
    ((moderators ((L72_Elite_Kraken))) (warnings ((gooduser)))
     (notes
      ((L72_Elite_Kraken
        ((O
          ((n (String "This is a usernote")) (t (Float 1602196578)) (m (Float 0))
           (l (String l,ili4vc)) (w (Float 0))))))))) |}];
  print_s [%sexp (Usernote_page.to_json page : Json.t)];
  [%expect
    {|
    (O
     ((ver (Float 6))
      (constants
       (O
        ((users (A ((String L72_Elite_Kraken))))
         (warnings (A ((String gooduser)))))))
      (blob
       (String
        eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsooGUkpWSiEZmcUKQJSoUFqcWpSXX5KqpKNUomRlaGZgZGhpZmpuoaOUq2RloKOUA1Sdo5OZk2lSlgxUUw4UrI2trQUA1w4dGg==)))) |}]
;;

let%expect_test "Example adding a usernote with new kind" =
  (* This example shows a modification of the usernote page, including a new note kind. *)
  let sample =
    Json.of_string
      {|
{"ver":6,"constants":{"users":["L72_Elite_Kraken"],"warnings":["gooduser"]},"blob":"eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsooGUkpWSiEZmcUKQJSoUFqcWpSXX5KqpKNUomRlaGZgZGhpZmpuoaOUq2RloKOUA1Sdo5OZk2lSlgxUUw4UrI2trQUA1w4dGg=="}
|}
  in
  let page = Usernote_page.of_json sample in
  Usernote_page.add_note
    page
    ~username:(Username.of_string "L72_Elite_Kraken")
    ~spec:
      { text = "This is a new usernote"
      ; context = Link (Thing.Link.Id.of_string "ili4vc")
      ; time = Time_ns.epoch
      ; moderator = Username.of_string "spez"
      ; warning = "a_note"
      };
  let s = Usernote_page.to_json page |> Json.value_to_string in
  printf "%s\n" s;
  (* We verified that this blob shows the new note when loaded on Reddit. *)
  [%expect
    {|
    {"ver":6,"constants":{"users":["L72_Elite_Kraken","spez"],"warnings":["gooduser","a_note"]},"blob":"eJyrVvIxN4p3zcksSY33LkrMTs1TsqpWyitWsooGUkpWSiEZmcUKQJSokJdarlBanFqUl1+SqqSjlAOUzNHJzMk0KUsGckuUrAx0lHKVrAx1lMqBZK0Oun4kvUDFhmYGRoaWZqbmFmBdBhgGAg0xqI2trQUAukkvpw=="} |}];
  let page = Usernote_page.of_json (Json.of_string s) in
  print_s [%sexp (page : Usernote_page.t)];
  [%expect
    {|
    ((moderators ((L72_Elite_Kraken) (spez))) (warnings ((gooduser) (a_note)))
     (notes
      ((L72_Elite_Kraken
        ((O
          ((n (String "This is a new usernote")) (l (String l,ili4vc))
           (t (Float 0)) (m (Float 1)) (w (Float 1))))
         (O
          ((n (String "This is a usernote")) (t (Float 1602196578)) (m (Float 0))
           (l (String l,ili4vc)) (w (Float 0))))))))) |}]
;;
