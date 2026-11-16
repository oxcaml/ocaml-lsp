open Async
open Test.Import

let print_complete_prefix_at_pos
  (source : string)
  (position : Position.t)
  (prefix : string)
  =
  let makeRequest (textDocument : TextDocumentIdentifier.t) =
    Lsp.Client_request.UnknownRequest
      { meth = Lsp.Client_request.Custom_request_names.complete_prefix_at_pos
      ; params =
          Some
            (`Assoc
              [ "textDocument", TextDocumentIdentifier.yojson_of_t textDocument
              ; "position", Position.yojson_of_t position
              ; "prefix", `String prefix
              ])
      }
  in
  Lsp_helpers.iter_lsp_response ~makeRequest ~source (fun x ->
    print_endline (Yojson.Safe.pretty_to_string x))
;;

let%expect_test "Serialization works as expected" =
  let module C = Ocaml_lsp_server.Testing.Compl.Complete_by_prefix in
  let serialize =
    Ocaml_lsp_server.Custom_request.Complete_prefix_at_pos.For_testing.serialize
  in
  let print application_labels =
    let entries =
      [ CompletionItem.t_of_yojson
          (`Assoc [ "detail", `String "'a option -> bool"; "label", `String "is_none" ])
      ]
    in
    serialize { C.entries; application_labels }
    |> Yojson.Safe.pretty_to_string
    |> print_endline
  in
  let labels =
    [ { C.Function_label.name = "~key"; type_ = "'_weak1" }
    ; { name = "~data"; type_ = "'_weak1" }
    ]
  in
  print None;
  [%expect {| { "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ] } |}];
  print (Some []);
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": [ "application", { "labels": [] } ]
    }
    |}];
  print (Some labels);
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": [
        "application",
        {
          "labels": [
            { "name": "~key", "type": "'_weak1" },
            { "name": "~data", "type": "'_weak1" }
          ]
        }
      ]
    }
    |}];
  return ()
;;

let%expect_test "Can complete with prefix in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if Option.is_ (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:18 in
  let%map () = print_complete_prefix_at_pos source position "Option.is_" in
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "start": { "line": 3, "character": 15 },
              "end": { "line": 3, "character": 18 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "start": { "line": 3, "character": 15 },
              "end": { "line": 3, "character": 18 }
            }
          }
        }
      ]
    }
    |}]
;;

let%expect_test "Can complete with prefix not in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if  (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:8 in
  let%map () = print_complete_prefix_at_pos source position "Option.is_" in
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "start": { "line": 3, "character": 8 },
              "end": { "line": 3, "character": 8 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "start": { "line": 3, "character": 8 },
              "end": { "line": 3, "character": 8 }
            }
          }
        }
      ]
    }
    |}]
;;

let%expect_test "Can complete with different prefix in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if Or_error.is_some (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:24 in
  let%map () = print_complete_prefix_at_pos source position "Option.is_" in
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "start": { "line": 3, "character": 17 },
              "end": { "line": 3, "character": 24 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "start": { "line": 3, "character": 17 },
              "end": { "line": 3, "character": 24 }
            }
          }
        }
      ]
    }
    |}]
;;

let%expect_test "Completion can return function application context" =
  let source =
    {ocaml|
open MoreLabels
let (tbl : (string, int) Hashtbl.t) = Hashtbl.create 1
let () = Hashtbl.add tbl ~key
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:29 in
  let%map () = print_complete_prefix_at_pos source position "~key" in
  [%expect
    {|
    {
      "entries": [],
      "context": [
        "application",
        {
          "labels": [
            { "name": "~key", "type": "string" },
            { "name": "~data", "type": "int" }
          ]
        }
      ]
    }
    |}]
;;
