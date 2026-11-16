open Test.Import
open Async

let%expect_test "TextDocumentRename works when document URI has a query string" =
  let source =
    {ocaml|
let x = 1
let y = x + 1
|ocaml}
  in
  let%bind () =
    Lsp_helpers.iter_lsp_response
      ~uri:(DocumentUri.of_string "file:///foo.ml?review-annotation")
      ~makeRequest:(fun textDocument ->
        TextDocumentRename
          (RenameParams.create
             ~textDocument
             ~position:(Position.create ~line:1 ~character:4)
             ~newName:"z"
             ()))
      ~source
      (function
        | None -> print_endline "No rename response"
        | Some workspace_edit ->
          WorkspaceEdit.yojson_of_t workspace_edit
          |> Yojson.Safe.pretty_to_string
          |> print_endline)
  in
  [%expect
    {|
    {
      "changes": {
        "file:///foo.ml?review-annotation": [
          {
            "newText": "z",
            "range": {
              "start": { "line": 2, "character": 8 },
              "end": { "line": 2, "character": 9 }
            }
          },
          {
            "newText": "z",
            "range": {
              "start": { "line": 1, "character": 4 },
              "end": { "line": 1, "character": 5 }
            }
          }
        ]
      }
    }
    |}];
  Deferred.unit
;;

let%expect_test "TextDocumentRename works when document URI does not have a query string" =
  let source =
    {ocaml|
let x = 1
let y = x + 1
|ocaml}
  in
  let%bind () =
    Lsp_helpers.iter_lsp_response
      ~uri:(DocumentUri.of_string "file:///foo.ml")
      ~makeRequest:(fun textDocument ->
        TextDocumentRename
          (RenameParams.create
             ~textDocument
             ~position:(Position.create ~line:1 ~character:4)
             ~newName:"z"
             ()))
      ~source
      (function
        | None -> print_endline "No rename response"
        | Some workspace_edit ->
          WorkspaceEdit.yojson_of_t workspace_edit
          |> Yojson.Safe.pretty_to_string
          |> print_endline)
  in
  [%expect
    {|
    {
      "changes": {
        "file:///foo.ml": [
          {
            "newText": "z",
            "range": {
              "start": { "line": 2, "character": 8 },
              "end": { "line": 2, "character": 9 }
            }
          },
          {
            "newText": "z",
            "range": {
              "start": { "line": 1, "character": 4 },
              "end": { "line": 1, "character": 5 }
            }
          }
        ]
      }
    }
    |}];
  Deferred.unit
;;
