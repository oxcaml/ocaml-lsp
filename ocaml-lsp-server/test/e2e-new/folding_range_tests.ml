open Async
open Test.Import

(* These tests exist to answer the question "when the editor folds the wrong region, is
   ocaml-lsp at fault?". The server only reports candidate ranges; the client decides
   which one to attach to a given gutter arrow. So the useful thing to print is every
   range the server offers, laid out against the source, with the ranges that share a
   start line called out -- those are precisely the cases where the client has to pick,
   and where a bad range on the server side shows up as a surprising fold. *)

let request_folding_ranges ?(path = "foo.ml") ~source () =
  let uri = DocumentUri.of_path path in
  let textDocument = TextDocumentIdentifier.create ~uri in
  Test.run_request
    ~prep:(fun client -> Test.openDocument ~client ~uri ~source)
    (TextDocumentFoldingRange (FoldingRangeParams.create ~textDocument ()))
;;

(* Sort so that outer ranges come before the ranges they contain: earlier start line
   first, and among ranges sharing a start line, the one that extends furthest first. *)
let compare_ranges =
  Core.Comparable.lexicographic
    [ Core.Comparable.lift Int.compare ~f:(fun (range : FoldingRange.t) ->
        range.startLine)
    ; Core.Comparable.reverse
        (Core.Comparable.lift Int.compare ~f:(fun (range : FoldingRange.t) ->
           range.endLine))
    ; Core.Comparable.lift
        (Option.compare Int.compare)
        ~f:(fun (range : FoldingRange.t) -> range.startCharacter)
    ]
;;

let sort_ranges ranges = List.sort ranges ~compare:compare_ranges

(* Render one column per range, next to the source, so the extent of each range is visible
   at a glance:

   {v
     /  range starts on this line
     |  range covers this line
     \  range ends on this line
   v} *)
let render_diagram lines ranges =
  let n_lines = Array.length lines in
  let column (range : FoldingRange.t) =
    Array.init n_lines ~f:(fun line ->
      if line = range.startLine
      then '/'
      else if line = range.endLine
      then '\\'
      else if line > range.startLine && line < range.endLine
      then '|'
      else ' ')
  in
  let columns = List.map ranges ~f:column in
  Array.iteri lines ~f:(fun line text ->
    let gutter = List.map columns ~f:(fun col -> String.make 1 col.(line)) in
    print_endline
      (String.concat ~sep:"" gutter ^ Printf.sprintf " %2d | " line ^ text
       |> String.rstrip))
;;

let render_range_list lines ranges =
  let line_text line =
    if line >= 0 && line < Array.length lines then String.strip lines.(line) else "<eof>"
  in
  List.iter ranges ~f:(fun (range : FoldingRange.t) ->
    let char_of = Option.value_map ~default:"?" ~f:Int.to_string in
    printf
      "  %d:%s - %d:%s   %S ... %S\n"
      range.startLine
      (char_of range.startCharacter)
      range.endLine
      (char_of range.endCharacter)
      (line_text range.startLine)
      (line_text range.endLine))
;;

(* The gutter arrow on a line is backed by whichever range the client picks among the
   ranges starting there. More than one candidate means the fold is client-dependent, so
   these are the lines worth scrutinizing. *)
let render_contested_lines lines ranges =
  let by_start_line =
    List.sort_and_group ranges ~compare:(fun (a : FoldingRange.t) b ->
      Int.compare a.startLine b.startLine)
    |> List.filter ~f:(fun group -> List.length group > 1)
  in
  match by_start_line with
  | [] -> print_endline "  (none: every fold arrow has exactly one candidate range)"
  | groups ->
    List.iter groups ~f:(fun group ->
      let start_line = (List.hd_exn group).FoldingRange.startLine in
      printf "  line %d %S ends at line " start_line (String.strip lines.(start_line));
      List.map group ~f:(fun (r : FoldingRange.t) -> Int.to_string r.endLine)
      |> String.concat ~sep:" or "
      |> print_endline)
;;

let print_folding_ranges ?path ~source () =
  let%map ranges = request_folding_ranges ?path ~source () in
  let lines = Array.of_list (String.split source ~on:'\n') in
  match ranges with
  | None -> print_endline "no folding ranges"
  | Some ranges ->
    let ranges = sort_ranges ranges in
    render_diagram lines ranges;
    print_endline "ranges (startLine:char - endLine:char):";
    render_range_list lines ranges;
    print_endline "lines with more than one candidate fold:";
    render_contested_lines lines ranges
;;

(* Regression test for a report that folding at the [let module] line swallowed the rest
   of the function body. [Pexp_letmodule] used to push its own location, which spans the
   body the module is in scope for. The only range starting on that line should now be the
   [struct ... end] one. *)
let%expect_test "first-class module bound inside a function" =
  let%map () =
    print_folding_ranges
      ~source:
        {ocaml|let foo =
  let module M = struct
    type t = unit
  end
  in
  let bar = () in
  bar
;;
|ocaml}
      ()
  in
  [%expect
    {|
    /   0 | let foo =
    |/  1 |   let module M = struct
    ||  2 |     type t = unit
    |\  3 |   end
    |   4 |   in
    |   5 |   let bar = () in
    \   6 |   bar
        7 | ;;
        8 |
    ranges (startLine:char - endLine:char):
      0:0 - 6:5   "let foo =" ... "bar"
      1:17 - 3:5   "let module M = struct" ... "end"
    lines with more than one candidate fold:
      (none: every fold arrow has exactly one candidate range)
    |}]
;;

let%expect_test "module bound at the top level" =
  let%map () =
    print_folding_ranges
      ~source:
        {ocaml|module M = struct
  type t = unit
end

let foo =
  let bar = () in
  bar
;;
|ocaml}
      ()
  in
  [%expect
    {|
    //   0 | module M = struct
    ||   1 |   type t = unit
    \\   2 | end
         3 |
      /  4 | let foo =
      |  5 |   let bar = () in
      \  6 |   bar
         7 | ;;
         8 |
    ranges (startLine:char - endLine:char):
      0:0 - 2:3   "module M = struct" ... "end"
      0:11 - 2:3   "module M = struct" ... "end"
      4:0 - 6:5   "let foo =" ... "bar"
    lines with more than one candidate fold:
      line 0 "module M = struct" ends at line 2 or 2
    |}]
;;

let%expect_test "let open, let exception, and plain let in a function body" =
  let%map () =
    print_folding_ranges
      ~source:
        {ocaml|let foo =
  let open List in
  let exception E in
  let bar =
    ()
  in
  bar
;;
|ocaml}
      ()
  in
  [%expect
    {|
    /   0 | let foo =
    |   1 |   let open List in
    |   2 |   let exception E in
    |/  3 |   let bar =
    |\  4 |     ()
    |   5 |   in
    \   6 |   bar
        7 | ;;
        8 |
    ranges (startLine:char - endLine:char):
      0:0 - 6:5   "let foo =" ... "bar"
      3:2 - 4:6   "let bar =" ... "()"
    lines with more than one candidate fold:
      (none: every fold arrow has exactly one candidate range)
    |}]
;;

let%expect_test "two first-class modules in sequence" =
  let%map () =
    print_folding_ranges
      ~source:
        {ocaml|let foo =
  let module M = struct
    type t = unit
  end
  in
  let module N = struct
    type t = int
  end
  in
  let bar = () in
  bar
;;
|ocaml}
      ()
  in
  [%expect
    {|
    /    0 | let foo =
    |/   1 |   let module M = struct
    ||   2 |     type t = unit
    |\   3 |   end
    |    4 |   in
    | /  5 |   let module N = struct
    | |  6 |     type t = int
    | \  7 |   end
    |    8 |   in
    |    9 |   let bar = () in
    \   10 |   bar
        11 | ;;
        12 |
    ranges (startLine:char - endLine:char):
      0:0 - 10:5   "let foo =" ... "bar"
      1:17 - 3:5   "let module M = struct" ... "end"
      5:17 - 7:5   "let module N = struct" ... "end"
    lines with more than one candidate fold:
      (none: every fold arrow has exactly one candidate range)
    |}]
;;

let%expect_test "first-class module with a one-line remainder" =
  let%map () =
    print_folding_ranges
      ~source:
        {ocaml|let foo =
  let module M = struct
    type t = unit
  end in
  ()
;;
|ocaml}
      ()
  in
  [%expect
    {|
    /   0 | let foo =
    |/  1 |   let module M = struct
    ||  2 |     type t = unit
    |\  3 |   end in
    \   4 |   ()
        5 | ;;
        6 |
    ranges (startLine:char - endLine:char):
      0:0 - 4:4   "let foo =" ... "()"
      1:17 - 3:5   "let module M = struct" ... "end in"
    lines with more than one candidate fold:
      (none: every fold arrow has exactly one candidate range)
    |}]
;;
