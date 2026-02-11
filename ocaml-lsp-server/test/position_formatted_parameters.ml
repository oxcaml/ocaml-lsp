open Ocaml_lsp_server.Util

(** This set of tests aims to validate that the `parse_formatted_parameters`
    function correctly returns the positions of the parameters in a formatted
    signature. *)

let pos_to_string l =
  List.iter (fun (s, e) -> Printf.printf "(%d;%d)" s e) l

let prefix_test str offset =
  let positions = parse_formatted_parameters str ~offset in
  pos_to_string positions
;;

let%expect_test "single parameter" =
  prefix_test "int -> unit" 0;
  [%expect {| (0;3) |}]
;;

let%expect_test "multiple simple parameters" =
  prefix_test "int -> string -> bool -> unit" 0;
  [%expect {| (0;3)(6;13)(16;21) |}]
;;

let%expect_test "named parameters" =
  prefix_test "x:int -> y:string -> z:bool -> unit" 0;
  [%expect {| (0;5)(8;17)(20;27) |}]
;;

let%expect_test "parenthesized parameter" =
  prefix_test "(int * string) -> bool -> unit" 0;
  [%expect {| (0;14)(17;22) |}]
;;

let%expect_test "multiline simple formatting" =
  prefix_test "int ->\nstring ->\nbool ->\nunit" 0;
  [%expect {| (0;3)(6;13)(16;21) |}]
;;

let%expect_test "with offset" =
  prefix_test "int -> string -> unit" 5;
  [%expect {| (5;8)(11;18) |}]
;;

let%expect_test "labeled and optional mix" =
  prefix_test "~x:int -> ?y:string -> bool -> unit" 0;
  [%expect {| (0;6)(9;19)(22;27) |}]
;;

let%expect_test "optional without explicit type annotation spacing" =
  prefix_test "?x:int->?y:string->unit" 0;
  [%expect {| (0;5)(8;16) |}]
;;

let%expect_test "multiline optional arguments" =
  prefix_test "?x:int ->\n?y:string ->\nbool ->\nunit" 0;
  [%expect {| (0;6)(9;19)(22;27) |}]
;;

let%expect_test "optional with offset" =
  prefix_test "?x:int -> ?y:string -> unit" 10;
  [%expect {| (10;16)(19;29) |}]
;;

let%expect_test "formatted signature parsing positions"  =
  prefix_test "int ->\nint ->\nint -> f:string\n-> d:\n  (unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit ->\n  unit) -> unit" 0;
  [%expect {| (0;3)(6;10)(13;17)(20;29)(32;164) |}]
;;

let%expect_test "nested parenthesis" =
  prefix_test "?x:int -> (?y:string -> (int -> ?v:string -> int) -> unit) -> unit" 0;
  [%expect {| (0;6)(9;58) |}]
;;
