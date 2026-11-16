open! Core
module Format_result = Ocaml_lsp_code_formatter.Format_result

(* Remove the common indentation of the lines before the first blank line. Ocamlformat
   indents the right-hand side of the synthetic type declaration in [format_type], while
   anything after a blank line (e.g. type definitions shown on verbose hovers) is a
   separate top-level item and already starts at column 0. *)
let dedent_first_block text =
  let lines = String.split text ~on:'\n' in
  let first_block, rest =
    List.split_while lines ~f:(fun line -> not (String.is_empty (String.strip line)))
  in
  let indent =
    List.map first_block ~f:(fun line ->
      String.length line - String.length (String.lstrip line))
    |> List.min_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let first_block =
    List.map first_block ~f:(fun line -> String.drop_prefix line indent)
  in
  String.concat ~sep:"\n" (first_block @ rest)
;;

let format_type ~typ : Format_result.t =
  let prefix, format_result_fn =
    (* The types returned by this are either signature or value types, and signature types
       will start with sig. We need this prefix because the formatter doesn't do anything
       on OCaml code that has a syntax error *)
    match String.is_prefix typ ~prefix:"sig\n" with
    | true -> "module T :", fun formatted_typ -> String.strip formatted_typ
    | false ->
      (* We use this specific name so that the = sign occurs on the 90th character so
         Ocamlformat will start the type on the next line *)
      ( "type \
         t1234567891234567891234567891234567891234567891234567891234567891234567891234567891 \
         = "
      , fun formatted_typ ->
          (* Remove initial/ending newlines if they exist (Ocamlformat produces exactly 1
             of each due to our setup) *)
          let formatted_typ =
            String.chop_prefix_if_exists ~prefix:"\n" formatted_typ
            |> String.chop_suffix_if_exists ~suffix:"\n"
          in
          (* Only strip if there are no newlines in the type - we don't always strip
             because we want to preserve relative indentation for types like
             {v
               Long_name_module.t
               -> Longer_name_module.t
               -> ...
             v} *)
          (match String.contains formatted_typ '\n' with
           | false -> String.strip formatted_typ
           | true -> dedent_first_block formatted_typ) )
  in
  match Ocaml_lsp_code_formatter.format_ocaml_intf (prefix ^ typ) with
  | Ok new_typ ->
    Ok
      (new_typ
       |> String.chop_prefix_if_exists ~prefix:(String.strip prefix)
       |> format_result_fn)
  | (Error (_ : Error.t) | Formatting_not_supported) as result -> result
;;

let format_code code = Ocaml_lsp_code_formatter.format_ocaml_intf code
