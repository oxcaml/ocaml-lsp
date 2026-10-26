open Odoc_parser

let nestable_block_element = function
  | `Code_block { Ast.meta; content; _ } ->
    `Code_block (Option.map (fun { Ast.language; _ } -> language) meta, content)
  | `Table ((grid, _), _) -> `Table (List.map (List.map fst) grid)
  | `Media _ -> `Media
  | (`Paragraph _ | `Verbatim _ | `Modules _ | `List _ | `Math_block _) as element ->
    element
;;

let tag tag = tag
