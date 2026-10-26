let nestable_block_element = function
  | `Code_block (meta, content) -> `Code_block (Option.map fst meta, content)
  | (`Paragraph _ | `Verbatim _ | `Modules _ | `List _ | `Math_block _) as element ->
    element
;;

let tag = function
  | #Odoc_parser.Ast.tag as tag -> tag
;;
