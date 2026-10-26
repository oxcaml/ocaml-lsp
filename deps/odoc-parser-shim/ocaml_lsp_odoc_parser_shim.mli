(** The parts of the odoc-parser AST whose shape differs between odoc-parser 2 and 3. *)

open Odoc_parser

(** [`Code_block (language, content)]. [`Table] (the contents of each cell, row by row)
    and [`Media] only come from odoc-parser 3. *)
val nestable_block_element
  :  Ast.nestable_block_element
  -> [ `Paragraph of Ast.inline_element Loc.with_location list
     | `Code_block of string Loc.with_location option * string Loc.with_location
     | `Verbatim of string
     | `Modules of string Loc.with_location list
     | `List of
       [ `Unordered | `Ordered ]
       * [ `Light | `Heavy ]
       * Ast.nestable_block_element Loc.with_location list list
     | `Math_block of string
     | `Table of Ast.nestable_block_element Loc.with_location list list list
     | `Media
     ]

(** [`Hidden], [`Children_order], [`Toc_status], [`Order_category] and [`Short_title] only
    come from odoc-parser 3. *)
val tag
  :  Ast.tag
  -> [ Ast.ocamldoc_tag
     | `Canonical of string Loc.with_location
     | `Inline
     | `Open
     | `Closed
     | `Hidden
     | `Children_order of Ast.nestable_block_element Loc.with_location list
     | `Toc_status of Ast.nestable_block_element Loc.with_location list
     | `Order_category of Ast.nestable_block_element Loc.with_location list
     | `Short_title of Ast.nestable_block_element Loc.with_location list
     ]
