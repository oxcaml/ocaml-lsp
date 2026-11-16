module Fiber = Ocaml_lsp_fiber
open Async
open Test.Import

val iter_code_actions
  :  ?prep:(unit Test.Import.Client.t -> unit Fiber.t)
  -> ?path:string
  -> ?diagnostics:Diagnostic.t list
  -> source:string
  -> Range.t
  -> (CodeActionResult.t -> unit)
  -> unit Deferred.t

(** Print the code actions returned for [range] in [source], keeping only those satisfying
    [filter]. *)
val print_code_actions
  :  ?prep:(unit Test.Import.Client.t -> unit Fiber.t)
  -> ?path:string
  -> ?filter:([ `Command of Command.t | `CodeAction of CodeAction.t ] -> bool)
  -> string
  -> Range.t
  -> unit Deferred.t

(** Whether [action] is a code action whose kind starts with [action_name]. *)
val find_action
  :  string
  -> [ `Command of Command.t | `CodeAction of CodeAction.t ]
  -> bool

val parse_selection : string -> string * Range.t

val apply_code_action
  :  ?path:string
  -> ?diagnostics:Diagnostic.t list
  -> string
  -> string
  -> Range.t
  -> string option Deferred.t

(** [code_action_test title source] runs the code action with title [title] and prints the
    resulting source. *)
val code_action_test : ?path:string -> title:string -> string -> unit Deferred.t
