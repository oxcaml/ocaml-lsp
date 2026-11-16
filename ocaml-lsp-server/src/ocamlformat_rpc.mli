(** Functions for styling pieces of OCaml code that the LSP processes, to make them look
    better when presented to the user. *)

module Format_result = Ocaml_lsp_code_formatter.Format_result

(** Format the given OCaml type or module signature.

    Module signatures look like [sig ... end] (e.g. ["sig type t end"]). Type signatures
    are anything other than that form (e.g. ["int -> int"]).

    Callers should fall back to the unformatted type silently on
    [Formatting_not_supported]. *)
val format_type : typ:string -> Format_result.t

(** Requires that the program is a syntactically correct OCaml program. *)
val format_code : string -> Format_result.t
