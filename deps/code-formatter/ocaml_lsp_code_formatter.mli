open! Core

module Format_result : sig
  type t =
    | Ok of string
    | Error of Error.t
    | Formatting_not_supported
end

(** Format [contents] as the contents of an OCaml interface file. Requires [contents] to
    be syntactically valid. *)
val format_ocaml_intf : string -> Format_result.t
