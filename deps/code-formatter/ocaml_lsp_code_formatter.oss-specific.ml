open! Core

module Format_result = struct
  type t =
    | Ok of string
    | Error of Error.t
    | Formatting_not_supported
end

let format_ocaml_intf _contents : Format_result.t = Formatting_not_supported
