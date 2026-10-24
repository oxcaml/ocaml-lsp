open Core

let split_n_boxed l n = Core.List.split_n l n |> Core.Tuple2.box
let elide_backtrace v = Base.Dynamic.set_root Base.Backtrace.elide v
let ppx_string_config_for_string = Ppx_string.config_for_string Local_input_heap_output
let set_once_set_if_none t ~here value = Core.Set_once.set_if_none t ~here value

let flag_optional_with_default_doc name arg_type sexp_of_default ~default ~doc =
  Command.Param.flag_optional_with_default_doc_sexp
    name
    arg_type
    sexp_of_default
    ~default
    ~doc
;;
