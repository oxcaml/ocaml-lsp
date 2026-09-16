open! Core

type t = { info : Session_info.t } [@@deriving sexp_of]

let init ~deployment_stage ~client_info ~worker_name =
  let info = Session_info.create ~deployment_stage ~client_info ~worker_name in
  Async.return { info }
;;

let close _ = Async.return (Ok ())
