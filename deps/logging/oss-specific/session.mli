(** Session-level logging context. Created once during LSP initialization. *)
type t = { info : Session_info.t } [@@deriving sexp_of]

val init
  :  deployment_stage:Stage.t
  -> client_info:Lsp.Types.Client_info.t option
  -> worker_name:string option
  -> t Async.Deferred.t

val close : t -> unit Core.Or_error.t Async.Deferred.t
