(** Session-level metadata included in every log row. This is common between all users of
    [ocaml-lsp-logging] *)
type t =
  { username : string
  ; deployment_stage : Stage.t
  ; sensitivity_context : Sensitivity_context.t
  ; client_info : Lsp.Types.Client_info.t option
  ; host : string
  ; ocaml_version : string
  ; build_version_jane : string option
  ; worker_name : string option
  (** Logical name of the ocaml-lsp worker process behind this session (as passed via
      [ocaml-lsp -worker-name]). Populated by [ocaml-lsp-wrapper] when it spawns workers
      so that each worker's logs can be distinguished. *)
  }
[@@deriving sexp_of]

(** Create a new session info

    [username], if provided, is logged instead of the user running this process *)
val create
  :  deployment_stage:Stage.t
  -> client_info:Lsp.Types.Client_info.t option
  -> worker_name:string option
  -> username:string option
  -> t

val get_editor_name : t -> string
val get_editor_version : t -> string
val get_ai_model_name : t -> string option
