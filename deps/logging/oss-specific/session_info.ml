open! Core

module Client_info = struct
  (* Redeclared with a type equation for [sexp_of] derivation. *)
  type t = Lsp.Types.Client_info.t =
    { name : string
    ; version : string option
    ; ai_model_name : string option
    }
  [@@deriving sexp_of]
end

type t =
  { username : string
  ; deployment_stage : Stage.t
  ; log_paths_and_features : bool
  ; client_info : Client_info.t option
  ; host : string
  ; ocaml_version : string
  ; build_version_jane : string option
  ; worker_name : string option
  }
[@@deriving sexp_of]

let lookup_username () = Option.value (Sys.getenv "USER") ~default:"unknown"

let create ~deployment_stage ~client_info ~worker_name =
  { username = lookup_username ()
  ; deployment_stage
  ; log_paths_and_features = false
  ; client_info
  ; host = Core_unix.gethostname ()
  ; ocaml_version = Sys.ocaml_version
  ; build_version_jane = None
  ; worker_name
  }
;;

let get_editor_name t =
  match t.client_info with
  | None -> Lsp.Types.Client_info.default_editor_name
  | Some client_info -> Lsp.Types.Client_info.get_editor_name client_info
;;

let get_editor_version t =
  match t.client_info with
  | None -> Lsp.Types.Client_info.default_editor_version
  | Some client_info -> Lsp.Types.Client_info.get_editor_version client_info
;;

let get_ai_model_name t =
  match t.client_info with
  | None -> None
  | Some client_info -> Lsp.Types.Client_info.get_ai_model_name client_info
;;
