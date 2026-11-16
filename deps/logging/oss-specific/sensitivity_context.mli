open! Core

(** Decides which paths and feature names may be logged *)
type t [@@deriving sexp_of]

val get_workspace_context : unit -> t
