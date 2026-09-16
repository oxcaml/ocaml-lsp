open! Import
open Fiber.O

let priority = Priorities.color

let compute_colors ~log_info document : Lsp.Types.ColorInformation.t list Fiber.t =
  Document.Merlin.with_pipeline_exn ~log_info ~priority document (fun pipeline ->
    let parse_tree : Mreader.parsetree = Mpipeline.reader_parsetree pipeline in
    match parse_tree with
    | `Interface _ -> []
    | `Implementation structure -> Css_lsp.compute_colors structure)
;;

let compute ~log_info server (params : Lsp.Types.DocumentColorParams.t) =
  let state : State.t = Server.state server in
  let feature_enabled = Config_data.is_ppx_css_colors_enabled state.configuration.data in
  let empty () =
    Fiber.return
      (Ocaml_lsp_logging.with_logging ~f:(fun () -> Reply.now []) log_info, state)
  in
  match feature_enabled with
  | false -> empty ()
  | true ->
    let uri = params.textDocument.uri in
    let doc : Document.t option =
      let store = state.store in
      Document_store.get_opt store uri
    in
    (match doc with
     | None -> empty ()
     | Some doc ->
       (match Document.syntax doc with
        | Reason | Ocamllex | Menhir | Cram | Dune -> empty ()
        | Ocaml ->
          (match Document.kind doc with
           | `Other -> empty ()
           | `Merlin doc ->
             Fiber.return
               ( Reply.later (fun send ->
                   let* colors =
                     Ocaml_lsp_logging.with_fiber_logging
                       ~f:(fun () -> compute_colors ~log_info doc)
                       log_info
                   in
                   send colors)
               , state ))))
;;
