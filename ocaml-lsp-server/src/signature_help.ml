open Import
module List = Merlin_utils.Std.List
module String = Merlin_utils.Std.String
module Misc_utils = Merlin_analysis.Misc_utils
module Type_utils = Merlin_analysis.Type_utils

let priority = Priorities.signature_help

open struct
  open Ocaml_typing
  module Predef = Predef
  module Btype = Btype
end

let format_doc ~markdown ~doc =
  `MarkupContent
    (if markdown
     then (
       let value =
         match Doc_to_md.translate doc with
         | Raw d -> sprintf "(** %s *)" d
         | Markdown d -> d
       in
       { MarkupContent.value; kind = MarkupKind.Markdown })
     else { MarkupContent.value = doc; kind = MarkupKind.PlainText })
;;

let run ~log_info (state : State.t) { SignatureHelpParams.textDocument = { uri }; position; _ } =
  let open Fiber.O in
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  let pos = Position.logical position in
  let prefix =
    (* The value of [short_path] doesn't make a difference to the final result because
       labels cannot include dots. However, a true value is slightly faster for getting
       the prefix. *)
    Compl.prefix_of_position (Document.source doc) pos ~short_path:`Suffix
  in
  (* TODO use merlin resources efficiently and do everything in 1 thread *)
  match Document.kind doc with
  | `Other ->
    let help = Some (SignatureHelp.create ~signatures:[] ()) in
    Fiber.return help
  | `Merlin merlin ->
    let* application_signature =
      let* inside_comment = Check_for_comments.position_in_comment ~log_info ~position ~merlin ~priority in
      match inside_comment with
      | true -> Fiber.return None
      | false ->
        Document.Merlin.with_pipeline_exn ~log_info ~priority merlin (fun pipeline ->
          let typer = Mpipeline.typer_result pipeline in
          let pos = Mpipeline.get_lexing_pos pipeline pos in
          let node = Mtyper.node_at typer pos in
          Merlin_analysis.Signature_help.application_signature node ~prefix ~cursor:pos)
    in
    (match application_signature with
     | None ->
       let help = Some (SignatureHelp.create ~signatures:[] ()) in
       Fiber.return help
     | Some application_signature ->
       let prefix =
         let fun_name = Option.value ~default:"_" application_signature.function_name in
         sprintf "%s : " fun_name
       in
       let offset = String.length prefix in
       let* doc =
         Document.Merlin.doc_comment
           ~log_info
           ~priority:Priorities.signature_help
           merlin
           application_signature.function_position
       in
       let+ formatted_signature =
         let typ = application_signature.signature in
         let+ result = Ocamlformat_rpc.format_type state.ocamlformat_rpc ~typ in
         match result with
         | Ok v -> String.trim v
         | Error _ -> typ
       in
       let info =
         let parameters =
          (* If the signature was formatted, parameter positions must be recalculated. *)
           if application_signature.signature = formatted_signature
           then
             List.map
               application_signature.parameters
               ~f:(fun (p : Merlin_analysis.Signature_help.parameter_info) ->
                 let label = `Offset (offset + p.param_start, offset + p.param_end) in
                 ParameterInformation.create ~label ())
           else (
             let parameters =
               Util.parse_formatted_parameters formatted_signature ~offset
             in
             List.map parameters ~f:(fun (s, e) ->
               let label = `Offset (s, e) in
               ParameterInformation.create ~label ()))
         in
         let documentation =
           let open Option.O in
           let+ doc in
           let markdown =
             ClientCapabilities.markdown_support
               (State.client_capabilities state)
               ~field:(fun td ->
                 let* sh = td.signatureHelp in
                 let+ si = sh.signatureInformation in
                 si.documentationFormat)
           in
           format_doc ~markdown ~doc
         in
         let label = prefix ^ formatted_signature in
         SignatureInformation.create ~label ?documentation ~parameters ()
       in
       Some
         (SignatureHelp.create
            ~signatures:[ info ]
            ~activeSignature:0
            ?activeParameter:(Some application_signature.active_param)
            ()))
;;
