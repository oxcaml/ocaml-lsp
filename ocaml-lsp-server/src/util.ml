open Import

type 't req_params_spec =
  { params_schema : Jsonrpc.Structured.t
  ; of_jsonrpc_params : Jsonrpc.Structured.t -> 't option
  }

let of_jsonrpc_params_exn spec params =
  let raise_invalid_params ?data ~message () =
    Jsonrpc.Response.Error.raise
    @@ Jsonrpc.Response.Error.make
         ?data
         ~code:Jsonrpc.Response.Error.Code.InvalidParams
         ~message
         ()
  in
  match params with
  | None -> raise_invalid_params ~message:"Expected params but received none" ()
  | Some params ->
    (match spec.of_jsonrpc_params params with
     | Some t -> t
     | None ->
       let error_json =
         `Assoc
           [ "params_expected", (spec.params_schema :> Json.t)
           ; "params_received", (params :> Json.t)
           ]
       in
       raise_invalid_params ~message:"Unexpected parameter format" ~data:error_json ())
;;

let language_id_of_fname s =
  match Filename.extension s with
  | ".mli" | ".eliomi" -> "ocaml.interface"
  | ".ml" | ".eliom" -> "ocaml"
  | ".rei" | ".re" -> "reason"
  | ".mll" -> "ocaml.ocamllex"
  | ".mly" -> "ocaml.menhir"
  | ext -> Code_error.raise_s [%message "unsupported file extension" ~ext]
;;

let open_document_from_file (state : State.t) uri =
  let open Fiber.O in
  let filename = Uri.to_path uri in
  Fiber.of_thunk (fun () ->
    match Core.Or_error.try_with (fun () -> Core.In_channel.read_all filename) with
    | Error _ ->
      Log.log ~section:"debug" (fun () ->
        Log.msg "Unable to open file" [ "filename", `String filename ]);
      Fiber.return None
    | Ok text ->
      let languageId = language_id_of_fname filename in
      let text_document = TextDocumentItem.create ~uri ~languageId ~version:0 ~text in
      let params = DidOpenTextDocumentParams.create ~textDocument:text_document in
      let+ doc =
        let position_encoding = State.position_encoding state in
        Document.make
          ~position_encoding
          (State.wheel state)
          state.merlin_config
          state.merlin
          params
      in
      Some doc)
;;

let is_at_cursor position =
  let (`Logical (cursor_line, cursor_col)) = Position.logical position in
  let is_at_cursor ({ loc_start; loc_end; _ } : Ocaml_parsing.Location.t) =
    let start_col = loc_start.pos_cnum - loc_start.pos_bol in
    let end_col = loc_end.pos_cnum - loc_end.pos_bol in
    let at_or_after_start =
      loc_start.pos_lnum < cursor_line
      || (loc_start.pos_lnum = cursor_line && start_col <= cursor_col)
    in
    let before_or_at_end =
      loc_end.pos_lnum > cursor_line
      || (loc_end.pos_lnum = cursor_line && end_col >= cursor_col)
    in
    at_or_after_start && before_or_at_end
  in
  is_at_cursor
;;

let get_doc_id ~(params : Jsonrpc.Structured.t option) =
  match params with
  | Some (`Assoc params) ->
    List.Assoc.find ~equal:String.equal params "textDocument"
    |> Option.map ~f:TextDocumentIdentifier.t_of_yojson
  | Some (`List [ json_uri ]) -> Some (TextDocumentIdentifier.t_of_yojson json_uri)
  | _ -> None
;;

let get_pos ~(params : Jsonrpc.Structured.t option) =
  match params with
  | Some (`Assoc params) ->
    List.Assoc.find ~equal:String.equal params "position"
    |> Option.map ~f:Position.t_of_yojson
  | _ -> None
let to_chars s = List.init (String.length s) ~f:(fun e -> String.get s e)

let rec parse_formatted_parameters pdepth acc strl res c : (int * int) list =
  match strl with
  | [] ->
    (* The last element is the result type, it should not appear as a parameter. *)
    res
  | '-' :: '>' :: tl when pdepth = 0 ->
    (* A parameter is delimited by a '->' and if there is no open parenthesis. *)
    let res = (c - List.length acc, c - 1) :: res in
    parse_formatted_parameters pdepth [] tl res (c + 2)
  | hd :: tl ->
    let pdepth =
      match hd with
      | '(' -> pdepth + 1
      | ')' -> pdepth - 1
      | _ -> pdepth
    in
    parse_formatted_parameters pdepth (hd :: acc) tl res (c + 1)
;;

let parse_formatted_parameters str ~offset =
  List.rev (parse_formatted_parameters 0 [] (to_chars str) [] offset)
;;
