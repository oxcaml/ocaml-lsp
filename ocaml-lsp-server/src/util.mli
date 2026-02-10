open Import

type 't req_params_spec =
  { params_schema : Jsonrpc.Structured.t
    (** used to document the structure of the params; example:
      [`Assoc [ "uri" , `String "<Uri>" ]]; *)
  ; of_jsonrpc_params : Jsonrpc.Structured.t -> 't option
    (** parses given structured JSON if it's of the expected schema; otherwise, return
      [None] *)
  }

val of_jsonrpc_params_exn
  :  'req_params req_params_spec
  -> Jsonrpc.Structured.t option
  -> 'req_params

val open_document_from_file : State.t -> DocumentUri.t -> Document.t option Fiber.t
val is_at_cursor : Position.t -> Loc.t -> bool
val get_doc_id : params:Jsonrpc.Structured.t option -> TextDocumentIdentifier.t option
val get_pos : params:Jsonrpc.Structured.t option -> Position.t option

(** [parse_formatted_parameters str ~offset] returns the list of parameter positions of a formatted [str] with an [~offset]. The top-level arrow delimits parameters, and the ranges include the character after the arrow, which could be either a whitespace or a newline. *)
val parse_formatted_parameters : string -> offset:int -> (int * int) list
