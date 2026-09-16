open Import

(* Attribute names used by [ppx_template] (e.g. [[@@kind k = (value, bits64)]]). We match
   on the part of the name before the first ['.'] so as to also catch dotted variants like
   [[@@kind.default ...]] and [[@@mode.explicit ...]]. *)
let is_template_attribute (attr : Parsetree.attribute) =
  let name =
    match String.lsplit2 attr.attr_name.txt ~on:'.' with
    | None -> attr.attr_name.txt
    | Some (name, (_ : string)) -> name
  in
  match name with
  | "kind" | "kind_set" | "mode" | "modality" | "alloc" | "synchro" -> true
  | _ -> false
;;

let is_doc_attribute (attr : Parsetree.attribute) =
  match attr.attr_name.txt with
  | "ocaml.doc" | "ocaml.text" | "doc" -> true
  | _ -> false
;;

(* Rewrites a declaration to remove all kinds, modes, and modalities, along with the
   attributes selected by [strip_attribute]. *)
let deoxidize_mapper ~strip_attribute : Ast_mapper.mapper =
  let default = Ast_mapper.default_mapper in
  let strip_vars vars =
    List.map vars ~f:(fun (var, (_ : Parsetree.jkind_annotation option)) -> var, None)
  in
  { default with
    attributes =
      (fun this attributes ->
        List.filter attributes ~f:(fun attribute -> not (strip_attribute attribute))
        |> default.attributes this)
  ; modes = (fun (_ : Ast_mapper.mapper) (_ : Parsetree.modes) -> [])
  ; modalities = (fun (_ : Ast_mapper.mapper) (_ : Parsetree.modalities) -> [])
  ; typ =
      (fun this typ ->
        let typ : Parsetree.core_type =
          let subsume_attributes (inner : Parsetree.core_type) =
            { inner with ptyp_attributes = inner.ptyp_attributes @ typ.ptyp_attributes }
          in
          match typ.ptyp_desc with
          | Ptyp_any (Some (_ : Parsetree.jkind_annotation)) ->
            { typ with ptyp_desc = Ptyp_any None }
          | Ptyp_var (var, Some (_ : Parsetree.jkind_annotation)) ->
            { typ with ptyp_desc = Ptyp_var (var, None) }
          | Ptyp_alias (inner, None, Some (_ : Parsetree.jkind_annotation)) ->
            (* [T as (_ : k)]: dropping the kind annotation leaves nothing to alias. *)
            subsume_attributes inner
          | Ptyp_alias (inner, (Some _ as name), Some (_ : Parsetree.jkind_annotation)) ->
            { typ with ptyp_desc = Ptyp_alias (inner, name, None) }
          | Ptyp_poly (vars, inner) ->
            { typ with ptyp_desc = Ptyp_poly (strip_vars vars, inner) }
          | Ptyp_newlayout ((_ : string Loc.loc list), inner) ->
            (* [layout_ a. T]: locally abstract layouts only exist to be referred to by
               kind annotations, which we are stripping. *)
            subsume_attributes inner
          | _ -> typ
        in
        default.typ this typ)
  ; type_declaration =
      (fun this decl ->
        let decl = default.type_declaration this decl in
        { decl with ptype_jkind_annotation = None })
  ; constructor_declaration =
      (fun this decl ->
        let decl = default.constructor_declaration this decl in
        { decl with pcd_vars = strip_vars decl.pcd_vars })
  ; extension_constructor =
      (fun this ext ->
        let ext = default.extension_constructor this ext in
        match ext.pext_kind with
        | Pext_decl (vars, args, res) ->
          { ext with pext_kind = Pext_decl (strip_vars vars, args, res) }
        | Pext_rebind (_ : Longident.t Loc.loc) -> ext)
  }
;;

(** [trigger_mapper] is used only for checking whether there's anything that causes us to
    want to display the codelens. *)
let trigger_mapper = deoxidize_mapper ~strip_attribute:is_template_attribute

(** [display_mapper] is used to actually determine what to display in the codelens. *)
let display_mapper = deoxidize_mapper ~strip_attribute:(fun _ -> true)

(* Prepares a declaration for display in a code lens (and for comparison against its
   deoxidized version):
   - drops doc comments, which shouldn't be repeated in the lens;
   - elides [sig ... end] bodies of module (type) declarations, so that only the
     declaration at the top is shown. Items inside the body get their own lenses.

   Both the original and the deoxidized declaration are passed through this mapper before
   being compared, so the comparison only sees differences that [trigger_mapper] made (and
   only those visible after eliding module bodies). Passing both through a mapper also
   normalizes fields that mappers don't preserve (e.g. [ptyp_loc_stack]). *)
let cleanup_for_display_mapper : Ast_mapper.mapper =
  let default = Ast_mapper.default_mapper in
  { default with
    attributes =
      (fun this attributes ->
        List.filter attributes ~f:(fun attribute -> not (is_doc_attribute attribute))
        |> default.attributes this)
  ; module_type =
      (fun this mty ->
        match mty.pmty_desc with
        | Pmty_signature
            { psg_modalities; psg_items = (_ : Parsetree.signature_item list); psg_loc }
          ->
          { mty with
            pmty_desc =
              Pmty_signature
                { psg_modalities = this.modalities this psg_modalities
                ; psg_items = []
                ; psg_loc
                }
          ; pmty_attributes = this.attributes this mty.pmty_attributes
          }
        | _ -> default.module_type this mty)
  }
;;

let apply mapper item = mapper.Ast_mapper.signature_item mapper item
let terminal_sig_end = lazy (Re2.create_exn {re|sig\s+end$|re})
let sig_end = lazy (Re2.create_exn {re|sig\s+end|re})
let interior_space_run = lazy (Re2.create_exn {re|(\S)  +|re})

let print_signature_item item =
  Format.asprintf "%a" Pprintast.signature_item item
  (* Collapse [Pprintast] artifacts like the double space in [module type S  = sig], while
     leaving indentation at the start of lines alone. *)
  |> Re2.rewrite_exn (Lazy.force interior_space_run) ~template:{re|\1 |re}
  (* An empty signature here is (almost certainly) a module body elided by
     [cleanup_for_display_mapper]; render just the declaration up to the [sig]. *)
  (* A [sig end] at the end of the line indicates the elided body of the module. In this
     case, drop the [end] to match the line declaring the module. *)
  |> Re2.rewrite_exn (Lazy.force terminal_sig_end) ~template:"sig"
  (* Any remaining instance of [sig end] is likely the parameter of a functor. Replace it
     with [<sig>] to make it clear that there is an elided signature. *)
  |> Re2.rewrite_exn (Lazy.force sig_end) ~template:"<sig>"
;;

let signature_item (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_value _
  | Psig_type _
  | Psig_typesubst _
  | Psig_typext _
  | Psig_exception _
  | Psig_module _
  | Psig_recmodule _
  | Psig_modtype _
  | Psig_modtypesubst _
  | Psig_include _ ->
    let original = apply cleanup_for_display_mapper item in
    let deoxidized = apply cleanup_for_display_mapper (apply trigger_mapper item) in
    (match Poly.equal original deoxidized with
     | true -> []
     | false ->
       let range = Range.of_loc item.psig_loc in
       let title =
         print_signature_item
           (apply cleanup_for_display_mapper (apply display_mapper item))
       in
       let command = Command.create ~title ~command:"" () in
       [ CodeLens.create ~range ~command () ])
  | Psig_modsubst _
  | Psig_open _
  | Psig_class _
  | Psig_class_type _
  | Psig_attribute _
  | Psig_extension _
  | Psig_jkind _ -> []
;;

let structure_item (item : Parsetree.structure_item) =
  match item.pstr_desc with
  | Pstr_modtype decl ->
    (* Reuse the signature-item handling so that a [module type] declaration in an
       implementation can itself get a lens (with its body elided), just as it would in an
       interface. The items inside its body get their own lenses when the iterator
       descends into them. *)
    signature_item { psig_desc = Psig_modtype decl; psig_loc = item.pstr_loc }
  | Pstr_eval _
  | Pstr_value _
  | Pstr_primitive _
  | Pstr_type _
  | Pstr_typext _
  | Pstr_exception _
  | Pstr_module _
  | Pstr_recmodule _
  | Pstr_open _
  | Pstr_class _
  | Pstr_class_type _
  | Pstr_include _
  | Pstr_attribute _
  | Pstr_extension _
  | Pstr_jkind _ -> []
;;

let with_iterator f =
  let acc = ref [] in
  let wrap_pure ~get_lenses ~default_iterator this parsetree_node =
    let lenses = get_lenses parsetree_node in
    acc := lenses :: !acc;
    default_iterator this parsetree_node
  in
  let iterator =
    { Ast_iterator.default_iterator with
      signature_item =
        wrap_pure
          ~get_lenses:signature_item
          ~default_iterator:Ast_iterator.default_iterator.signature_item
    ; structure_item =
        wrap_pure
          ~get_lenses:structure_item
          ~default_iterator:Ast_iterator.default_iterator.structure_item
    }
  in
  f iterator;
  List.rev !acc |> List.concat
;;

let for_parsetree parsetree =
  with_iterator (fun iterator ->
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure)
;;

let for_document ~log_info doc =
  Fiber.map
    (Document.Merlin.with_pipeline_exn
       ~log_info
       ~priority:Priorities.code_lens
       doc
       Mpipeline.reader_parsetree)
    ~f:for_parsetree
;;
