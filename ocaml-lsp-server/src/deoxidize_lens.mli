open! Import

(** Code lenses that show a "deoxidized" version of signature declarations: for each
    [val], [type], [exception], [module], [module type], and [include] declaration in a
    signature, if the declaration carries kind annotations, mode/modality annotations, or
    ppx_template annotations, we show the declaration with all of those stripped off.
    Declarations with nothing to strip get no lens. *)
val for_document : log_info:Log_info.t -> Document.Merlin.t -> CodeLens.t list Fiber.t

val for_parsetree : Mreader.parsetree -> CodeLens.t list
