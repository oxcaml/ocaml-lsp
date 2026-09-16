open Import

module InlayHints = struct
  let default_hint_pattern_variables = false
  let default_hint_let_bindings = false
  let default_hint_function_params = false
  let default_hint_let_syntax_ppx = false

  type t =
    { hint_pattern_variables : bool
         [@default default_hint_pattern_variables] [@key "hintPatternVariables"]
    ; hint_let_bindings : bool
         [@default default_hint_let_bindings] [@key "hintLetBindings"]
    ; hint_function_params : bool
         [@default default_hint_function_params] [@key "hintFunctionParams"]
    ; hint_let_syntax_ppx : bool
         [@default default_hint_let_syntax_ppx] [@key "hintLetSyntaxPpx"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default =
    { hint_pattern_variables = default_hint_pattern_variables
    ; hint_let_bindings = default_hint_let_bindings
    ; hint_function_params = default_hint_function_params
    ; hint_let_syntax_ppx = default_hint_let_syntax_ppx
    }
  ;;
end

module Lens = struct
  let default_enable = false

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

(** Code lenses showing signature declarations with all kinds, modes, and ppx_template
    annotations stripped off. This is deliberately separate from [Lens] (the [codelens]
    setting), which shows inferred types of toplevel items in implementations. *)
module DeoxidizeLens = struct
  let default_enable = false

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module ExtendedHover = struct
  let default_enable = true

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module SyntaxDocumentation = struct
  let default_enable = true

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module WhichDiagnostics = struct
  open Base (* to bring [Bool.equal] into scope *)

  (* Typing errors are not reported if there are syntax errors. If a user has dune errors
     and merlin errors turned on, they will *usually* see reasonable behavior, if merlin
     finds the same errors as the build and the lsp successfully de-duplicates. But both
     of those steps can sometimes fail. *)
  let default_merlin_syntax = false
  let default_merlin_typing = false
  let default_dune = false

  type t =
    { merlin_syntax : bool [@default default_merlin_syntax]
    ; merlin_typing : bool [@default default_merlin_typing]
    ; dune : bool [@default default_dune]
    }
  [@@deriving yojson, equal] [@@yojson.allow_extra_fields]

  let default =
    { merlin_syntax = default_merlin_syntax
    ; merlin_typing = default_merlin_typing
    ; dune = default_dune
    }
  ;;
end

module ShortenMerlinDiagnostics = struct
  let default_enable = false

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module PpxCssColors = struct
  let default_enable = true

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

(** Whether we should fall back to remote-lsp when there is no local build running. This
    must be turned off on remote-lsp lsp servers!

    We therefore default this setting to false. We expect that editors will set it to true
    in their default configs. *)
module RemoteLspFallback = struct
  let default_enable = false

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module DuneBuildOnOpen = struct
  let default_enable = false

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module UseFastCompl = struct
  let default_enable = true

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module FuzzyCompletion = struct
  let default_enable = false

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module FilterDoubleUnderscore = struct
  let default_enable = true

  type t = { enable : bool [@default default_enable] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = default_enable }
end

module DocumentSymbol = struct
  let default_include_local_bindings = false

  type t =
    { include_local_bindings : bool
         [@default default_include_local_bindings] [@key "includeLocalBindings"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { include_local_bindings = default_include_local_bindings }
end

(* NB: if you change one of the string names of the fields here, make sure to grep the
   codebase for occurences of the old name, as it may be used in yojson fragments, and the
   typechecker won't catch the mismatch between the old name and the new name. *)
type t =
  { codelens : Lens.t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
  ; deoxidize_lens : DeoxidizeLens.t Json.Nullable_option.t
       [@key "deoxidizeLens"] [@default None] [@yojson_drop_default ( = )]
  ; extended_hover : ExtendedHover.t Json.Nullable_option.t
       [@key "extendedHover"] [@default None] [@yojson_drop_default ( = )]
  ; inlay_hints : InlayHints.t Json.Nullable_option.t
       [@key "inlayHints"] [@default None] [@yojson_drop_default ( = )]
  ; syntax_documentation : SyntaxDocumentation.t Json.Nullable_option.t
       [@key "syntaxDocumentation"] [@default None] [@yojson_drop_default ( = )]
  ; which_diagnostics : WhichDiagnostics.t Json.Nullable_option.t
       [@key "whichDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; shorten_merlin_diagnostics : ShortenMerlinDiagnostics.t Json.Nullable_option.t
       [@key "shortenMerlinDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; ppx_css_colors : PpxCssColors.t Json.Nullable_option.t
       [@key "ppxCssColors"] [@default None] [@yojson_drop_default ( = )]
  ; remote_lsp_fallback : RemoteLspFallback.t Json.Nullable_option.t
       [@key "remoteLspFallback"] [@default None] [@yojson_drop_default ( = )]
  ; dune_build_on_open : DuneBuildOnOpen.t Json.Nullable_option.t
       [@key "duneBuildOnOpen"] [@default None] [@yojson_drop_default ( = )]
  ; (* When true, [ocaml_lsp_wrapper] forwards [completion] requests to both the canonical
       [ocaml-lsp] and the [ocaml-lsp-fast-compl]. NB: this is intentionally named
       "FastCompl" instead of "FastCompletion" to map to the binary name
       [ocaml_lsp_fast_compl]. *)
    fast_compl : UseFastCompl.t Json.Nullable_option.t
       [@key "useFastCompl"] [@default None] [@yojson_drop_default ( = )]
  ; (* When [true] ocaml-lsp ignore text after the last '.' and let the LSP client to
       fuzzy match. This improves UX for VSCode users. *)
    fuzzy_completion : FuzzyCompletion.t Json.Nullable_option.t
       [@key "fuzzyCompletion"] [@default None] [@yojson_drop_default ( = )]
  ; filter_double_underscore : FilterDoubleUnderscore.t Json.Nullable_option.t
       [@key "filterDoubleUnderscore"] [@default None] [@yojson_drop_default ( = )]
  ; document_symbol : DocumentSymbol.t Json.Nullable_option.t
       [@key "documentSymbol"] [@default None] [@yojson_drop_default ( = )]
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let default =
  { codelens = Some Lens.default
  ; deoxidize_lens = Some DeoxidizeLens.default
  ; extended_hover = Some ExtendedHover.default
  ; inlay_hints = Some InlayHints.default
  ; syntax_documentation = Some SyntaxDocumentation.default
  ; which_diagnostics = Some WhichDiagnostics.default
  ; shorten_merlin_diagnostics = Some ShortenMerlinDiagnostics.default
  ; ppx_css_colors = Some PpxCssColors.default
  ; remote_lsp_fallback = Some RemoteLspFallback.default
  ; dune_build_on_open = Some DuneBuildOnOpen.default
  ; fast_compl = Some UseFastCompl.default
  ; fuzzy_completion = Some FuzzyCompletion.default
  ; filter_double_underscore = Some FilterDoubleUnderscore.default
  ; document_symbol = Some DocumentSymbol.default
  }
;;

(* The accessors below resolve unset config sections to their defaults, so that the
   default behavior of each setting is defined in exactly one place: the [default_*]
   constants in the modules above. *)

let is_codelens_enabled t =
  Option.value t.codelens ~default:Lens.default |> fun { enable } -> enable
;;

let is_deoxidize_lens_enabled t =
  Option.value t.deoxidize_lens ~default:DeoxidizeLens.default |> fun { enable } -> enable
;;

let is_extended_hover_enabled t =
  Option.value t.extended_hover ~default:ExtendedHover.default |> fun { enable } -> enable
;;

let inlay_hints t = Option.value t.inlay_hints ~default:InlayHints.default

let is_syntax_documentation_enabled t =
  Option.value t.syntax_documentation ~default:SyntaxDocumentation.default
  |> fun { enable } -> enable
;;

let which_diagnostics t =
  Option.value t.which_diagnostics ~default:WhichDiagnostics.default
;;

let is_shorten_merlin_diagnostics_enabled t =
  Option.value t.shorten_merlin_diagnostics ~default:ShortenMerlinDiagnostics.default
  |> fun { enable } -> enable
;;

let is_ppx_css_colors_enabled t =
  Option.value t.ppx_css_colors ~default:PpxCssColors.default |> fun { enable } -> enable
;;

let is_remote_lsp_fallback_enabled t =
  Option.value t.remote_lsp_fallback ~default:RemoteLspFallback.default
  |> fun { enable } -> enable
;;

let is_dune_build_on_open_enabled t =
  Option.value t.dune_build_on_open ~default:DuneBuildOnOpen.default
  |> fun { enable } -> enable
;;

let is_fast_compl_enabled t =
  Option.value t.fast_compl ~default:UseFastCompl.default |> fun { enable } -> enable
;;

let is_fuzzy_completion_enabled t =
  Option.value t.fuzzy_completion ~default:FuzzyCompletion.default
  |> fun { enable } -> enable
;;

let is_filter_double_underscore_enabled t =
  Option.value t.filter_double_underscore ~default:FilterDoubleUnderscore.default
  |> fun { enable } -> enable
;;

let document_symbol t = Option.value t.document_symbol ~default:DocumentSymbol.default
