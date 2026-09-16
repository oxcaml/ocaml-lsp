open Async
open Test.Import

(* Print the input source back out, with each lens rendered just above the line it applies
   to (prefixed by [lens_prefix]), the way an editor would show it. That way the reader
   doesn't have to match lenses up with the source by line number. *)
let lens_prefix = ">> "

(* Editors render a lens title on a single line, so collapse any whitespace in the title
   to show what the user will actually see. *)
let render_title title =
  String.split_on_chars title ~on:[ ' '; '\t'; '\n'; '\r' ]
  |> List.filter ~f:(fun word -> not (String.is_empty word))
  |> String.concat ~sep:" "
;;

let apply_lens ?(path = "test.mli") ?(enable = true) source =
  let uri = DocumentUri.of_path path in
  let request =
    CodeLensParams.create ~textDocument:(TextDocumentIdentifier.create ~uri) ()
  in
  let%map lenses =
    Test.run_request
      ~prep:(fun client -> Test.openDocument ~client ~uri ~source)
      ~settings:(`Assoc [ "deoxidizeLens", `Assoc [ "enable", `Bool enable ] ])
      (TextDocumentCodeLens request)
  in
  let lenses =
    match lenses with
    | None ->
      print_endline "(no code lens response)";
      []
    | Some [] ->
      print_endline "(no code lenses)";
      []
    | Some lenses -> lenses
  in
  let print_lens ~indent (lens : CodeLens.t) =
    let title =
      match lens.command with
      | Some command -> command.title
      | None -> "<no command>"
    in
    print_endline (indent ^ lens_prefix ^ render_title title)
  in
  String.split_lines source
  |> List.iteri ~f:(fun line_number line ->
    (* Line up the lens with the declaration it belongs to. *)
    let indent = String.take_while line ~f:(fun c -> Char.equal c ' ') in
    List.iter lenses ~f:(fun (lens : CodeLens.t) ->
      if lens.range.start.line = line_number then print_lens ~indent lens);
    print_endline line)
;;

let%expect_test "deoxidize lens disabled" =
  let%map () =
    apply_lens
      ~enable:false
      {ocaml|
val f : int -> int @@ portable
|ocaml}
  in
  [%expect
    {|
    (no code lenses)

    val f : int -> int @@ portable
    |}]
;;

let%expect_test "vals" =
  let source =
    {ocaml|
val id : 'a -> 'a

val map : 'a list @ local -> f:('a -> 'b) @ local -> 'b list

val make : ('a : immediate). 'a -> 'a list @@ portable

val partially_annotated : 'a ('b : immediate). 'a -> 'b -> 'a

external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    val id : 'a -> 'a

    >> val map : 'a list -> f:('a -> 'b) -> 'b list
    val map : 'a list @ local -> f:('a -> 'b) @ local -> 'b list

    >> val make : 'a . 'a -> 'a list
    val make : ('a : immediate). 'a -> 'a list @@ portable

    >> val partially_annotated : 'a 'b . 'a -> 'b -> 'a
    val partially_annotated : 'a ('b : immediate). 'a -> 'b -> 'a

    external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
    |}]
;;

let%expect_test "types and exceptions" =
  let source =
    {ocaml|
type ('a : immediate) t : value mod portable

type r = { x : int } [@@deriving sexp_of]

type s = { y : int Modes.Global.t [@globalized] }

type 'a u = Leaf of 'a | Node : ('b : immediate). 'b -> 'a u

exception E : ('a : immediate). 'a -> exn

type q = ..

type q += Q : ('a : immediate). 'a -> q
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    >> type 'a t
    type ('a : immediate) t : value mod portable

    type r = { x : int } [@@deriving sexp_of]

    type s = { y : int Modes.Global.t [@globalized] }

    >> type 'a u = | Leaf of 'a | Node: 'b . 'b -> 'a u
    type 'a u = Leaf of 'a | Node : ('b : immediate). 'b -> 'a u

    >> exception E: 'a . 'a -> exn
    exception E : ('a : immediate). 'a -> exn

    type q = ..

    >> type q += | Q: 'a . 'a -> q
    type q += Q : ('a : immediate). 'a -> q
    |}]
;;

let%expect_test "attributes and doc comments" =
  let source =
    {ocaml|
(** A doc comment alone shouldn't produce a lens. *)
val documented : int -> int

(** A doc comment shouldn't be repeated in the lens. *)
val documented_and_annotated : int -> int @@ portable

val zero_alloc_fn : int -> int [@@zero_alloc]

val zero_alloc_and_portable : int -> int @@ portable [@@zero_alloc]

type t : immutable_data = { foo : int [@sexp.opaque] } [@@deriving sexp]
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    (** A doc comment alone shouldn't produce a lens. *)
    val documented : int -> int

    (** A doc comment shouldn't be repeated in the lens. *)
    >> val documented_and_annotated : int -> int
    val documented_and_annotated : int -> int @@ portable

    val zero_alloc_fn : int -> int [@@zero_alloc]

    >> val zero_alloc_and_portable : int -> int
    val zero_alloc_and_portable : int -> int @@ portable [@@zero_alloc]

    >> type t = { foo: int }
    type t : immutable_data = { foo : int [@sexp.opaque] } [@@deriving sexp]
    |}]
;;

let%expect_test "modules and module types" =
  let source =
    {ocaml|
module M : sig
  val f : int -> int @@ portable

  val g : int -> int
end

module P : sig @@ portable
  val p : int
end

module type S = sig
  type t : immutable_data

  val h : t -> t
end

module F (X : S) : sig
  val i : X.t -> X.t @@ portable
end

module G (X : sig
    val j : int @@ portable
  end) : S

include S @@ portable

include sig
  val k : int -> int @@ portable
end
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    module M : sig
      >> val f : int -> int
      val f : int -> int @@ portable

      val g : int -> int
    end

    >> module P : sig
    module P : sig @@ portable
      val p : int
    end

    module type S = sig
      >> type t
      type t : immutable_data

      val h : t -> t
    end

    module F (X : S) : sig
      >> val i : X.t -> X.t
      val i : X.t -> X.t @@ portable
    end

    module G (X : sig
        >> val j : int
        val j : int @@ portable
      end) : S

    >> include S
    include S @@ portable

    include sig
      >> val k : int -> int
      val k : int -> int @@ portable
    end
    |}]
;;

let%expect_test "ppx_template attributes" =
  let source =
    {ocaml|
val%template zero : unit -> 'a [@@kind k = (value, bits64)]

val%template apply : ('a -> 'b) @ m -> 'a -> 'b [@@mode m = (global, local)]

val%template id : 'a -> 'a [@@modality p = (portable, nonportable)]

val%template box : 'a -> 'a ref [@@alloc a = (heap, stack)]

val%template cmp : 'a -> 'a -> int [@@kind_set ks = (value, bits64)]

val mono_attributes : ('a list[@kind bits64]) -> ('a[@mode local]) -> unit
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    >> val zero : unit -> 'a
    val%template zero : unit -> 'a [@@kind k = (value, bits64)]

    >> val apply : ('a -> 'b) -> 'a -> 'b
    val%template apply : ('a -> 'b) @ m -> 'a -> 'b [@@mode m = (global, local)]

    >> val id : 'a -> 'a
    val%template id : 'a -> 'a [@@modality p = (portable, nonportable)]

    >> val box : 'a -> 'a ref
    val%template box : 'a -> 'a ref [@@alloc a = (heap, stack)]

    >> val cmp : 'a -> 'a -> int
    val%template cmp : 'a -> 'a -> int [@@kind_set ks = (value, bits64)]

    >> val mono_attributes : 'a list -> 'a -> unit
    val mono_attributes : ('a list[@kind bits64]) -> ('a[@mode local]) -> unit
    |}]
;;

let%expect_test "ppx_template blocks and floating attributes" =
  let source =
    {ocaml|
[%%template:
val succ : int -> int [@@mode m = (global, local)]

type nothing_to_strip = int]

[%%template:
[@@@kind.default k = (value, bits64)]

val no_annotations_of_its_own : int -> int]
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    [%%template:
    >> val succ : int -> int
    val succ : int -> int [@@mode m = (global, local)]

    type nothing_to_strip = int]

    [%%template:
    [@@@kind.default k = (value, bits64)]

    val no_annotations_of_its_own : int -> int]
    |}]
;;

let%expect_test "ppx_template modules and functors" =
  let source =
    {ocaml|
module%template M : sig
  val f : ('a : k). 'a -> 'a [@@kind k = (value, bits64)]
end
[@@kind k = (value, bits64)]

module%template F (X : S) : sig
  val g : X.t -> X.t
end
[@@mode m = (global, local)]

module type%template T = sig
  val h : 'a -> 'a
end
[@@modality p = (portable, nonportable)]

include%template S [@@mode m = (global, local)]

module%template.portable N : sig
  val i : int -> int
end
|ocaml}
  in
  let%map () = apply_lens source in
  (* Q: Why doesn't [module%template.portable N : sig] get a codelens?

     A: It's because [module%template.portable N : sig] uses an "extension" node rather
     than an "attribute" node, and stripping out extention nodes is a more difficult
     problem than attributes. The other cases where %template gets stripped are because
     we're already stripping out other attributes. With some effort I could fix this case,
     but I didn't think it's worth it because %template.portable is a fairly simple
     annotation that's easy to read. *)
  [%expect
    {|
    >> module M : sig
    module%template M : sig
      >> val f : 'a . 'a -> 'a
      val f : ('a : k). 'a -> 'a [@@kind k = (value, bits64)]
    end
    [@@kind k = (value, bits64)]

    >> module F : (X : S) -> sig
    module%template F (X : S) : sig
      val g : X.t -> X.t
    end
    [@@mode m = (global, local)]

    >> module type T = sig
    module type%template T = sig
      val h : 'a -> 'a
    end
    [@@modality p = (portable, nonportable)]

    >> include S
    include%template S [@@mode m = (global, local)]

    module%template.portable N : sig
      val i : int -> int
    end
    |}]
;;

let%expect_test "multi-line declarations" =
  let source =
    {ocaml|
type ('a : immediate) long_variant =
  | First_constructor of { first_field : int list; second_field : string option }
  | Second_constructor of int * string * float * bool
  | Third_constructor : ('b : value). 'b list -> 'a long_variant

type long_record =
  { a_function_field : int -> string -> float -> unit -> int @@ portable
  ; another_field : int list
  ; a_third_field : string option
  }

type%template ('a : k) templated_record =
  { boxed_contents : 'a
  ; unboxed_contents : 'a
  ; a_longer_field_name_to_force_wrapping : 'a option
  }
[@@kind k = (value, bits64)]
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    >> type 'a long_variant = | First_constructor of { first_field: int list ; second_field: string option } | Second_constructor of int * string * float * bool | Third_constructor: 'b . 'b list -> 'a long_variant
    type ('a : immediate) long_variant =
      | First_constructor of { first_field : int list; second_field : string option }
      | Second_constructor of int * string * float * bool
      | Third_constructor : ('b : value). 'b list -> 'a long_variant

    >> type long_record = { a_function_field: int -> string -> float -> unit -> int ; another_field: int list ; a_third_field: string option }
    type long_record =
      { a_function_field : int -> string -> float -> unit -> int @@ portable
      ; another_field : int list
      ; a_third_field : string option
      }

    >> type 'a templated_record = { boxed_contents: 'a ; unboxed_contents: 'a ; a_longer_field_name_to_force_wrapping: 'a option }
    type%template ('a : k) templated_record =
      { boxed_contents : 'a
      ; unboxed_contents : 'a
      ; a_longer_field_name_to_force_wrapping : 'a option
      }
    [@@kind k = (value, bits64)]
    |}]
;;

let%expect_test "functor with multiline args" =
  let source =
    {ocaml|
module%template Make (M : sig
    type t
    val foo : t @ m -> int
  end) (N : sig
    type t
    val bar : t @ m -> int
  end) : sig
  val baz : M.t @ m -> int
end
[@@mode m = (portable, nonportable)]
|ocaml}
  in
  let%map () = apply_lens source in
  [%expect
    {|
    >> module Make : (M : <sig>) -> (N : <sig>) -> sig
    module%template Make (M : sig
        type t
        >> val foo : t -> int
        val foo : t @ m -> int
      end) (N : sig
        type t
        >> val bar : t -> int
        val bar : t @ m -> int
      end) : sig
      >> val baz : M.t -> int
      val baz : M.t @ m -> int
    end
    [@@mode m = (portable, nonportable)]
    |}]
;;

let%expect_test "signatures within an ml file" =
  let source =
    {ocaml|
module M : sig
  val f : int -> int @@ portable

  val g : int -> int
end = struct
  let f x = x
  let g x = x
end

module type S = sig @@ portable
  type t : immutable_data

  val h : t -> t
end

module Nested = struct
  module N : sig
    val i : int -> int @@ portable
  end = struct
    let i x = x
  end
end

module%template T : sig
  val j : 'a -> 'a
  [@@kind k = (value, bits64)]
end = struct
  let j x = x
  [@@kind k = (value, bits64)]
end
[@@kind k = (value, bits64)]

let nothing_to_see_here : int = 5
|ocaml}
  in
  let%map () = apply_lens ~path:"test.ml" source in
  [%expect
    {|
    module M : sig
      >> val f : int -> int
      val f : int -> int @@ portable

      val g : int -> int
    end = struct
      let f x = x
      let g x = x
    end

    >> module type S = sig
    module type S = sig @@ portable
      >> type t
      type t : immutable_data

      val h : t -> t
    end

    module Nested = struct
      module N : sig
        >> val i : int -> int
        val i : int -> int @@ portable
      end = struct
        let i x = x
      end
    end

    module%template T : sig
      >> val j : 'a -> 'a
      val j : 'a -> 'a
      [@@kind k = (value, bits64)]
    end = struct
      let j x = x
      [@@kind k = (value, bits64)]
    end
    [@@kind k = (value, bits64)]

    let nothing_to_see_here : int = 5
    |}]
;;
