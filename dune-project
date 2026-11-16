(lang dune 3.20)
(name ocaml-lsp-server)

(license ISC)
(maintainers "Jane Street developers")
(authors
 "ocaml-lsp contributors"
 "Jane Street Group, LLC")

(source (github oxcaml/ocaml-lsp))

(generate_opam_files true)

(package
 (name jsonrpc)
 (synopsis "JSON-RPC protocol implementation")
 (depends
  (ocaml (and (>= 5.4) (< 5.5)))
  core
  (ppx_jane :with-test)
  (yojson (<= 3.0))))

(package
 (name lsp)
 (synopsis "LSP protocol implementation in OCaml")
 (depends
  (ocaml (and (>= 5.4) (< 5.5)))
  (jsonrpc (= :version))
  async
  base
  core
  (dyn :with-test)
  file_path
  ppx_bin_prot
  ppx_compare
  (ppx_expect :with-test)
  ppx_js_style
  ppx_let
  ppx_sexp_conv
  ppx_string
  ppx_yojson_conv
  uutf
  yojson))

(package
 (name ocaml-lsp-server)
 (synopsis "OCaml Language Server Protocol implementation")
 (allow_empty)
 (depends
  (ocaml (and (>= 5.4) (< 5.5)))
  async
  base
  cmarkit
  (cppo :build)
  core
  core_unix
  csexp
  (expect_test_helpers_async :with-test)
  (expect_test_helpers_core :with-test)
  file_path
  lrgrep
  merlin-lib
  ocamlc-loc
  ;; Upstream ocaml-lsp-server vendors odoc-parser instead
  (odoc-parser (= 2.0.0))
  ppx_bin_prot
  ppx_compare
  ppx_enumerate
  (ppx_expect :with-test)
  ppx_here
  (ppx_jane :with-test)
  ppx_js_style
  ppx_let
  ppx_log
  ppx_sexp_conv
  ppx_sexp_message
  ppx_sexp_value
  ppx_string
  ppx_yojson_conv
  re
  re2
  spawn
  uutf
  yojson
  ;; This needs to match the Menhir requirement of OxCaml
  (menhir (and :with-test (= 20231231)))))
