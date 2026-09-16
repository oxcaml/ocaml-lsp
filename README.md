ocaml-lsp for oxcaml
=================================
This is version of `ocaml-lsp` developed by Jane Street and tailored towards working with `oxcaml`.

Contributing
============

A lifecycle of an OSS contribution to this repo is:
1. a PR is made on github
2. a review happens until the PR is approved
3. the PR is not merged, but rather imported by a Jane Street employee into Jane Steet's internal repository
4. internal validation happens
5. a sync from the internal repository to this repository brings the change into the main
   branch
6. the PR is closed.

Build and test
--------------
To build the repo:
```
$ make buildoss
```

To run the full test suite:
```
$ make runtestoss
```

Installing build and test-time dependencies
-------------------------------------------

When run without any additional overrides, `make buildoss` and `make runtestoss` will
install and build all of their build-time and test-time dependencies automatically. Both
commands also do their best to not re-install/re-build anything that is already available.

In more detail, `make buildoss` will create a local `opam` switch (in the `_opam` subdirectory),
using `ocaml-base-compiler.5.4.1`, then pin `merlin-lib` and install the rest of
`ocaml-lsp.opam` dependencies. `make runtestoss` will do the same, and in addition it will
also build an *oxcaml* standard library under `_oxcaml/_install/lib/ocaml`. This stdlib is
needed (and `ocaml` stdlib cannot be used) because this project *targets* `oxcaml` despite
being *built* in `ocaml`. So the e2e tests contain the source code of the analyzed program, which is `oxcaml` code. And that `oxcaml` code needs access to `oxcaml` stdlib to work properly.

Both `merlin-lib` pinning and the `oxcaml` checkout used to build the stdlib use the
commit stored in `oxcaml_merlin_hash`.

Local `opam` switch creation can be disabled by supplying the `OSS_DEPS_ALREADY_INSTALLED`
env var: with it set the makefile expects to be running in the appropriate opam switch environment.

`oxcaml` stdlib building can be disabled by supplying the `oxcaml` installation path via
`OXCAML_INSTALL`. It is the caller's responsibility to ensure that the installation is
of a version compatible with the pinned `merlin-lib`.
