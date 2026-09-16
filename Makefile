# Where OSS build should put its artifacts
BUILD_DIR ?= _build_oss

# Which version of the `ocaml` (*not* `oxcaml`) compiler to use to build this version of `ocaml-lsp`
OCAML_COMPILER ?= ocaml-base-compiler.5.4.1

# Where oxcaml and merlin live
OXCAML_REPO ?= https://github.com/oxcaml/oxcaml.git

# Which commit of $OXCAML_REPO to get Merlin from
OXCAML_MERLIN_HASH ?= $(shell cat oxcaml_merlin_hash)

MERLIN_PIN ?= git+$(OXCAML_REPO)\#$(OXCAML_MERLIN_HASH)

# Where Merlin lives in $OXCAML_REPO
MERLIN_SUBPATH ?= external/merlin

# When OXCAML_INSTALL is not supplied, and we're building our own OX_STDLIB, everything
# OxCaml-compiler-related lives under $(OXCAML_DIR): the compiler is cloned
# and built in $(OXCAML_DIR)/oxcaml (removed once installed), installed with
# $(OXCAML_DIR)/_install as its --prefix, and stamped with $(OXCAML_DIR)/HASH.
OXCAML_DIR ?= _oxcaml

# Optionally set this to the installation prefix of a pre-built OxCaml compiler (a
# directory containing lib/ocaml). When set, [ox-stdlib] builds nothing and the tests use
# that installation's stdlib (`$(OXCAML_INSTALL)/lib/ocaml`) instead. When supplying this,
# it is the caller's responsibility to ensure that `$OXCAML_INSTALL` was built from the a
# version of the codebase compatible with the pinned merlin version.
OXCAML_INSTALL ?=

# Name of the `autoconf` binary. Only necessary if it is not `autoconf` and if the oxcaml
# stdlib is being built (so not necessary for `buildoss`, but necessary for `runtestoss`)
AUTOCONF ?= autoconf # Note: this has to be autoconf >= 2.71

ifeq ($(OXCAML_INSTALL),)
OX_STDLIB = $(abspath $(OXCAML_DIR))/_install/lib/ocaml
else
OX_STDLIB = $(OXCAML_INSTALL)/lib/ocaml
endif

export DUNE_BUILD_DIR := $(BUILD_DIR)

# Supplying `OSS_DEPS_ALREADY_INSTALLED` makes this makefile not build a local opam switch
ifndef OSS_DEPS_ALREADY_INSTALLED
OSS_DEPS_PREREQUISITE := oss-deps
OPAM_ENV := eval "$$(opam --cli=2.5 env)";
endif

.PHONY: oss-deps buildoss runtestoss ox-stdlib cleanoss deepcleanoss

cleanoss:
	rm -rf $(BUILD_DIR)

deepcleanoss: cleanoss
	rm -rf _opam $(OXCAML_DIR)

oss-deps:
	@if [ ! -d _opam ]; then \
		opam --cli=2.5 switch create . $(OCAML_COMPILER) --no-install --yes; \
	fi
	eval "$$(opam --cli=2.5 env)"; \
	opam --cli=2.5 pin add merlin-lib "$(MERLIN_PIN)" --subpath="$(MERLIN_SUBPATH)" --no-action --yes; \
	python3 -c 'from pathlib import Path; p = Path("_opam/.opam-switch/overlay/merlin-lib/opam"); p.write_text(p.read_text().replace("  [\"dune\" \"subst\"] {dev}\n", ""))'; \
	opam --cli=2.5 install merlin-lib --skip-updates --yes; \
	opam --cli=2.5 install ./ocaml-lsp.opam --deps-only --with-test --skip-updates --yes

buildoss: $(OSS_DEPS_PREREQUISITE)
	$(OPAM_ENV) dune build --root .

# The merlin-lib we pin comes from the oxcaml repo, so it cannot read `.cmi`s produced by
# the `ocaml` compiler. Without an `oxcaml`-built stdlib from the same revision, no
# identifier from the standard library resolves in the tests.

ox-stdlib: $(OSS_DEPS_PREREQUISITE)
	@set -e; \
	if [ -n "$(OXCAML_INSTALL)" ]; then \
		echo "Using pre-built OxCaml install at $(OXCAML_INSTALL)"; \
	else \
		oxcaml_hash="$(OXCAML_MERLIN_HASH)"; \
		if [ -f $(OXCAML_DIR)/HASH ] \
		  && [ "$$(cat $(OXCAML_DIR)/HASH)" = "$$oxcaml_hash" ]; then \
			echo "OxCaml stdlib for $$oxcaml_hash already present in $(OXCAML_DIR)"; \
		else \
			autoconf_version=$$($(AUTOCONF) --version | sed -n '1s/.* \([0-9][0-9.]*\)$$/\1/p'); \
			if [ "$$(printf '%s\n' 2.71 "$$autoconf_version" | sort -V | head -n 1)" != "2.71" ]; then \
				echo "error: autoconf >= 2.71 is required to build the OxCaml stdlib" \
				  "(found $$autoconf_version). Maybe try: make ox-stdlib AUTOCONF=autoconf27"; \
				exit 1; \
			fi; \
			rm -rf $(OXCAML_DIR); \
			mkdir -p $(OXCAML_DIR)/oxcaml; \
			git -C $(OXCAML_DIR)/oxcaml init -q; \
			git -C $(OXCAML_DIR)/oxcaml remote add origin $(OXCAML_REPO); \
			git -C $(OXCAML_DIR)/oxcaml fetch --depth 1 origin "$$oxcaml_hash"; \
			git -C $(OXCAML_DIR)/oxcaml checkout -q FETCH_HEAD; \
			$(OPAM_ENV) \
			cd $(OXCAML_DIR)/oxcaml; \
			$(AUTOCONF); \
			./configure --enable-dev --prefix="$(abspath $(OXCAML_DIR))/_install"; \
			env -u DUNE_BUILD_DIR $(MAKE) install; \
			cd "$(abspath .)"; \
			echo "$$oxcaml_hash" > $(OXCAML_DIR)/HASH; \
			rm -rf $(OXCAML_DIR)/oxcaml; \
		fi; \
	fi

runtestoss: $(OSS_DEPS_PREREQUISITE) ox-stdlib
	$(OPAM_ENV) \
	OCAMLLSP_STDLIB="$(OX_STDLIB)" \
	dune runtest --display=short --root .
