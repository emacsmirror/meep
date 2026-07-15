# SPDX-License-Identifier: GPL-3.0-or-later

define HELP_TEXT

- doc
  Build the Sphinx manual.

- doc-publish
  Build and publish the manual to Codeberg Pages.

- test, watch_test
  Runs all tests.

- check, watch_check
  Runs all checks.

- format
  Format all Emacs Lisp files.

Environment Variables:

- EMACS_BIN
  The command used to run Emacs, defaults to "emacs"

endef
# HELP_TEXT (end)

# Needed for when tests are run from another directory: `make -C ./path/to/tests`.
BASE_DIR := $(CURDIR)

# -----------------------------------------------------------------------------
# Number of cores, for Sphinx's parallel HTML build.
#
# $(OS) is only pre-set on Windows; derive it from uname elsewhere so the
# per-platform core counts below resolve.

OS ?= $(shell uname -s)

ifndef NPROCS
    NPROCS := 1
    ifeq ($(OS), Linux)
        NPROCS := $(shell nproc)
    endif
    ifeq ($(OS), NetBSD)
        NPROCS := $(shell getconf NPROCESSORS_ONLN)
    endif
    ifneq (,$(filter $(OS),Darwin FreeBSD))
        NPROCS := $(shell sysctl -n hw.ncpu)
    endif
endif

# Default Emacs binary
EMACS_BIN ?= emacs

EL_FILES := \
	./meep.el \
	./meep-region-mark.el \
	./tests/meep_tests.el \
	./tests/meep_tests_internal.el \
	./tests/emacs/init.el

# Check for emacs-batch-check availability.
EMACS_BATCH_CHECK_BIN ?= $(shell which emacs-batch-check)

# Additional files to watch (can be overridden)
EXTRA_WATCH_FILES ?=

# -----------------------------------------------------------------------------
# Help for build targets

export HELP_TEXT
.PHONY: help
help: FORCE
	@echo "$$HELP_TEXT"


# -----------------------------------------------------------------------------
# Manual

# Sphinx manual directories.
DOC_DIR := $(BASE_DIR)/doc/manual
DOC_BUILD_DIR := $(DOC_DIR)/build
DOC_HTML_DIR := $(DOC_BUILD_DIR)/_build/html

# Codeberg Pages publishing. git-pages serves the tip of the `pages` branch at
# PAGES_URL. A Forgejo webhook (repository Settings -> Webhooks, type Forgejo,
# Target URL = PAGES_URL, Branch filter = pages) must be configured once so a
# push to the branch is served instead of ignored.
PAGES_REMOTE ?= origin
PAGES_BRANCH ?= pages
PAGES_URL ?= https://ideasman42.codeberg.page/emacs-meep/

# Generate the RST sources then build the HTML manual with Sphinx.
# `-W` treats Sphinx warnings as errors so a broken manual fails the build.
.PHONY: doc
doc: FORCE
	@cd "$(BASE_DIR)" && \
	python ./doc/manual/sphinx_doc_gen.py --quiet && \
	sphinx-build -W --keep-going --quiet --jobs $(NPROCS) -b html "$(DOC_BUILD_DIR)" "$(DOC_HTML_DIR)"
	@echo "Manual built at: $(DOC_HTML_DIR)/index.html"

# Build the HTML manual and publish it to Codeberg Pages. A freshly built orphan
# commit is force-pushed to $(PAGES_REMOTE)/$(PAGES_BRANCH), so the branch holds
# only the current snapshot. The publish helper strips the Sphinx build cache.
.PHONY: doc-publish
doc-publish: doc
	@cd "$(BASE_DIR)" && \
	python ./_misc/doc_publish.py \
		--html-dir "$(DOC_HTML_DIR)" \
		--remote $(PAGES_REMOTE) \
		--branch $(PAGES_BRANCH)
	@echo "Live at $(PAGES_URL) once the Forgejo webhook fires."

# -----------------------------------------------------------------------------
# Tests

.PHONY: test
test: FORCE
	@cd "$(BASE_DIR)" && \
	python ./tests/meep_tests.py

.PHONY: watch_test
watch_test: FORCE
	@cd "$(BASE_DIR)" && \
	bash -c "while true; do \
		inotifywait -e close_write $(EL_FILES); \
		tput clear && make test; \
	done"

# -----------------------------------------------------------------------------
# Checking Utilities

.PHONY: check
check: FORCE
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self --load-path=../bray --load-path=../melpazoid/melpazoid meep.el

.PHONY: watch_check
watch_check: FORCE
	@cd "$(BASE_DIR)" && \
	while true; do \
		$(MAKE) check; \
		inotifywait -q -e close_write $(EXTRA_WATCH_FILES) $(EL_FILES); \
		tput clear; \
	done

# -----------------------------------------------------------------------------
# Formatting

.PHONY: format
format: FORCE
	@cd "$(BASE_DIR)" && \
	python ./_misc/auto_format.py --jobs $(NPROCS)

FORCE:
