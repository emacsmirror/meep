# SPDX-License-Identifier: GPL-3.0-or-later

define HELP_TEXT

- docs
  Update documentation.

- test, watch_test
  Runs all tests.

- check, watch_check
  Runs all checks.

Environment Variables:

- EMACS_BIN
  The command used to run Emacs, defaults to "emacs"

endef
# HELP_TEXT (end)

# Needed for when tests are run from another directory: `make -C ./path/to/tests`.
BASE_DIR := $(CURDIR)

# Default Emacs binary
EMACS_BIN ?= emacs

EL_FILES := \
	./meep.el \
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
# Maintenance

.PHONY: docs
docs: FORCE
	@cd "$(BASE_DIR)" && \
	python ./_misc/readme_update.py

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

FORCE:
