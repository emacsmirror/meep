
############
MEEP Hacking
############

Notes for anyone interested to jump into the code.


Code Style
==========

- Code is formatted using `elisp-autofmt <https://codeberg.org/ideasman42/emacs-elisp-autofmt>`__.
- Use ``cond`` instead of ``if``.


Dependencies
============

- No 3rd party dependencies.
- No use of ``cl-lib``.


Conventions
===========

- Interactive functions should accept a numeric arguments where applicable.
- Interactive functions that are directional should be reversed when passing in a negative number.
- Text editing operations should support rectangle regions where possible.
- Reserve ``error`` / ``user-error`` for failure or invalid usage.
  Use ``message`` for commands that need to communicate no changes were performed.

  *So as not to interfere with recording macros or repeating actions.*


Make Targets
============

While MEEP doesn't need to be "built", there are some convenience targets for development.

Note that these targets expect ``../bray`` and ``../repeat-fu`` to be checked out along side MEEP.

Testing
   Tests can be run using ``make test`` or ``make watch_test``.

   Tests can be found under ``tests/``.

Checks
   Checks can be run via ``make check`` or ``make watch_check``.

   `emacs-batch-check <https://codeberg.org/ideasman42/emacs-batch-check>`__ is used for running checks.

Docs
   To re-generate ``docs/reference.rst`` run ``make docs``.
