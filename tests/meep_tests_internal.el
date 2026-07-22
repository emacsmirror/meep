;;; meep_tests_internal.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-meep
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Tests for internal MEEP API's.

;;; Usage

;; Run:
;; `emacs -batch --script meep_tests_internal.el -f ert-run-tests-batch-and-exit'

;;; Code:

(defvar my-meep-load-path (file-name-concat (file-name-directory load-file-name) ".."))

(add-to-list 'load-path my-meep-load-path)

(require 'ert)
(require 'meep)

(defmacro meep-test--with-advice (advice &rest body)
  "Execute BODY with ADVICE temporarily enabled.

ADVICE is a list of `(SYMBOL HOW FUNCTION)' triplets;
see `advice-add' for HOW values."
  (declare (indent 1))
  (let ((body-let nil)
        (body-advice-add nil)
        (body-advice-remove nil)
        (item nil)
        (advice-list advice))
    (unless (listp advice-list)
      (error "Advice must be a list"))
    (when (null advice-list)
      (error "Advice must be a list containing at least one item"))
    (while (setq item (pop advice-list))
      (unless (and (listp item) (eq 3 (length item)))
        (error "Each advice must be a list of 3 items"))
      (let ((fn-sym (gensym))
            (fn-advise (pop item))
            (fn-advice-ty (pop item))
            (fn-body (pop item)))
        (push (list fn-sym fn-body) body-let)
        (push (list 'advice-add fn-advise fn-advice-ty fn-sym) body-advice-add)
        (push (list 'advice-remove fn-advise fn-sym) body-advice-remove)))
    (setq body-let (nreverse body-let))
    (setq body-advice-add (nreverse body-advice-add))
    `(let ,body-let
       (unwind-protect
           (progn
             ,@body-advice-add
             ,@body)
         ,@body-advice-remove))))

(defmacro meep-test--with-call-limit (fn limit &rest body)
  "Run BODY, erroring once FN has been called more than LIMIT times.

Turns a runaway loop into a failure instead of a hang.  `with-timeout'
cannot do this: the loops guarded here read no input, so their timer
never gets a chance to fire & the whole test run stops instead of one
test reporting.  Measured, not assumed - both loops were left looping to
check which way it goes."
  (declare (indent 2))
  (let ((count-sym (gensym "count-")))
    `(let ((,count-sym 0))
       (meep-test--with-advice ((,fn
                                 :before
                                 (lambda (&rest _)
                                   (setq ,count-sym (1+ ,count-sym))
                                   (when (> ,count-sym ,limit)
                                     (error "%S called over %d times, loop did not terminate"
                                            ,fn
                                            ,limit)))))
         ,@body))))

;; Comment-bounds tests enable many major modes, whose setup does unrelated
;; work that prints in batch.  Turn the work itself off rather than muffling
;; the messages - indent guessing, schema validation & localized indent vars
;; are all unused here.  Each mode consults its variable in the mode body,
;; before any mode hook runs, so a hook can never disable the work in time;
;; the `with-meep-mode-defaults-for-*' macros bind it around the test
;; instead.  Each macro requires its library before binding: a `let' of a
;; variable whose `defcustom' has yet to load is a lexical shadow the mode
;; never sees, & first loading the library inside the binding writes the
;; default over it (both observed, not assumed).
;;
;; `sh-set-shell' & `vhdl-mode' print with no variable behind the message
;; at all - only those exact messages are suppressed, by matching their
;; format string, see `with-meep-message-suppress'.

(defmacro with-meep-message-suppress (messages &rest body)
  "Run BODY with `message' calls matching MESSAGES suppressed.
MESSAGES is a list of format strings, compared against the literal
FORMAT argument rather than the formatted result - the result embeds
values that vary by machine (a shell type tracks $SHELL, a version
banner its release), where the format string is exactly what the
printing code passes.  A suppressed call still returns the formatted
string for callers that use it; any other message prints as normal."
  (declare (indent 1))
  `(meep-test--with-advice ((#'message
                             :around
                             (lambda (fn-orig &rest args)
                               (cond
                                ((member (car args) ,messages)
                                 (apply #'format-message args))
                                (t
                                 (apply fn-orig args))))))
     ,@body))

(defmacro with-meep-mode-defaults-for-python (&rest body)
  "Run BODY with `python-mode' set up not to guess the indent offset."
  (declare (indent 0))
  `(progn
     (require 'python)
     (let ((python-indent-guess-indent-offset nil))
       ,@body)))

(defmacro with-meep-mode-defaults-for-nxml (&rest body)
  "Run BODY with `nxml-mode' set up not to validate against a schema."
  (declare (indent 0))
  `(progn
     (require 'rng-nxml)
     (let ((rng-nxml-auto-validate-flag nil))
       ,@body)))

(defmacro with-meep-mode-defaults-for-sh (&rest body)
  "Run BODY with `sh-mode' set up not to localize the indent variables.
The `sh-set-shell' indent setup messages print in both branches with no
variable behind them, suppress those exact messages."
  (declare (indent 0))
  `(progn
     (require 'sh-script)
     (let ((sh-make-vars-local nil))
       (with-meep-message-suppress '("Setting up indent for shell type %s"
                                     "Indentation setup for shell type %s")
         ,@body))))

(defmacro with-meep-mode-defaults-for-vhdl (&rest body)
  "Run BODY with `vhdl-mode's version banner suppressed.
The banner ends the mode's own setup & no variable turns it off."
  (declare (indent 0))
  `(with-meep-message-suppress '("VHDL Mode %s.%s")
     ,@body))

(defmacro with-meep-mode-defaults (&rest body)
  "Run BODY with every mode's unrelated setup above disabled.
For a test sweeping many modes as data rather than exercising one."
  (declare (indent 0))
  `(with-meep-mode-defaults-for-python
     (with-meep-mode-defaults-for-nxml
       (with-meep-mode-defaults-for-sh
         (with-meep-mode-defaults-for-vhdl
           ,@body)))))

(meep-bootstrap-once)

;; ---------------------------------------------------------------------------
;; P-list Remove

(ert-deftest plist-remove-empty ()
  "Removing from an empty P-list should return nil."
  (should (equal (meep--plist-remove nil :foo) nil)))

(ert-deftest plist-remove-missing-key ()
  "Removing a key not present should return the original P-list."
  (let ((plist '(:a 1 :b 2)))
    (should (equal (meep--plist-remove plist :c) plist))))

(ert-deftest plist-remove-first-key ()
  "Removing the first key should drop the first key-value pair."
  (should (equal (meep--plist-remove '(:a 1 :b 2 :c 3) :a) '(:b 2 :c 3))))

(ert-deftest plist-remove-middle-key ()
  "Removing a key in the middle should drop that key-value pair."
  (should (equal (meep--plist-remove '(:a 1 :b 2 :c 3) :b) '(:a 1 :c 3))))

(ert-deftest plist-remove-last-key ()
  "Removing the last key should drop the last key-value pair."
  (should (equal (meep--plist-remove '(:a 1 :b 2) :b) '(:a 1))))

(ert-deftest plist-remove-multiple-occurrences ()
  "If the key appears multiple times, only the first occurrence is removed."
  (should (equal (meep--plist-remove '(:a 1 :b 2 :a 3) :a) '(:b 2 :a 3))))


;; ---------------------------------------------------------------------------
;; Region list overlap checks

(ert-deftest ranges-overlap-p-basic-overlap ()
  "Ranges clearly overlap."
  (should (meep--ranges-overlap-p '((1 . 5)) '((3 . 7))))
  (should (meep--ranges-overlap-p '((3 . 7)) '((1 . 5)))))

(ert-deftest ranges-overlap-p-no-overlap ()
  "Ranges are disjoint."
  (should-not (meep--ranges-overlap-p '((1 . 5)) '((6 . 10))))
  (should-not (meep--ranges-overlap-p '((6 . 10)) '((1 . 5)))))

(ert-deftest ranges-overlap-p-touching-edges ()
  "Half-open ranges that touch at an edge do not overlap.
END is the position past the range, so [1 5) and [5 8) share no position,
while [1 6) and [5 8) do (position 5)."
  (should-not (meep--ranges-overlap-p '((1 . 5)) '((5 . 8))))
  (should-not (meep--ranges-overlap-p '((5 . 8)) '((1 . 5))))
  (should (meep--ranges-overlap-p '((1 . 6)) '((5 . 8))))
  (should (meep--ranges-overlap-p '((5 . 8)) '((1 . 6)))))

(ert-deftest ranges-overlap-p-multiple-ranges ()
  "Overlap occurs when any pair of ranges intersect."
  (should (meep--ranges-overlap-p '((1 . 2) (10 . 15)) '((3 . 4) (14 . 20))))
  (should-not (meep--ranges-overlap-p '((1 . 2) (5 . 6)) '((3 . 4) (7 . 8)))))

(ert-deftest ranges-overlap-p-empty-lists ()
  "Empty lists never overlap."
  (should-not (meep--ranges-overlap-p nil nil))
  (should-not (meep--ranges-overlap-p '((1 . 5)) nil))
  (should-not (meep--ranges-overlap-p nil '((1 . 5)))))

(ert-deftest ranges-overlap-p-single-point-ranges ()
  "Zero-width ranges (START == END) are empty and overlap nothing."
  (should-not (meep--ranges-overlap-p '((5 . 5)) '((5 . 5))))
  (should-not (meep--ranges-overlap-p '((5 . 5)) '((4 . 5))))
  (should-not (meep--ranges-overlap-p '((5 . 5)) '((5 . 6)))))


;; ---------------------------------------------------------------------------
;; Preset Loader

(defmacro with-meep-preset-test (mode &rest body)
  "Run BODY in a fresh temp buffer with `major-mode' set to MODE.
MODE is an unquoted mode symbol.  The preset cache entry for MODE
is cleared before and after BODY.  Using a fresh buffer keeps
each test's buffer-local state isolated."
  (declare (indent 1))
  `(progn
     (remhash ',mode meep--preset-cache)
     (unwind-protect
         (with-temp-buffer
           (setq-local major-mode ',mode)
           ,@body)
       (remhash ',mode meep--preset-cache))))

(defmacro with-meep-mock-preset (mode preset-body &rest body)
  "Run BODY with `meep-preset-MODE' defined to evaluate PRESET-BODY.

MODE is an unquoted mode symbol (e.g. `meep-test-foo-mode'); the
defined function is named `meep-preset-MODE' and `require' is
mocked to return t for that symbol - so the loader sees the
preset as already loaded.  PRESET-BODY is a single form that the
function evaluates (use `progn' or `let' for multiple forms).
`lwarn' is silenced for the duration of BODY.

On exit the function is `fmakunbound'-ed and the advice is
removed."
  (declare (indent 2))
  (let ((preset-fn-sym (intern (concat "meep-preset-" (symbol-name mode)))))
    `(progn
       (defun ,preset-fn-sym ()
         ,preset-body)
       (unwind-protect
           (meep-test--with-advice ((#'require
                                     :around
                                     (lambda (orig &rest args)
                                       (cond
                                        ((eq (car args) ',preset-fn-sym)
                                         t)
                                        (t
                                         (apply orig args)))))
                                    (#'lwarn :override (lambda (&rest _) nil)))
             ,@body)
         (fmakunbound ',preset-fn-sym)))))

(defmacro with-meep-derived-mode-parent (child parent &rest body)
  "Run BODY with CHILD's `derived-mode-parent' set to PARENT.
CHILD and PARENT are unquoted mode symbols.  The property is
cleared on exit, even if BODY signals."
  (declare (indent 2))
  `(progn
     (put ',child 'derived-mode-parent ',parent)
     (unwind-protect
         (progn
           ,@body)
       (put ',child 'derived-mode-parent nil))))

(defmacro with-meep-derived-mode-extra-parents (child parents &rest body)
  "Run BODY with CHILD's `derived-mode-extra-parents' set to PARENTS.
CHILD is an unquoted mode symbol, PARENTS an unquoted list of mode
symbols (as registered by `derived-mode-add-parents').  The property
is cleared on exit, even if BODY signals."
  (declare (indent 2))
  `(progn
     (put ',child 'derived-mode-extra-parents ',parents)
     (unwind-protect
         (progn
           ,@body)
       (put ',child 'derived-mode-extra-parents nil))))

(ert-deftest meep-preset-ensure-missing-returns-nil ()
  "A mode with no preset returns nil; cache stores `t' (cached-missing-state)."
  (with-meep-preset-test meep-test-no-such-mode
    (should (null (meep-preset-ensure)))
    (should (eq t (gethash 'meep-test-no-such-mode meep--preset-cache)))))

(ert-deftest meep-preset-ensure-bundled-preset-name-matches-mode ()
  "Every bundled preset is named for a real mode, not one of its aliases.
The lookup keys off `major-mode', so a preset named for an alias is never
found - `meep-preset-xml-mode' was dead because `xml-mode' is only an alias
of `nxml-mode'.  A mode that is not installed cannot be checked, skip it."
  (let ((filenames (directory-files my-meep-load-path nil "\\`meep-preset-.*\\.el\\'"))
        (checked 0))
    ;; The presets are bundled, an empty list means the glob stopped matching.
    (should filenames)
    (dolist (filename filenames)
      (save-match-data
        (should (string-match "\\`meep-preset-\\(.*\\)\\.el\\'" filename))
        (let ((mode (intern (match-string 1 filename))))
          (when (fboundp mode)
            ;; An autoloaded mode's `symbol-function' is the autoload object,
            ;; not the alias it resolves to - the check passed over `org-mode',
            ;; `ruby-mode', `scheme-mode' & three more without looking at
            ;; anything.  Load it so there is a definition to judge.
            (autoload-do-load (symbol-function mode) mode)
            (setq checked (1+ checked))
            (should-not (symbolp (symbol-function mode)))))))
    ;; A mode that is not installed is skipped, so the count is what says the
    ;; test still looks at something - without it every mode could go missing
    ;; & this would pass having verified nothing.  Only the third-party modes
    ;; are ever absent, the rest ship with Emacs.
    (should (> checked 6))))

(ert-deftest meep-preset-ensure-missing-short-circuits ()
  "A second `meep-preset-ensure' call does not re-attempt `require'."
  (with-meep-preset-test meep-test-no-such-mode-2
    (should (null (meep-preset-ensure)))
    (let ((require-call-count 0))
      (meep-test--with-advice ((#'require
                                :before
                                (lambda (&rest _)
                                  (setq require-call-count (1+ require-call-count)))))
        (should (null (meep-preset-ensure)))
        (should (eq 0 require-call-count))))))

(ert-deftest meep-preset-ensure-returns-cached-alist ()
  "A pre-cached alist is returned without re-loading."
  (with-meep-preset-test meep-test-cached-mode
    (let ((alist '((meep-test--var-a . 11) (meep-test--var-b . 22)))
          (require-call-count 0))
      (puthash 'meep-test-cached-mode alist meep--preset-cache)
      (meep-test--with-advice ((#'require
                                :before
                                (lambda (&rest _)
                                  (setq require-call-count (1+ require-call-count)))))
        (should (equal alist (meep-preset-ensure)))
        (should (eq 0 require-call-count))))))

(ert-deftest meep-preset-ensure-invokes-preset-function-once ()
  "The preset function runs once per mode; the alist is memoized."
  (with-meep-preset-test meep-test-once-mode
    (let ((call-count 0))
      (with-meep-mock-preset meep-test-once-mode
          (progn
            (setq call-count (1+ call-count))
            '((meep-test--var-x . 99)))
        (should (equal '((meep-test--var-x . 99)) (meep-preset-ensure)))
        (should (eq 1 call-count))
        (should (equal '((meep-test--var-x . 99)) (meep-preset-ensure)))
        (should (eq 1 call-count))))))

(defvar meep-test--applied-var nil)
(defvar meep-test--respected-var nil)

(ert-deftest meep-preset-ensure-applies-alist-buffer-locally ()
  "Alist entries are set buffer-locally."
  (with-meep-preset-test meep-test-apply-mode
    (puthash 'meep-test-apply-mode '((meep-test--applied-var . :from-preset)) meep--preset-cache)
    (should-not (local-variable-p 'meep-test--applied-var))
    (should (meep-preset-ensure))
    (should (local-variable-p 'meep-test--applied-var))
    (should (eq :from-preset meep-test--applied-var))))

(ert-deftest meep-preset-ensure-respects-existing-buffer-local ()
  "If a variable is already buffer-local, its value is not overwritten."
  (with-meep-preset-test meep-test-respect-mode
    (puthash
     'meep-test-respect-mode '((meep-test--respected-var . :from-preset)) meep--preset-cache)
    (setq-local meep-test--respected-var :user-set)
    (should (meep-preset-ensure))
    (should (eq :user-set meep-test--respected-var))))

(ert-deftest meep-preset-ensure-walks-derived-mode-parent ()
  "When the current mode has no preset but an ancestor does, the
ancestor's preset is used; the result is cached under the original
`major-mode'."
  (with-meep-derived-mode-parent meep-test-child-mode meep-test-parent-mode
    (with-meep-preset-test meep-test-child-mode
      (with-meep-mock-preset meep-test-parent-mode '((meep-test--inherited-var . :from-parent))
        ;; Child preset file is genuinely absent - real `require'
        ;; with noerror returns nil, so no extra mock is needed.
        (should (equal '((meep-test--inherited-var . :from-parent)) (meep-preset-ensure)))
        (should
         (equal
          '((meep-test--inherited-var . :from-parent))
          (gethash 'meep-test-child-mode meep--preset-cache)))))))

(ert-deftest meep-preset-ensure-falls-back-to-extra-parents ()
  "When neither the mode nor its direct-parent chain has a preset, an
`derived-mode-extra-parents' preset is used as a fallback - this is
how a tree-sitter mode reaches its classic mode's preset (e.g.
`c++-ts-mode' registers `c++-mode')."
  (with-meep-derived-mode-extra-parents meep-test-ts-child-mode (meep-test-classic-mode)
    (with-meep-preset-test meep-test-ts-child-mode
      (with-meep-mock-preset meep-test-classic-mode '((meep-test--inherited-var . :from-extra))
        (should (equal '((meep-test--inherited-var . :from-extra)) (meep-preset-ensure)))))))

(ert-deftest meep-preset-ensure-direct-parent-beats-extra-parent ()
  "The direct `derived-mode-parent' preset is preferred over an
`derived-mode-extra-parents' preset present at the same level."
  (with-meep-derived-mode-parent meep-test-prio-child-mode meep-test-prio-direct-mode
    (with-meep-derived-mode-extra-parents meep-test-prio-child-mode (meep-test-prio-extra-mode)
      (with-meep-preset-test meep-test-prio-child-mode
        (with-meep-mock-preset meep-test-prio-direct-mode '((meep-test--prio-var . :direct))
          (with-meep-mock-preset meep-test-prio-extra-mode '((meep-test--prio-var . :extra))
            (should (equal '((meep-test--prio-var . :direct)) (meep-preset-ensure)))))))))

(ert-deftest meep-preset-ensure-breadth-first-nearest-ancestor-wins ()
  "The walk is breadth-first: a level-1 `derived-mode-extra-parents'
preset wins over a level-2 ancestor reached through the direct parent
chain.  A depth-first walk would instead find the deeper ancestor
first."
  ;; child --(direct)--> direct --(direct)--> grand (level 2, has preset)
  ;;   \--(extra)--> extra (level 1, has preset)
  (with-meep-derived-mode-parent meep-test-bf-child-mode meep-test-bf-direct-mode
    (with-meep-derived-mode-extra-parents meep-test-bf-child-mode (meep-test-bf-extra-mode)
      (with-meep-derived-mode-parent meep-test-bf-direct-mode meep-test-bf-grand-mode
        (with-meep-preset-test meep-test-bf-child-mode
          (with-meep-mock-preset meep-test-bf-extra-mode '((meep-test--bf-var . :level-1-extra))
            (with-meep-mock-preset meep-test-bf-grand-mode '((meep-test--bf-var . :level-2-grand))
              (should (equal '((meep-test--bf-var . :level-1-extra)) (meep-preset-ensure))))))))))

(ert-deftest meep-preset-ensure-loaded-without-function-returns-nil ()
  "Feature loads but defines no function => nil, cached-missing-state."
  (with-meep-preset-test meep-test-no-fn-mode
    (meep-test--with-advice ((#'require
                              :around
                              (lambda (orig &rest args)
                                (cond
                                 ((eq (car args) 'meep-preset-meep-test-no-fn-mode)
                                  t)
                                 (t
                                  (apply orig args)))))
                             (#'lwarn :override (lambda (&rest _) nil)))
      (should (null (meep-preset-ensure)))
      (should (eq t (gethash 'meep-test-no-fn-mode meep--preset-cache))))))

(ert-deftest meep-preset-ensure-load-error-shadows-parent ()
  "A child preset file that errors during `require' stops the walk;
the parent's preset is *not* silently substituted."
  (with-meep-derived-mode-parent meep-test-loaderr-child-mode meep-test-good-parent-mode-2
    (with-meep-preset-test meep-test-loaderr-child-mode
      (with-meep-mock-preset meep-test-good-parent-mode-2
          '((meep-test--should-not-be-set . :from-parent))
        ;; Child file errors during load.
        (meep-test--with-advice ((#'require
                                  :around
                                  (lambda (orig &rest args)
                                    (cond
                                     ((eq (car args) 'meep-preset-meep-test-loaderr-child-mode)
                                      (signal 'error '("synthetic-load-error")))
                                     (t
                                      (apply orig args))))))
          (should (null (meep-preset-ensure)))
          (should (eq t (gethash 'meep-test-loaderr-child-mode meep--preset-cache))))))))

(ert-deftest meep-preset-ensure-broken-child-shadows-parent ()
  "A child preset file that loads but defines no function stops the walk;
the parent's preset is *not* silently substituted."
  (with-meep-derived-mode-parent meep-test-broken-child-mode meep-test-good-parent-mode
    (with-meep-preset-test meep-test-broken-child-mode
      (with-meep-mock-preset meep-test-good-parent-mode
          '((meep-test--should-not-be-set . :from-parent))
        ;; Child file loads but provides no function.
        (meep-test--with-advice ((#'require
                                  :around
                                  (lambda (orig &rest args)
                                    (cond
                                     ((eq (car args) 'meep-preset-meep-test-broken-child-mode)
                                      t)
                                     (t
                                      (apply orig args))))))
          (should (null (meep-preset-ensure)))
          (should (eq t (gethash 'meep-test-broken-child-mode meep--preset-cache))))))))

(ert-deftest meep-preset-ensure-non-list-return-cached-missing ()
  "A preset returning a non-list value is treated as cached-missing
and does not propagate to the apply step."
  (with-meep-preset-test meep-test-bad-return-mode
    (with-meep-mock-preset meep-test-bad-return-mode 42
      (should (null (meep-preset-ensure)))
      (should (eq t (gethash 'meep-test-bad-return-mode meep--preset-cache))))))

(ert-deftest meep-preset-ensure-cycle-guard ()
  "A cyclic `derived-mode-parent' chain terminates without looping.
What bounds the cycle is that the walk tests each mode at most once, so
that is what is asserted - counting the modes tested rather than waiting
for the loop to stop.  A `with-timeout' here read as a guard but was not
one: the walk takes no input, so its timer never fired & a regression
hung the whole run instead of failing this test."
  (with-meep-derived-mode-parent meep-test-cycle-a-mode meep-test-cycle-b-mode
    (with-meep-derived-mode-parent meep-test-cycle-b-mode meep-test-cycle-a-mode
      (with-meep-preset-test meep-test-cycle-a-mode
        (meep-test--with-call-limit #'meep--preset-try-mode 8
          (should (null (meep-preset-ensure))))
        (should (eq t (gethash 'meep-test-cycle-a-mode meep--preset-cache)))))))

(defvar meep-test--evar nil)

(ert-deftest meep-preset-ensure-variable-buffer-local-wins ()
  "A buffer-local VAR is returned and the preset is not consulted."
  (with-meep-preset-test meep-test-evar-local-mode
    (puthash 'meep-test-evar-local-mode '((meep-test--evar . :from-preset)) meep--preset-cache)
    (setq-local meep-test--evar :user-set)
    (should (eq :user-set (meep-preset-ensure-variable 'meep-test--evar)))))

(ert-deftest meep-preset-ensure-variable-buffer-local-nil-wins ()
  "A buffer-local VAR set to nil is returned as nil; the preset is not consulted.
A user's explicit `(setq-local meep-foo nil)' is a deliberate
override and must shadow the preset."
  (with-meep-preset-test meep-test-evar-local-nil-mode
    (puthash 'meep-test-evar-local-nil-mode '((meep-test--evar . :from-preset)) meep--preset-cache)
    (setq-local meep-test--evar nil)
    (should (local-variable-p 'meep-test--evar))
    (should (null (meep-preset-ensure-variable 'meep-test--evar)))))

(ert-deftest meep-preset-ensure-variable-globally-set-wins ()
  "A globally-set VAR is returned and the preset is not consulted.
A user's `(setq meep-foo ...)' in their init must take precedence
over the bundled preset."
  (with-meep-preset-test meep-test-evar-global-mode
    (puthash 'meep-test-evar-global-mode '((meep-test--evar . :from-preset)) meep--preset-cache)
    (let ((meep-test--evar :user-set-globally))
      (should-not (local-variable-p 'meep-test--evar))
      (should (eq :user-set-globally (meep-preset-ensure-variable 'meep-test--evar))))))

(ert-deftest meep-preset-ensure-variable-from-preset-when-unset ()
  "When VAR is unset (nil), the preset's value is returned without applying."
  (with-meep-preset-test meep-test-evar-preset-mode
    (puthash 'meep-test-evar-preset-mode '((meep-test--evar . :from-preset)) meep--preset-cache)
    (should (null meep-test--evar))
    (should (eq :from-preset (meep-preset-ensure-variable 'meep-test--evar)))
    ;; Crucially: returning the value did NOT install it buffer-locally.
    (should-not (local-variable-p 'meep-test--evar))))

(ert-deftest meep-preset-ensure-variable-no-preset-no-value ()
  "When VAR is unset and absent from the preset, return nil."
  (with-meep-preset-test meep-test-evar-fallback-mode
    (puthash 'meep-test-evar-fallback-mode t meep--preset-cache)
    (should (null meep-test--evar))
    (should (null (meep-preset-ensure-variable 'meep-test--evar)))
    (should-not (local-variable-p 'meep-test--evar))))

(ert-deftest meep-preset-variables-bundled-presets-respect-allowlist ()
  "Every bundled `meep-preset-*' file only sets variables in
`meep-preset-variables'."
  (dolist (file (directory-files my-meep-load-path nil "\\`meep-preset-.+\\.el\\'"))
    (let ((preset-sym (intern (file-name-sans-extension file))))
      (require preset-sym)
      (dolist (entry (funcall preset-sym))
        (let ((var (car entry)))
          (unless (memq var meep-preset-variables)
            (ert-fail
             (format "preset %s sets disallowed variable %S (allowed: %S)"
                     file
                     var
                     meep-preset-variables))))))))

(ert-deftest meep-syntax-backend-resolve-auto-and-override ()
  "Auto resolves by mode; an explicit `meep-syntax-backend' overrides."
  (with-temp-buffer
    (text-mode)
    (let ((meep-syntax-backend nil))
      (should (eq 'text (meep--syntax-backend-resolve))))
    (let ((meep-syntax-backend 'syntax))
      (should (eq 'syntax (meep--syntax-backend-resolve)))))
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((meep-syntax-backend nil))
      (should (eq 'syntax (meep--syntax-backend-resolve))))
    (let ((meep-syntax-backend 'text))
      (should (eq 'text (meep--syntax-backend-resolve))))))

(ert-deftest meep-syntax-enclosing-pair-ignores-string-brackets ()
  "The syntax backend skips a bracket inside a string; the text scan does not."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a \"(\" b)")
    (goto-char (point-min))
    (search-forward "b")
    (let* ((pos (match-beginning 0))
           (bounds-init (cons pos pos))
           (bounds-limit (cons (point-min) (point-max)))
           (pair '("(" . ")")))
      ;; Syntax tree: only the real outer pair encloses point.
      (let ((meep-syntax-backend 'syntax))
        (should (equal '(1 . 10) (meep--syntax-enclosing-pair bounds-init bounds-limit pair))))
      ;; Text scan: the stray `(' inside the string is mistaken for the open.
      (let ((meep-syntax-backend 'text))
        (should (equal '(5 . 10) (meep--syntax-enclosing-pair bounds-init bounds-limit pair)))))))
;; ---------------------------------------------------------------------------
;; Bounds-of-char and bounds-of-thing motion

(ert-deftest bounds-of-char-calc-distinct-honours-count ()
  "The distinct-bracket finder peels to the Nth enclosing pair, like same-delimiter.
Previously N was dropped for brackets, so a count always returned the innermost; a
count past the available depth clamps to the outermost."
  (with-temp-buffer
    (insert "((a))") ; `(' at 1 and 2, `)' at 4 and 5.
    (let ((clamp (cons (point-min) (point-max)))
          (anchor (cons 3 3))) ; Point inside `a'.
      (should
       (equal '(2 . 5) (meep--region-mark-bounds-of-char-calc anchor clamp 1 '("(" . ")") t)))
      (should
       (equal '(1 . 6) (meep--region-mark-bounds-of-char-calc anchor clamp 2 '("(" . ")") t)))
      (should
       (equal '(1 . 6) (meep--region-mark-bounds-of-char-calc anchor clamp 9 '("(" . ")") t))))))

(ert-deftest bounds-of-char-calc-same-delim-cursor-on-open-honours-count ()
  "Cursor-on-open same-delimiter peels outward by N, like the other branches.
Previously the forward search dropped N, so every count returned the innermost
pair.  With the open anchored at point the tokens forward alternate close, open,
close ..., so the Nth pair's close is the (2N-1)th token and the span ends on a
closing delimiter (counting raw tokens would land on an opening one for N > 1)."
  (with-temp-buffer
    (insert "a 'one' 'two' 'three' b") ; Quotes at 3, 7, 9, 13, 15, 21.
    (let ((clamp (cons (point-min) (point-max)))
          (anchor (cons 3 3))) ; Point on the first quote (cursor-on-open).
      ;; N=1 is `'one'', N=2 spans out to the close of `'two'' (3 . 14), not its
      ;; open (3 . 10) - the span must end on a closing delimiter.
      (should
       (equal '(3 . 8) (meep--region-mark-bounds-of-char-calc anchor clamp 1 '("'" . "'") t)))
      (should
       (equal '(3 . 14) (meep--region-mark-bounds-of-char-calc anchor clamp 2 '("'" . "'") t)))
      ;; N=1 and N=2 must differ - the regression returned the innermost for both.
      (should-not
       (equal
        (meep--region-mark-bounds-of-char-calc anchor clamp 1 '("'" . "'") t)
        (meep--region-mark-bounds-of-char-calc anchor clamp 2 '("'" . "'") t)))
      ;; A count past the available delimiters returns nil, not the outermost.
      (should (null (meep--region-mark-bounds-of-char-calc anchor clamp 9 '("'" . "'") t))))))

;; ---------------------------------------------------------------------------
;; Syntax Comment Inner Bounds

(ert-deftest syntax-comment-bounds-inner-c-block ()
  "A C block comment's bounds exclude the `/*' & `*/' delimiters."
  (with-temp-buffer
    (insert "/* a */ x /* b */\n")
    (c-mode)
    ;; Any interior position, including the ender's own characters.
    (dolist (pos (list 3 4 5 6 7))
      (should (equal '(3 . 6) (meep--syntax-comment-bounds-inner pos))))
    ;; The opener's characters count as outside, as does the code between.
    (dolist (pos (list 1 2 8 9 10))
      (should (null (meep--syntax-comment-bounds-inner pos))))
    ;; The second comment resolves independently.
    (should (equal '(13 . 16) (meep--syntax-comment-bounds-inner 14)))))

(ert-deftest syntax-comment-bounds-inner-c-line ()
  "A C line comment's bounds exclude the `//' opener & the closing newline."
  (with-temp-buffer
    (insert "// foo\nx\n")
    (c-mode)
    (should (equal '(3 . 7) (meep--syntax-comment-bounds-inner 5)))))

(ert-deftest syntax-comment-bounds-inner-single-char-delims ()
  "A single-char opener (`#' or `;') & the closing newline are excluded.
These delimiters have no syntax flags, exercising the class fall-backs."
  (with-temp-buffer
    (insert "# foo\nx\n")
    (with-meep-mode-defaults-for-python
      (python-mode))
    (should (equal '(2 . 6) (meep--syntax-comment-bounds-inner 4))))
  (with-temp-buffer
    ;; Only the first `;' is the delimiter, the second is content.
    (insert ";; foo\nx\n")
    (emacs-lisp-mode)
    (should (equal '(2 . 7) (meep--syntax-comment-bounds-inner 5)))))

(ert-deftest syntax-comment-bounds-inner-unterminated ()
  "An unterminated comment has no ender to bound, return nil."
  (with-temp-buffer
    (insert "/* foo")
    (c-mode)
    (should (null (meep--syntax-comment-bounds-inner 5)))))

(ert-deftest syntax-comment-bounds-inner-point-preserved ()
  "A pure query: point is preserved even when POS is far from point."
  (with-temp-buffer
    (insert "int x;\n/* a\n * b\n */\nint y;\n")
    (c-mode)
    (goto-char (point-max))
    (let ((pos-orig (point)))
      (should (equal '(10 . 19) (meep--syntax-comment-bounds-inner 12)))
      (should (eq pos-orig (point))))))

(ert-deftest comment-bounds-outer-keeps-printing-ender ()
  "A comment ending in a printing character keeps that character.
The ender has comment-end syntax, so skipping that syntax would truncate
`-->' to `--' & `}' from a Pascal brace comment."
  (with-temp-buffer
    (insert "<p>a</p>\n<!-- c -->\n")
    (html-mode)
    (goto-char 15)
    (should (equal '(10 . 20) (meep--bounds-at-point-for-comment-outer))))
  (with-temp-buffer
    (insert "{ c }\nx\n")
    (pascal-mode)
    (goto-char 3)
    (should (equal '(1 . 6) (meep--bounds-at-point-for-comment-outer)))))

(ert-deftest comment-bounds-outer-drops-line-comment-newline ()
  "A line comment's terminating newline is not part of the outer bounds."
  (with-temp-buffer
    (insert "// l\nx\n")
    (c-mode)
    (goto-char 3)
    (should (equal '(1 . 5) (meep--bounds-at-point-for-comment-outer)))))

(defun meep-test--comment-inner-bounds (mode text pos &optional disable-vars)
  "Return the inner comment bounds at POS in a buffer of TEXT using MODE.
DISABLE-VARS are set buffer-locally to nil first.  Buffer-local is what
shadows a preset - a nil global value falls back to the preset instead."
  (with-temp-buffer
    (insert text)
    (funcall mode)
    (dolist (var disable-vars)
      (set (make-local-variable var) nil))
    (goto-char pos)
    (meep--bounds-at-point-for-comment-inner)))

(defun meep-test--comment-block-bounds (mode text pos inner)
  "Return the comment block bounds at POS in a buffer of TEXT using MODE.
INNER selects the inner bounds, as `meep--bounds-of-comment-block'.

The block scan resolves the comment at point once per comment it takes
in, so a low ceiling on that call catches a scan that stops making
progress - see `meep-test--with-call-limit' for why the ceiling is what
reports it & not a timeout."
  (with-temp-buffer
    (insert text)
    (funcall mode)
    (goto-char pos)
    (meep-test--with-call-limit #'meep--bounds-at-point-for-comment-outer 32
      (meep--bounds-of-comment-block inner))))

(ert-deftest comment-block-bounds-printing-closer-terminates ()
  "A comment whose closer is a printing character ends the block scan.

`meep--bounds-at-point-for-comment-outer' steps back a character when
point sits just past a comment, so where the closer is a printing
character (`-->', `*/', `}') & nothing blank follows it, the scan finds
the comment it just measured & the block never grows.  The extension has
to be checked for progress, not merely for having found a comment.

A regression reports as a failure rather than hanging the run, see
`meep-test--with-call-limit' - the loop reads no input, so a timeout
could not have done that."
  ;; Nothing after the closer, the comment ends the buffer.
  (should (equal '(1 . 11) (meep-test--comment-block-bounds #'html-mode "<!-- a -->" 6 nil)))
  (should (equal '(1 . 8) (meep-test--comment-block-bounds #'c-mode "/* a */" 4 nil)))
  ;; A printing character after the closer.
  (should (equal '(1 . 11) (meep-test--comment-block-bounds #'html-mode "<!-- a -->x" 6 nil)))
  (should (equal '(1 . 8) (meep-test--comment-block-bounds #'c-mode "/* a */x\n" 4 nil)))
  (should (equal '(1 . 6) (meep-test--comment-block-bounds #'pascal-mode "{ a }x" 3 nil)))
  ;; The inner bounds take the same scan.
  (should (equal '(6 . 7) (meep-test--comment-block-bounds #'html-mode "<!-- a -->" 6 t)))
  (should (equal '(4 . 5) (meep-test--comment-block-bounds #'c-mode "/* a */x" 4 t))))

(ert-deftest comment-block-bounds-outer-includes-only-line-ending ()
  "Outer block bounds exclude non-newline text after the final comment."
  (should (equal '(1 . 9) (meep-test--comment-block-bounds #'c-mode "/* a */\nnext" 4 nil))))

(ert-deftest comment-block-bounds-extends-across-adjacent-comments ()
  "The block still grows over comments separated only by blank-space.
The progress check must not cost the extension it guards."
  (should
   (equal
    '(1 . 23) (meep-test--comment-block-bounds #'html-mode "<!-- a -->\n<!-- b -->\n" 6 nil)))
  (should (equal '(1 . 13) (meep-test--comment-block-bounds #'pascal-mode "{ a }\n{ b }\n" 3 nil)))
  (should
   (equal '(1 . 11) (meep-test--comment-block-bounds #'emacs-lisp-mode ";; a\n;; b\n" 3 nil)))
  ;; The first & last comment's own delimiters are what inner contracts by.
  (should
   (equal '(6 . 18) (meep-test--comment-block-bounds #'html-mode "<!-- a -->\n<!-- b -->\n" 6 t)))
  (should
   (equal '(4 . 10) (meep-test--comment-block-bounds #'emacs-lisp-mode ";; a\n;; b\n" 3 t))))

(ert-deftest comment-block-bounds-extends-across-touching-comments ()
  "A block includes comments whose delimiters touch exactly."
  (should
   (equal '(1 . 21) (meep-test--comment-block-bounds #'html-mode "<!-- a --><!-- b -->" 6 nil)))
  (should (equal '(1 . 15) (meep-test--comment-block-bounds #'c-mode "/* a *//* b */" 4 nil)))
  (should (equal '(1 . 11) (meep-test--comment-block-bounds #'pascal-mode "{ a }{ b }" 3 nil))))

(ert-deftest comment-block-bounds-includes-unterminated-final-comment ()
  "A final line comment without a newline remains in its preceding block."
  (should (equal '(1 . 10) (meep-test--comment-block-bounds #'emacs-lisp-mode ";; a\n;; b" 3 nil)))
  (should (equal '(4 . 10) (meep-test--comment-block-bounds #'emacs-lisp-mode ";; a\n;; b" 3 t))))

(ert-deftest comment-block-bounds-does-not-cross-printing-character ()
  "A printing character separates comments even when followed by an opener.

The unterminated-comment fallback may step inside a delimiter at point, but
must not step over arbitrary text to find a later comment."
  (should (equal '(1 . 8) (meep-test--comment-block-bounds #'c-mode "/* a */ x/* b */" 4 nil)))
  (should (equal '(1 . 9) (meep-test--comment-block-bounds #'c-mode "/* a */\nx/* b */" 4 nil))))

(defun meep-test--comment-outer-bounds (mode text pos)
  "Return the outer comment bounds at POS in a buffer of TEXT using MODE."
  (with-temp-buffer
    (insert text)
    (funcall mode)
    (goto-char pos)
    (meep--bounds-at-point-for-comment-outer)))

(ert-deftest comment-bounds-outer-blank-space-ender ()
  "The outer bounds stop before a blank-space ender, whichever it is.
A mode names every character that ends a line comment, not the newline
alone - taking any other into the bounds means deleting the comment also
deletes it, silently rewriting the line."
  ;; `cc-mode' gives CR the same comment-end syntax as the newline, so a file
  ;; with mixed line endings ends this comment with the CR.  Taking it into
  ;; the bounds rewrote that one line to LF while its neighbours stayed CRLF.
  (should (equal '(1 . 5) (meep-test--comment-outer-bounds #'c-mode "// a\r\nx;\n" 4)))
  ;; A formfeed ends a line comment in `tex-mode' & `awk-mode', where it also
  ;; separates pages - deleting the comment must not take the separator.
  (should (equal '(1 . 4) (meep-test--comment-outer-bounds #'tex-mode "% a\fx\n" 3)))
  (should (equal '(1 . 4) (meep-test--comment-outer-bounds #'awk-mode "# a\fx\n" 3)))
  ;; Other line-comment modes leave CR as comment text in their syntax table;
  ;; it still belongs to the CRLF terminator, not the selected comment.
  (with-meep-mode-defaults-for-python
    (dolist (case
             '((emacs-lisp-mode "; a\r\nx\r\n" (1 . 4))
               (python-mode "# a\r\nx\r\n" (1 . 4))
               (sql-mode "-- a\r\nx\r\n" (1 . 5))
               (lua-mode "-- a\r\nx\r\n" (1 . 5))))
      (pcase-let ((`(,mode ,text ,expected) case))
        (should (equal expected (meep-test--comment-outer-bounds mode text 3))))))
  ;; A printing ender is the delimiter itself & stays whole, `<!-- x -->'
  ;; must not come back as `<!-- x --'.
  (should (equal '(1 . 11) (meep-test--comment-outer-bounds #'html-mode "<!-- x -->" 6)))
  (should (equal '(1 . 11) (meep-test--comment-outer-bounds #'html-mode "<!-- a -->\n<p>\n" 6)))
  (should (equal '(1 . 6) (meep-test--comment-outer-bounds #'pascal-mode "{ a }\nb;\n" 3))))

(ert-deftest comment-bounds-outer-unterminated-keeps-blank-lines ()
  "An unterminated comment keeps the blank lines it is open over.
A block comment being typed at a file's end runs to the buffer's end, so
its blank lines are the comment's own text & selecting or deleting it
must take them.  The mode must not decide this: `cc-mode' names the
newline as ending a *line* comment, which truncated the block comment
here, where `css-mode' leaving it blank-space never did."
  (should (equal '(1 . 6) (meep-test--comment-outer-bounds #'c-mode "/* a\n" 4)))
  (should (equal '(1 . 6) (meep-test--comment-outer-bounds #'css-mode "/* a\n" 4)))
  (should (equal '(1 . 8) (meep-test--comment-outer-bounds #'c-mode "/* a\n\n\n" 4)))
  (should (equal '(1 . 8) (meep-test--comment-outer-bounds #'css-mode "/* a\n\n\n" 4)))
  ;; Blank lines *after* a terminated comment are not its own.
  (should (equal '(1 . 5) (meep-test--comment-outer-bounds #'c-mode "// a\n\n\nx;\n" 3))))

(ert-deftest comment-bounds-inner-opener-longer-than-comment-start ()
  "An opener longer than `comment-start' is stepped over whole.
`comment-start' can be a *prefix* of another opener the mode names in
`comment-start-skip' - `texinfo-mode' writes both `@c ' & `@comment ' -
so capping the match to its length cut the delimiter mid-word & left the
rest as the comment's text."
  (let ((disable '(meep-move-comment-skip-space)))
    (should
     (equal '(9 . 11) (meep-test--comment-inner-bounds #'texinfo-mode "@comment a\n" 10 disable)))
    ;; The shorter opener the same mode names still contracts.
    (should (equal '(3 . 5) (meep-test--comment-inner-bounds #'texinfo-mode "@c a\n" 4 disable)))
    ;; `bat-mode' names `REM' in a case its `comment-start' does not match.
    (should (equal '(4 . 6) (meep-test--comment-inner-bounds #'bat-mode "REM a\n" 5 disable)))))

(ert-deftest comment-bounds-inner-decoration-contracts-symmetrically ()
  "Decoration is given back at both ends or neither.
`comment-end-skip' is greedy where the mode sets it (`css-mode' uses
`[ \\t]*\\*+/'), & with nothing capping it the trailing run was taken as
delimiter while the leading one stayed content - so `/*** a ***/' came
back as `** a ' & deleting the selection left `/****/'."
  (let ((disable '(meep-move-comment-skip-space meep-move-comment-skip-repeated)))
    (should
     (equal '(3 . 10) (meep-test--comment-inner-bounds #'css-mode "/*** a ***/\n" 7 disable)))
    ;; A mode leaving `comment-end-skip' nil contracts the same markup.
    (should
     (equal '(3 . 10) (meep-test--comment-inner-bounds #'c-mode "/*** a ***/\n" 7 disable))))
  ;; `comment-normalize-vars' derives `comment-end-skip' buffer-locally, which
  ;; any comment command does - the bounds must not depend on having run one.
  (with-temp-buffer
    (insert "/*** a ***/\n")
    (c-mode)
    (setq-local meep-move-comment-skip-space nil)
    (setq-local meep-move-comment-skip-repeated nil)
    (comment-normalize-vars t)
    (goto-char 7)
    (should (equal '(3 . 10) (meep--bounds-at-point-for-comment-inner)))))

(ert-deftest comment-bounds-inner-opener-run-of-differing-chars ()
  "An opener written as differing comment-start characters stays whole.
Scheme's `#;' & Fortran's `*!' are one delimiter, not decoration, so
`meep-move-comment-skip-repeated' does not answer for them - leaving the
`;' of `#;' as content let deleting the selection corrupt the comment."
  (let ((disable '(meep-move-comment-skip-space meep-move-comment-skip-repeated)))
    (should (equal '(3 . 5) (meep-test--comment-inner-bounds #'fortran-mode "*! a\n" 4 disable)))
    (should (equal '(3 . 8) (meep-test--comment-inner-bounds #'scheme-mode "#;(a b)\n" 4 disable)))
    ;; A repeat of the *same* character is decoration, still the option's.
    (should
     (equal '(2 . 6) (meep-test--comment-inner-bounds #'emacs-lisp-mode ";;; a\n" 4 disable)))))

(ert-deftest comment-bounds-inner-two-char-opener-keeps-comment-start-content ()
  "Do not extend a resolved two-character opener over its text.

The semicolon after Scheme's `#|' is content even though it has
comment-start syntax; removing it leaves it stranded when the body is
replaced."
  (should (equal '(3 . 7) (meep-test--comment-inner-bounds #'scheme-mode "#|;foo|#\n" 4))))

(ert-deftest comment-bounds-inner-multi-char-delimiters ()
  "Multi-char delimiters contract whole, in a comment & mid-buffer.
Contracting by syntax alone steps over a single character, leaving
`<!-- c -->' as `!-- c --'."
  (should
   (equal '(15 . 16) (meep-test--comment-inner-bounds #'html-mode "<p>a</p>\n<!-- c -->\n" 15)))
  ;; `nxml-mode' resolves its preset by that name, `xml-mode' is only an alias.
  (with-meep-mode-defaults-for-nxml
    (should (equal '(6 . 7) (meep-test--comment-inner-bounds #'nxml-mode "<!-- c -->\n" 6)))))

(ert-deftest comment-bounds-inner-generic-fence-closer ()
  "A generic-comment-fence closer contracts like a comment-end character.
The opener path accepts the fence class (14); the inner-end must too, else
`!inner!' keeps its closing fence & comes back as `inner!'.  No built-in
mode exposes a bare class-14 comment fence, so install one directly."
  (should
   (equal
    '(2 . 7)
    (meep-test--comment-inner-bounds
     (lambda ()
       (let ((st (make-syntax-table)))
         (modify-syntax-entry ?! "!" st)
         (set-syntax-table st)))
     "!inner!" 4))))

(ert-deftest comment-bounds-inner-propertized-content-ender-kept ()
  "A content character propertized as the comment's ender stays content.
`perl-mode' moves a comment's ender onto its last character when the
newline must open a here-doc, as `scheme-mode' marks the datum's final
character for `#;'.  Stepping over it truncates the content to
`commen'; a closer the mode's grammar names carries its class in the
syntax table itself, which tells the two apart."
  (should
   (equal
    '(12 . 20)
    (meep-test--comment-inner-bounds #'perl-mode "$x = <<A; # comment\nbody\nA\n" 14
                                     '(meep-move-comment-skip-space
                                       meep-move-comment-skip-repeated)))))

(ert-deftest bounds-inner-from-delimiters-first-match-wins ()
  "`meep-bounds-inner-from-delimiters' takes the first pair that matches.
The delimiters are fixed strings, for a mode whose comments neither the
comment variables nor character syntax describe - Lua's `--[[ ]]' &
Ruby's `=begin/=end' are configured this way."
  (with-temp-buffer
    (insert "=begin x =end")
    (let ((bounds (cons (point-min) (point-max))))
      (should (equal '(7 . 10) (meep-bounds-inner-from-delimiters '(("=begin" . "=end")) bounds)))
      ;; A pair that does not match is passed over, not treated as a failure.
      (should
       (equal
        '(7 . 10)
        (meep-bounds-inner-from-delimiters '(("--[[" . "]]") ("=begin" . "=end")) bounds)))
      ;; Both ends must match, an opener alone does not select the pair.
      (should (null (meep-bounds-inner-from-delimiters '(("=begin" . "]]")) bounds)))
      (should (null (meep-bounds-inner-from-delimiters '(("--[[" . "=end")) bounds)))
      (should (null (meep-bounds-inner-from-delimiters nil bounds))))))

(ert-deftest comment-bounds-inner-delimiter-spec-wins-over-deduction ()
  "A `meep-bounds-for-inner-comment' spec resolves the inner bounds.
The spec is the only mechanism for a delimiter neither the comment
variables nor character syntax can describe, so it is consulted before
they are.  Written with delimiters that disagree with what the mode
would deduce, otherwise the precedence is not being tested at all."
  ;; `/*[[' & `]]*/' - the mode deduces its own `/*' & `*/', the spec does not.
  (with-temp-buffer
    (insert "/*[[ x ]]*/\n")
    (c-mode)
    (setq-local meep-move-comment-skip-space nil)
    (setq-local meep-bounds-for-inner-comment
                (list #'meep-bounds-inner-from-delimiters '(("/*[[" . "]]*/"))))
    (goto-char 6)
    (should (equal '(5 . 8) (meep--bounds-at-point-for-comment-inner))))
  ;; The same markup without a spec, showing the two disagree.
  (with-temp-buffer
    (insert "/*[[ x ]]*/\n")
    (c-mode)
    (setq-local meep-move-comment-skip-space nil)
    (goto-char 6)
    (should (equal '(3 . 10) (meep--bounds-at-point-for-comment-inner))))
  ;; A spec whose pairs do not match falls back to deduction rather than
  ;; leaving the comment uncontracted.
  (with-temp-buffer
    (insert "/*[[ x ]]*/\n")
    (c-mode)
    (setq-local meep-move-comment-skip-space nil)
    (setq-local meep-bounds-for-inner-comment
                (list #'meep-bounds-inner-from-delimiters '(("=begin" . "=end"))))
    (goto-char 6)
    (should (equal '(3 . 10) (meep--bounds-at-point-for-comment-inner)))))

(ert-deftest comment-bounds-inner-delimiter-spec-read-from-preset ()
  "The spec is resolved through the preset, not only a user setting.
`meep--bounds-at-point-for-comment-inner-guess' reads it with
`meep-preset-ensure-variable', so a mode's preset supplies it without the
user calling `meep-preset-ensure' from a hook."
  (with-meep-preset-test meep-test-delim-mode
    (with-meep-mock-preset meep-test-delim-mode
        (list
         (cons
          'meep-bounds-for-inner-comment
          (list #'meep-bounds-inner-from-delimiters '(("/*[[" . "]]*/")))))
      (insert "/*[[ x ]]*/\n")
      ;; Comment syntax comes from `c-mode', the preset is keyed off the
      ;; `major-mode' set after it.
      (c-mode)
      (setq-local major-mode 'meep-test-delim-mode)
      (setq-local meep-move-comment-skip-space nil)
      (goto-char 6)
      (should (equal '(5 . 8) (meep--bounds-at-point-for-comment-inner))))))

(ert-deftest comment-bounds-inner-without-preset ()
  "A mode with no preset contracts by comment syntax."
  (with-meep-mode-defaults-for-python
    (should (equal '(3 . 4) (meep-test--comment-inner-bounds #'python-mode "# a\nx=1\n" 3)))))

(ert-deftest comment-bounds-inner-from-comment-vars ()
  "Without a preset, delimiters come from the mode's comment variables.
`<!--' & `-->' are longer than character syntax can express."
  (with-meep-mode-defaults-for-nxml
    (dolist (mode (list #'html-mode #'nxml-mode))
      (should
       (equal
        '(6 . 7)
        (meep-test--comment-inner-bounds mode "<!-- c -->\n" 6
                                         '(meep-bounds-for-inner-comment)))))))

(ert-deftest comment-bounds-inner-ends-resolve-independently ()
  "Each end takes the first source that resolves it.
`c++-mode' leaves `comment-end' empty for its preferred `//' comments, so
a block comment's `*/' is only found by character syntax."
  (should
   (equal
    '(4 . 5)
    (meep-test--comment-inner-bounds #'c++-mode "/* b */\n" 4 '(meep-bounds-for-inner-comment)))))

(ert-deftest comment-bounds-inner-delimiters-share-padding ()
  "A comment small enough that both delimiters claim the same padding.
Stepping over padding at each end would cross them, contracting nothing.
Padding is content, `meep-move-comment-skip-space' decides whether to skip it."
  (should (equal '(5 . 6) (meep-test--comment-inner-bounds #'html-mode "<!-- -->\n" 6)))
  (should
   (equal
    '(5 . 8)
    (meep-test--comment-inner-bounds #'html-mode "<!-- c -->\n" 6
                                     '(meep-move-comment-skip-space)))))

(ert-deftest comment-bounds-inner-greedy-delimiter-match ()
  "A greedy `comment-start-skip' can claim the closer's characters.
`/\\*+' takes all three of `/**/'s asterisks, leaving nothing for `\\*+/' -
character syntax measures both delimiters directly instead."
  (should (equal '(3 . 3) (meep-test--comment-inner-bounds #'c-mode "/**/\n" 3))))

(ert-deftest comment-bounds-inner-decoration-is-not-the-delimiter ()
  "`comment-start-skip' matching past the delimiter does not take content.
`fortran-mode' reads a repeated character after the delimiter as decoration
\(`C----'), which in a comment reading `Cooling loop here' is its text.  The
delimiter's length comes from `comment-start' & character syntax instead."
  (let ((meep-move-comment-skip-space nil))
    (should
     (equal '(2 . 18) (meep-test--comment-inner-bounds #'fortran-mode "Cooling loop here\n" 4)))
    ;; Decoration is content too, it is only never *part of* the delimiter.
    (should
     (equal '(2 . 13) (meep-test--comment-inner-bounds #'fortran-mode "C---- banner\n" 4)))))

(ert-deftest comment-bounds-inner-case-folded-decoration-is-not-delimiter ()
  "Case-folded regexp repeats do not turn comment text into a delimiter."
  (let ((meep-move-comment-skip-space nil))
    (should
     (equal '(2 . 18) (meep-test--comment-inner-bounds #'fortran-mode "CoOling loop here\n" 4)))))

(ert-deftest comment-bounds-inner-keeps-newline-padding ()
  "Blank lines before the closer are content, whatever the mode pads with.
`nxml-mode' allows a newline in its `comment-end-skip' padding & `html-mode'
does not, so the same markup contracted differently between them."
  (with-meep-mode-defaults-for-nxml
    (dolist (mode (list #'nxml-mode #'html-mode))
      (should
       (equal
        '(5 . 10)
        (meep-test--comment-inner-bounds mode "<!-- a\n\n\n-->" 6
                                         '(meep-move-comment-skip-space)))))))

(ert-deftest comment-bounds-inner-end-skip-closer-begins-line-above ()
  "A closer whose match begins above END's own line is still found.
`vera-mode' closes a block comment with the newline itself (` *\\n'), so
the match starts on the line before END.  The search is bounded near END
rather than run from the comment's start - bounding it to END's own line
found no closer at all for those modes, & the comment came back
uncontracted with nothing to say why."
  (with-temp-buffer
    (insert "/* a  \n")
    (c-mode)
    (setq-local comment-end-skip " *\n")
    (should (eq 8 (meep--comment-inner-end-from-vars 1 8))))
  ;; The blank-space the closer claims is on its own line here.
  (with-temp-buffer
    (insert "/* a\n  \n")
    (c-mode)
    (setq-local comment-end-skip " *\n")
    (should (eq 9 (meep--comment-inner-end-from-vars 1 9)))))

(ert-deftest comment-bounds-inner-end-skip-alternation-anchored ()
  "A `comment-end-skip' alternation is anchored as a whole, not branch by branch.
An unwrapped top-level `\\|' would leave every branch but the last free to
match mid-comment.  The value here stands in for `vera-mode's
\\` *\\\\*+/\\\\| *\\n', the bundled mode this was found with."
  (with-temp-buffer
    (insert "/* a XX b */")
    (c-mode)
    (setq-local comment-end-skip "XX\\| *\\*+/")
    (setq-local meep-move-comment-skip-space nil)
    (goto-char 6)
    (should (equal '(3 . 11) (meep--bounds-at-point-for-comment-inner)))))

(ert-deftest comment-bounds-inner-line-comment-keeps-block-closer ()
  "A line comment whose text ends in the mode's block closer keeps it.
Both the comment variables & the two-character syntax flags describe the
block closer, which is content here - commented-out code is written this
way.  The comment is still open at its end, having only a newline to close
it, which is what tells the two apart."
  (should
   (equal '(4 . 27) (meep-test--comment-inner-bounds #'c-mode "// int x = 1; /* legacy */\n" 10)))
  (should (equal '(4 . 13) (meep-test--comment-inner-bounds #'js-mode "// a /* b */\n" 8)))
  ;; Pascal closes a block comment with a printing character.
  (should (equal '(4 . 11) (meep-test--comment-inner-bounds #'pascal-mode "// a { b }\n" 8)))
  ;; An unterminated block comment has no closer to step over either.  Its
  ;; newline is the comment's text: `cc-mode' names the newline as ending a
  ;; *line* comment, & reading that as this comment's end truncated it - a
  ;; mode whose newline is plain blank-space (`css-mode') never did, so the
  ;; same markup contracted two ways.
  (should (equal '(4 . 6) (meep-test--comment-inner-bounds #'c-mode "/* a\n" 4))))

(ert-deftest comment-bounds-inner-keeps-repeated-opener ()
  "Nil `meep-move-comment-skip-repeated' keeps a repeated opener.
`comment-start-skip' takes the whole run (`//+'), so the base delimiter
comes from character syntax - which caps out at two characters, leaving
`<!--' to be recognized by the comment variables as before."
  (let ((meep-move-comment-skip-repeated nil)
        (meep-move-comment-skip-space nil))
    ;; Only the delimiter's repeats are given back, `//' itself is still taken.
    (should (equal '(3 . 7) (meep-test--comment-inner-bounds #'c-mode "//// a\n" 6)))
    (with-meep-mode-defaults-for-sh
      (should (equal '(2 . 5) (meep-test--comment-inner-bounds #'sh-mode "## a\n" 4))))
    (should (equal '(2 . 7) (meep-test--comment-inner-bounds #'emacs-lisp-mode ";;;; a\n" 6)))
    (should (equal '(3 . 10) (meep-test--comment-inner-bounds #'c-mode "/*** a ***/\n" 7)))
    ;; A multi-char opener is not a repeat, it stays whole.
    (should (equal '(5 . 8) (meep-test--comment-inner-bounds #'html-mode "<!-- c -->\n" 6))))
  ;; The default skips the run at both ends.
  (let ((meep-move-comment-skip-space nil))
    (should (equal '(5 . 7) (meep-test--comment-inner-bounds #'c-mode "//// a\n" 6)))
    (should (equal '(5 . 8) (meep-test--comment-inner-bounds #'c-mode "/*** a ***/\n" 7)))))

(ert-deftest comment-bounds-inner-repeated-only-marker-is-not-empty ()
  "Skipping repeated markers never contracts an inner range to nothing."
  (should (equal '(2 . 5) (meep-test--comment-inner-bounds #'emacs-lisp-mode ";;;;\n" 3))))

(ert-deftest comment-bounds-inner-recovers-body-written-in-mode-syntax ()
  "A comment written the mode's own way contracts to exactly its body.
Each comment is built from that mode's `comment-start' & `comment-end', so
the test follows a mode's own idea of its delimiters instead of hard-coding
them.  A mode whose comments are not in its syntax table (`org-mode' &
`rst-mode' parse their own) has no outer bounds to contract, skip it."
  (dolist (mode
           '(c-mode
             c++-mode
             objc-mode
             java-mode
             idl-mode
             pike-mode
             awk-mode
             css-mode
             js-mode
             html-mode
             nxml-mode
             python-mode
             emacs-lisp-mode
             lisp-mode
             scheme-mode
             sh-mode
             bat-mode
             makefile-mode
             conf-mode
             tex-mode
             sql-mode
             perl-mode
             ruby-mode
             pascal-mode
             prolog-mode
             verilog-mode
             vhdl-mode
             asm-mode
             dns-mode
             ld-script-mode
             gdb-script-mode
             fortran-mode
             nroff-mode))
    (when (fboundp mode)
      (with-temp-buffer
        (with-meep-mode-defaults
          (funcall mode))
        (when (and comment-start (not (string-empty-p (string-trim comment-start))))
          ;; A leading newline so a column-sensitive opener (`fortran-mode') starts a line.
          (insert "\n")
          (let ((beg (point)))
            (insert comment-start "BODY" comment-end "\n")
            (goto-char (+ beg (length comment-start) 1))
            (should (meep--bounds-at-point-for-comment-outer))
            (let ((bounds (meep--bounds-at-point-for-comment-inner)))
              (should bounds)
              (should
               (equal "BODY" (buffer-substring-no-properties (car bounds) (cdr bounds)))))))))))

(ert-deftest comment-inner-beg-from-vars-skip-needing-leading-context ()
  "An opener resolves when `comment-start-skip' matches context before it.
`perl-mode' & `tcl-mode' match `\\(^\\|\\s-\\);?#+ *' & similar, which a search
starting at the delimiter can never satisfy - the literal `comment-start'
has to answer instead."
  (dolist (mode (list #'perl-mode #'tcl-mode))
    (with-temp-buffer
      (funcall mode)
      (insert "code();\n   # note\n")
      ;; Point 12 is the `#', indented so `^' cannot match before it.
      (should (eq 13 (meep--comment-inner-beg-from-vars 12)))))
  ;; The literal must not answer where the mode has no comment: `fortran-mode'
  ;; comments start with `C' in column one, mid-line the same letter is code.
  (with-temp-buffer
    (fortran-mode)
    (insert "C a b\n      x = C D\n")
    (should (null (meep--comment-inner-beg-from-vars 23)))))

(ert-deftest comment-bounds-inner-repeated-opener-characters ()
  "An opener written as a run of comment-start characters is stepped over whole.
Scheme's `#;' & Fortran's `*!' are two such characters, not a two-character
delimiter, and unlike `;;' they differ so `meep-move-comment-skip-repeated'
cannot help.  The run is crossed whichever source matched the first
character - Scheme reaches it by syntax, Fortran by `comment-start-skip'."
  (should (equal '(3 . 8) (meep-test--comment-inner-bounds #'scheme-mode "#;(a b)\n" 4)))
  (should
   (equal '(4 . 8) (meep-test--comment-inner-bounds #'fortran-mode "*! note\n      x = 1\n" 4))))

(ert-deftest comment-bounds-inner-two-char-delimiters-no-preset ()
  "A two-char delimiter contracts in a mode that never had a preset.
Neither `--' nor `#|' has comment-start syntax, both are syntax flags on
character pairs."
  (should (equal '(4 . 5) (meep-test--comment-inner-bounds #'sql-mode "-- l\nselect 1;\n" 4)))
  (should (equal '(4 . 5) (meep-test--comment-inner-bounds #'lisp-mode "#| b |#\n" 4))))

(ert-deftest move-bounds-of-thing-echo-escapes-percent ()
  "A `%' in a bounds-motion key or description does not crash the transient help.
The echo escapes user-interpolated `%' before `set-transient-map' reads it through
`format-spec', see `meep--set-transient-map-echo'."
  (with-temp-buffer
    (let ((meep-bounds-commands '((?% meep-move-char-next "pc%t")))
          (inhibit-message t))
      ;; The bare `%' in the key and description would be an invalid format spec.
      (let ((exit (meep--move-bounds-of-thing-impl 1)))
        (should (functionp exit))
        (funcall exit)))))

(provide 'meep_tests_internal)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
;;; meep_tests_internal.el ends here
