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
  "Ranges that touch at edges should count as overlap."
  (should (meep--ranges-overlap-p '((1 . 5)) '((5 . 8))))
  (should (meep--ranges-overlap-p '((5 . 8)) '((1 . 5)))))

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
  "Ranges where start == end are treated as inclusive points."
  (should (meep--ranges-overlap-p '((5 . 5)) '((5 . 5))))
  (should (meep--ranges-overlap-p '((5 . 5)) '((4 . 5))))
  (should (meep--ranges-overlap-p '((5 . 5)) '((5 . 6)))))


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

(ert-deftest meep-preset-ensure-missing-returns-nil ()
  "A mode with no preset returns nil; cache stores `t' (cached-missing-state)."
  (with-meep-preset-test meep-test-no-such-mode
    (should (null (meep-preset-ensure)))
    (should (eq t (gethash 'meep-test-no-such-mode meep--preset-cache)))))

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
    (meep-preset-ensure)
    (should (local-variable-p 'meep-test--applied-var))
    (should (eq :from-preset meep-test--applied-var))))

(ert-deftest meep-preset-ensure-respects-existing-buffer-local ()
  "If a variable is already buffer-local, its value is not overwritten."
  (with-meep-preset-test meep-test-respect-mode
    (puthash
     'meep-test-respect-mode '((meep-test--respected-var . :from-preset)) meep--preset-cache)
    (setq-local meep-test--respected-var :user-set)
    (meep-preset-ensure)
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
  "A cyclic `derived-mode-parent' chain terminates without looping."
  (with-meep-derived-mode-parent meep-test-cycle-a-mode meep-test-cycle-b-mode
    (with-meep-derived-mode-parent meep-test-cycle-b-mode meep-test-cycle-a-mode
      (with-meep-preset-test meep-test-cycle-a-mode
        (with-timeout (2 (ert-fail "cycle guard did not terminate"))
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

(provide 'meep_tests_internal)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
;;; meep_tests_internal.el ends here
