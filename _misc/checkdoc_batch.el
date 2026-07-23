;;; checkdoc_batch.el --- Batch checkdoc with a failing exit code -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run `checkdoc-file' on each file argument, exiting with an error status
;; when any warnings were found:
;;
;;   `emacs --batch -l checkdoc_batch.el FILES...'

;;; Code:

(require 'checkdoc)

(defvar checkdoc-batch--found nil
  "Non-nil when a checkdoc warning was created.")

;; Batch runs cannot prompt, never modify the file being checked.
(setq checkdoc-autofix-flag 'never)

;; Wrap the default function instead of replacing it: the default constructs
;; the error object (a bare list or a struct depending on the Emacs version)
;; & `checkdoc-file' already prints each warning via `warn' in batch, so all
;; that's needed here is noting that a warning occurred.
(let ((orig-fn checkdoc-create-error-function))
  (setq checkdoc-create-error-function
        (lambda (text start end &optional unfixable)
          (setq checkdoc-batch--found t)
          (funcall orig-fn text start end unfixable))))

(dolist (file command-line-args-left)
  (checkdoc-file file))

(when checkdoc-batch--found
  (kill-emacs 1))
(kill-emacs 0)

;;; checkdoc_batch.el ends here
