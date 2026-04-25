;;; meep-region-mark.el --- Utilities for marking things -*- lexical-binding: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Luis Henriquez Perez

;; Author: Luis Henriquez Perez <luishenriquezperez@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-meep
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;;; Code:
(require 'meep)

(defmacro meep-region-mark--define-command (kind inout)
  "Define `meep-region-mark-KIND-INOUT' as an interactive mark command.
KIND is a text-object kind registered in `meep-text-object-alist'.
INOUT is the symbol `inner' or `outer'."
  (let ((inner (eq inout 'inner))
        (name (intern (format "meep-region-mark-%s-%s" kind inout))))
    `(defun ,name ()
       ,(format "Mark %s %s at point." inout kind)
       (interactive)
       (let ((bounds (meep-calc-bounds-at-point ',kind ,inner)))
         (cond
          (bounds
           (meep--region-mark-bounds-to-region bounds 'is-forward))
          (t
           (message ,(format "Not found: bounds of %s" kind))
           nil))))))

(defmacro meep-region-mark--define-all-commands ()
  "Expand to a `progn' defining mark commands for every kind.
Generated at macro-expansion time so each `defun' is visible to `load' and
gets a proper `symbol-file'."
  (let (forms)
    (dolist (entry meep-text-object-alist)
      (let ((kind (car entry)))
        (unless (plist-get (cdr entry) :no-inner)
          (push `(meep-region-mark--define-command ,kind inner) forms))
        (push `(meep-region-mark--define-command ,kind outer) forms)))
    `(progn
       ,@
       (nreverse forms))))


;; ---------------------------------------------------------------------------
;; Region Mark Commands (optional)
;;
;; These commands must be explicitly loaded via::
;;
;;    (require 'meep-region-mark)

(meep-region-mark--define-all-commands)

;; Deferred until `meep' loads so we can iterate the alist.
;;;###autoload
(with-eval-after-load 'meep
  (dolist (entry meep-text-object-alist)
    (let ((base (format "meep-region-mark-%s" (car entry))))
      (unless (plist-get (cdr entry) :no-inner)
        (autoload (intern (concat base "-inner")) "meep-region-mark" nil t))
      (autoload (intern (concat base "-outer")) "meep-region-mark" nil t))))

(provide 'meep-region-mark)
;;; meep-region-mark.el ends here
