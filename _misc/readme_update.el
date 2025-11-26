;;; readme_update.el --- Extract doc-strings -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-meep
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Dump doc-strings from emacs.

;;; Call:

;; (readme_update "^package-name-[a-z]+" 'fun (list 'package-name-to-exclude))

;; TODO.

;;; Code:

(defun readme_update-printf (&rest args)
  "Print function using ARGS."
  (princ (apply #'format args)))

(defun readme_update (prefix ty exclude)
  "Print doc-strings matching PREFIX (regex).
Of TY in symbol, excluding symbols in EXCLUDE.

Valid values for TY include:
- 'fun: Non interactive functions.
- 'fun-interactive: Only interactive functions.
- 'var: Variables.
- 'var-custom: Only custom variables."
  (let ((no-doc-string "<undocumented>")
        (vars nil))
    (mapatoms
     (lambda (sym)
       (let ((sym-str (symbol-name sym)))
         (when (string-match-p prefix sym-str)
           (unless (memq sym exclude)
             (when (cond
                    ((eq ty 'fun-interactive)
                     (and (fboundp sym) (commandp sym)))
                    ((eq ty 'fun)
                     (and (fboundp sym) (null (commandp sym))))
                    ((eq ty 'var)
                     (and (null (fboundp sym)) (null (get sym 'custom-type))))
                    ((eq ty 'var-custom)
                     (and (null (fboundp sym)) (get sym 'custom-type))))
               (push sym vars)))))))

    ;; Sort by their order in the buffer.
    ;; TODO: sort by filename as well using `string-lessp'.
    ;; NOTE: it seems sorting functions may not be necessary is they
    ;; look to be ordered by default.
    (cond
     ((memq ty (list 'fun 'fun-interactive))
      (setq vars
            (sort vars
                  #'(lambda (x y)
                      (< (cdr-safe (or (find-function-noselect x t) 0))
                         (cdr-safe (or (find-function-noselect y t) 0)))))))
     ((or (eq ty 'var) (eq ty 'var-custom))
      (setq vars
            (sort vars
                  #'(lambda (x y)
                      (< (cdr-safe (or (find-variable-noselect x nil) 0))
                         (cdr-safe (or (find-variable-noselect y nil) 0))))))))


    (dolist (sym vars)
      (let ((doc
             (cond
              ((memq ty (list 'fun 'fun-interactive))
               (or (documentation sym t) no-doc-string))
              ((or (eq ty 'var) (eq ty 'var-custom))
               (or (documentation-property sym 'variable-documentation) no-doc-string)))))

        (cond
         ((memq ty (list 'fun 'fun-interactive))
          (readme_update-printf
           "``%s``\n"
           (with-temp-buffer
             ;; Extract the second line which is the function signature.
             ;; TODO: there may be a cleaner way to extract the function signature.
             (insert (describe-function sym))
             (goto-char (point-min))
             (forward-line 2)
             (delete-region (point-min) (point))
             (forward-line 1)
             (delete-region (pos-eol) (point-max))
             (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
         (t
          (readme_update-printf
           "``%s``%s\n" (symbol-name sym)
           (or (ignore-errors
                 (format ": ``%S``" (symbol-value sym)))
               ""))))

        (with-temp-buffer
          (insert doc)
          (indent-rigidly (point-min) (point-max) 3)
          (readme_update-printf
           "%s\n\n" (buffer-substring-no-properties (point-min) (point-max))))))))

(provide 'readme_update)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; readme_update.el ends here
