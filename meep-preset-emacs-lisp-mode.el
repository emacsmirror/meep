;;; meep-preset-emacs-lisp-mode.el --- Meep preset for `emacs-lisp-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-emacs-lisp-mode ()
  "Return the meep preset alist for `emacs-lisp-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-list-item-bounds
    ;; Lisp lists `()' and vectors `[]' separate on whitespace.
    '(((?\( . ?\)) t) ((?\[ . ?\]) t)))))

(provide 'meep-preset-emacs-lisp-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-emacs-lisp-mode.el ends here
