;;; meep-preset-lisp-mode.el --- Meep preset for `lisp-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-lisp-mode ()
  "Return the meep preset alist for `lisp-mode' (Common Lisp)."
  (declare (important-return-value t))
  (list
   (cons
    'meep-list-item-bounds
    ;; Lisp lists `()' separate on whitespace.
    '(((?\( . ?\)) t)))))

(provide 'meep-preset-lisp-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-lisp-mode.el ends here
