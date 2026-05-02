;;; meep-preset-idl-mode.el --- Meep preset for `idl-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-idl-mode ()
  "Return the meep preset alist for `idl-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-idl-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-idl-mode.el ends here
