;;; meep-preset-pascal-mode.el --- Meep preset for `pascal-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-pascal-mode ()
  "Return the meep preset alist for `pascal-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("(*" . "*)") ; Block comment.
       ("{" . "}")))))) ; Brace comment.

(provide 'meep-preset-pascal-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-pascal-mode.el ends here
