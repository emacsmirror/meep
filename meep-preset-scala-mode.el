;;; meep-preset-scala-mode.el --- Meep preset for `scala-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-scala-mode ()
  "Return the meep preset alist for `scala-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-scala-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-scala-mode.el ends here
