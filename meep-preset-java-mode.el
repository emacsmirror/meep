;;; meep-preset-java-mode.el --- Meep preset for `java-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-java-mode ()
  "Return the meep preset alist for `java-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-java-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-java-mode.el ends here
