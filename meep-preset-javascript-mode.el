;;; meep-preset-javascript-mode.el --- Meep preset for `javascript-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-javascript-mode ()
  "Return the meep preset alist for `javascript-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-javascript-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-javascript-mode.el ends here
