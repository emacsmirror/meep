;;; meep-preset-c++-mode.el --- Meep preset for `c++-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-c++-mode ()
  "Return the meep preset alist for `c++-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-c++-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-c++-mode.el ends here
