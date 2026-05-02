;;; meep-preset-css-mode.el --- Meep preset for `css-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-css-mode ()
  "Return the meep preset alist for `css-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/")))))) ; Block comment.

(provide 'meep-preset-css-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-css-mode.el ends here
