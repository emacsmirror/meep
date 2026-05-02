;;; meep-preset-go-mode.el --- Meep preset for `go-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-go-mode ()
  "Return the meep preset alist for `go-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("//" . "")))))) ; Line comment.

(provide 'meep-preset-go-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-go-mode.el ends here
