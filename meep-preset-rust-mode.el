;;; meep-preset-rust-mode.el --- Meep preset for `rust-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-rust-mode ()
  "Return the meep preset alist for `rust-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-rust-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-rust-mode.el ends here
