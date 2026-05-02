;;; meep-preset-objc-mode.el --- Meep preset for `objc-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-objc-mode ()
  "Return the meep preset alist for `objc-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-objc-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-objc-mode.el ends here
