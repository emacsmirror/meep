;;; meep-preset-html-mode.el --- Meep preset for `html-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-html-mode ()
  "Return the meep preset alist for `html-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("<!--" . "-->")))) ; Comment.
   (cons
    'meep-surround-pairs
    '((bold . ("<b>" . "</b>")) ; Bold.
      (italic . ("<i>" . "</i>")) ; Italic.
      (code . ("<code>" . "</code>")) ; Inline code.
      (strike . ("<s>" . "</s>")))))) ; Strike-through.

(provide 'meep-preset-html-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-html-mode.el ends here
