;;; meep-preset-html-mode.el --- Meep preset for `html-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-html-mode ()
  "Return the meep preset alist for `html-mode'."
  (declare (important-return-value t))
  ;; `meep-bounds-for-inner-comment' is intentionally not set: `<!--' & `-->'
  ;; are deduced from the mode's comment variables.
  (list
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
