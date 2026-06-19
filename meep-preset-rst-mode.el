;;; meep-preset-rst-mode.el --- Meep preset for `rst-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-rst-mode ()
  "Return the meep preset alist for `rst-mode'."
  (declare (important-return-value t))
  (list
   ;; Multi-character delimiters (`**', ``` `` ```) appear before their
   ;; single-character counterparts so the more specific markup wins
   ;; when both could match.
   (cons
    'meep-match-bounds-of-char-contextual
    '(("\"" . "\"") ; Prose quote.
      ("**" . "**") ; Strong emphasis (bold).
      ("*" . "*") ; Emphasis (italic).
      ("``" . "``") ; Inline literal (code).
      ("`" . "`") ; Interpreted text.
      ("|" . "|") ; Substitution reference.
      ("(" . ")") ; Parens.
      ("[" . "]") ; Link / footnote / citation.
      ("{" . "}"))) ; Brace.
   ;; RST has no inline strike-through, so `strike' is left undefined.
   (cons
    'meep-surround-pairs
    '((bold . ("**" . "**")) ; Strong emphasis.
      (italic . ("*" . "*")) ; Emphasis.
      (code . ("``" . "``")))))) ; Inline literal.

(provide 'meep-preset-rst-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-rst-mode.el ends here
