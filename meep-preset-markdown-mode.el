;;; meep-preset-markdown-mode.el --- Meep preset for `markdown-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-markdown-mode ()
  "Return the meep preset alist for `markdown-mode'."
  (declare (important-return-value t))
  (list
   ;; Multi-character delimiters (`**', `__', ``` `` ```, `~~')
   ;; precede their single-character forms so the more specific
   ;; markup wins when both could match.
   (cons
    'meep-match-bounds-of-char-contextual
    '(("\"" . "\"") ; Prose quote.
      ("**" . "**") ; Strong emphasis (bold).
      ("*" . "*") ; Emphasis (italic).
      ("__" . "__") ; Strong emphasis (bold, alt).
      ("_" . "_") ; Emphasis (italic, alt).
      ("``" . "``") ; Inline code (literal backticks).
      ("`" . "`") ; Inline code.
      ("~~" . "~~") ; Strike-through (GFM).
      ("(" . ")") ; Parens.
      ("[" . "]") ; Link.
      ("{" . "}"))) ; Brace.
   (cons
    'meep-surround-pairs
    '((bold . ("**" . "**")) ; Strong emphasis.
      (italic . ("*" . "*")) ; Emphasis.
      (code . ("`" . "`")) ; Inline code.
      (strike . ("~~" . "~~")))))) ; Strike-through (GFM).

(provide 'meep-preset-markdown-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-markdown-mode.el ends here
