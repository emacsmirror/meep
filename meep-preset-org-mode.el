;;; meep-preset-org-mode.el --- Meep preset for `org-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-org-mode ()
  "Return the meep preset alist for `org-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-match-bounds-of-char-contextual
    '(("\"" . "\"") ; Prose quote.
      ("*" . "*") ; Bold.
      ("/" . "/") ; Italic.
      ("_" . "_") ; Underline.
      ("=" . "=") ; Verbatim.
      ("~" . "~") ; Code.
      ("+" . "+") ; Strike-through.
      ("(" . ")") ; Parens.
      ("[" . "]") ; Link / bracket.
      ("{" . "}"))) ; Macro `{{{...}}}' / brace.
   (cons
    'meep-surround-pairs
    '((bold . ("*" . "*")) ; Bold.
      (italic . ("/" . "/")) ; Italic.
      (code . ("~" . "~")) ; Code.
      (strike . ("+" . "+")))))) ; Strike-through.

(provide 'meep-preset-org-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-org-mode.el ends here
