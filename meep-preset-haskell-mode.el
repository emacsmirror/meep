;;; meep-preset-haskell-mode.el --- Meep preset for `haskell-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-haskell-mode ()
  "Return the meep preset alist for `haskell-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("{-#" . "#-}") ; Pragma.
       ("{-" . "-}")))))) ; Block comment.

(provide 'meep-preset-haskell-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-haskell-mode.el ends here
