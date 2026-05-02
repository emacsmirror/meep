;;; meep-preset-lua-mode.el --- Meep preset for `lua-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-lua-mode ()
  "Return the meep preset alist for `lua-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("--[[" . "]]") ; Block comment.
       ("--" . "")))))) ; Line comment.

(provide 'meep-preset-lua-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-lua-mode.el ends here
