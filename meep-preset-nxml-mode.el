;;; meep-preset-nxml-mode.el --- Meep preset for `nxml-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-nxml-mode ()
  "Return the meep preset alist for `nxml-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("<!--" . "-->")))))) ; Comment.

(provide 'meep-preset-nxml-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-nxml-mode.el ends here
