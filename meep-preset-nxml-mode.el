;;; meep-preset-xml-mode.el --- Meep preset for `xml-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-xml-mode ()
  "Return the meep preset alist for `xml-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("<!--" . "-->")))))) ; Comment.

(provide 'meep-preset-xml-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-xml-mode.el ends here
