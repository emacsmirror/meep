;;; meep-preset-ruby-mode.el --- Meep preset for `ruby-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-ruby-mode ()
  "Return the meep preset alist for `ruby-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("=begin" . "=end")))))) ; Block comment.

(provide 'meep-preset-ruby-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-ruby-mode.el ends here
