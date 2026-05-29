;;; meep-preset-scheme-mode.el --- Meep preset for `scheme-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-scheme-mode ()
  "Return the meep preset alist for `scheme-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-list-item-bounds
    ;; Scheme/Racket forms `()', `[]' and `{}' all separate on whitespace.
    '(((?\( . ?\)) t) ((?\[ . ?\]) t) ((?\{ . ?\}) t)))))

(provide 'meep-preset-scheme-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-scheme-mode.el ends here
