;;; meep-preset-clojure-mode.el --- Meep preset for `clojure-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-clojure-mode ()
  "Return the meep preset alist for `clojure-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-list-item-bounds
    ;; Lists `()', vectors `[]' and maps/sets `{}' all separate on whitespace.
    '(((?\( . ?\)) t) ((?\[ . ?\]) t) ((?\{ . ?\}) t)))))

(provide 'meep-preset-clojure-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-clojure-mode.el ends here
