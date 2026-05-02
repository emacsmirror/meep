;;; meep-preset-cmake-mode.el --- Meep preset for `cmake-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-cmake-mode ()
  "Return the meep preset alist for `cmake-mode'."
  (declare (important-return-value t))
  (list
   ;; Actual comment is `#[[ ]]'; `#[=[ ]=]' (with arbitrary `=') is also
   ;; allowed.  Match the minimal `#[ ]' and rely on blank-skipping for
   ;; the `=' padding characters.
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("#[" . "]")))))) ; Block comment.

(provide 'meep-preset-cmake-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-cmake-mode.el ends here
