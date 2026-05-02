;;; meep-preset-dart-mode.el --- Meep preset for `dart-mode' -*- lexical-binding: t -*-

;;;###autoload
(defun meep-preset-dart-mode ()
  "Return the meep preset alist for `dart-mode'."
  (declare (important-return-value t))
  (list
   (cons
    'meep-bounds-for-inner-comment
    '(meep-bounds-inner-from-delimiters
      (("/*" . "*/") ; Block comment.
       ("//" . "")))))) ; Line comment.

(provide 'meep-preset-dart-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "meep.el"
;; End:
;;; meep-preset-dart-mode.el ends here
