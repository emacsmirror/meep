;;; meep_tests_internal.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-meep
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Tests for internal MEEP API's.

;;; Usage

;; Run:
;; `emacs -batch --script meep_tests_internal.el -f ert-run-tests-batch-and-exit'

;;; Code:

(defvar my-meep-load-path (file-name-concat (file-name-directory load-file-name) ".."))

(add-to-list 'load-path my-meep-load-path)

(require 'ert)
(require 'meep)

(meep-bootstrap-once)

;; ---------------------------------------------------------------------------
;; P-list Remove

(ert-deftest plist-remove-empty ()
  "Removing from an empty P-list should return nil."
  (should (equal (meep--plist-remove nil :foo) nil)))

(ert-deftest plist-remove-missing-key ()
  "Removing a key not present should return the original P-list."
  (let ((plist '(:a 1 :b 2)))
    (should (equal (meep--plist-remove plist :c) plist))))

(ert-deftest plist-remove-first-key ()
  "Removing the first key should drop the first key-value pair."
  (should (equal (meep--plist-remove '(:a 1 :b 2 :c 3) :a) '(:b 2 :c 3))))

(ert-deftest plist-remove-middle-key ()
  "Removing a key in the middle should drop that key-value pair."
  (should (equal (meep--plist-remove '(:a 1 :b 2 :c 3) :b) '(:a 1 :c 3))))

(ert-deftest plist-remove-last-key ()
  "Removing the last key should drop the last key-value pair."
  (should (equal (meep--plist-remove '(:a 1 :b 2) :b) '(:a 1))))

(ert-deftest plist-remove-multiple-occurrences ()
  "If the key appears multiple times, only the first occurrence is removed."
  (should (equal (meep--plist-remove '(:a 1 :b 2 :a 3) :a) '(:b 2 :a 3))))


;; ---------------------------------------------------------------------------
;; Region list overlap checks

(ert-deftest ranges-overlap-p-basic-overlap ()
  "Ranges clearly overlap."
  (should (meep--ranges-overlap-p '((1 . 5)) '((3 . 7))))
  (should (meep--ranges-overlap-p '((3 . 7)) '((1 . 5)))))

(ert-deftest ranges-overlap-p-no-overlap ()
  "Ranges are disjoint."
  (should-not (meep--ranges-overlap-p '((1 . 5)) '((6 . 10))))
  (should-not (meep--ranges-overlap-p '((6 . 10)) '((1 . 5)))))

(ert-deftest ranges-overlap-p-touching-edges ()
  "Ranges that touch at edges should count as overlap."
  (should (meep--ranges-overlap-p '((1 . 5)) '((5 . 8))))
  (should (meep--ranges-overlap-p '((5 . 8)) '((1 . 5)))))

(ert-deftest ranges-overlap-p-multiple-ranges ()
  "Overlap occurs when any pair of ranges intersect."
  (should (meep--ranges-overlap-p '((1 . 2) (10 . 15)) '((3 . 4) (14 . 20))))
  (should-not (meep--ranges-overlap-p '((1 . 2) (5 . 6)) '((3 . 4) (7 . 8)))))

(ert-deftest ranges-overlap-p-empty-lists ()
  "Empty lists never overlap."
  (should-not (meep--ranges-overlap-p nil nil))
  (should-not (meep--ranges-overlap-p '((1 . 5)) nil))
  (should-not (meep--ranges-overlap-p nil '((1 . 5)))))

(ert-deftest ranges-overlap-p-single-point-ranges ()
  "Ranges where start == end are treated as inclusive points."
  (should (meep--ranges-overlap-p '((5 . 5)) '((5 . 5))))
  (should (meep--ranges-overlap-p '((5 . 5)) '((4 . 5))))
  (should (meep--ranges-overlap-p '((5 . 5)) '((5 . 6)))))

(provide 'meep_tests_internal)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
;;; meep_tests_internal.el ends here
