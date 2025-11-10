;;; meep_tests.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-meep
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides utility functions to bind to keys,
;; intended to be used with `bray' although
;; nearly all of the functions could also be used in vanilla Emacs.

;;; Usage

;; Bind any of the `autoload' functions in this file to a key-map.

;;; Code:

;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(require 'bray)
(require 'meep)

(defmacro simulate-input (&rest keys)
  "Helper macro to simulate input using KEYS."
  (declare (indent 0))
  `(let ((keys-list (list ,@keys)))
     (dolist (keys keys-list)
       (let ((minibuffer-message-timeout 0))
         (execute-kbd-macro keys)))))

(defun buffer-reset-text (initial-buffer-text)
  "Use INITIAL-BUFFER-TEXT to initialize the buffer with text."
  (buffer-disable-undo)
  (erase-buffer)
  ;; Don't move the cursor.
  (save-excursion (insert initial-buffer-text))
  (buffer-enable-undo))

(defmacro with-meep-test (initial-buffer-text &rest body)
  "Run BODY adding any message call to the MESSAGE-LIST list.
Setting the buffers text to INITIAL-BUFFER-TEXT."
  (declare (indent 1))
  ;; Messages make test output noisy (mainly evil mode switching messages).
  ;; Disable when debugging tests.
  `(let ((buf (generate-new-buffer "untitled"))
         ;; Keys:
         (k-meep-char-replace "g")
         (k-meep-insert "x")
         (k-meep-clipboard-killring-cut "e")
         (k-meep-move-char-next "l")
         (k-meep-move-char-prev "h")
         (k-meep-move-line-next "j")
         (k-meep-move-line-prev "k")
         (k-meep-move-matching-syntax-inner "'")
         (k-meep-move-matching-syntax-outer "\"")
         (k-meep-move-symbol-next ".")
         (k-meep-move-symbol-next-end "/")
         (k-meep-region-toggle "d")
         (k-meep-region-activate-and-reverse-motion "i")
         (k-bray-state-stack-pop [escape]) ;; Escape.
         (k-meep-insert-change "b")
         (k-rectangle-mark-mode "sd"))

     (ignore
      k-bray-state-stack-pop
      k-meep-char-replace
      k-meep-insert
      k-meep-clipboard-killring-cut
      k-meep-move-char-next
      k-meep-move-char-prev
      k-meep-move-line-next
      k-meep-move-line-prev
      k-meep-move-matching-syntax-inner
      k-meep-move-matching-syntax-outer
      k-meep-move-symbol-next
      k-meep-move-symbol-next-end
      k-meep-region-toggle
      k-meep-region-activate-and-reverse-motion
      k-meep-insert-change
      k-rectangle-mark-mode)

     ;; TODO: a global setting really.
     (setq ring-bell-function 'meep_tests-my-local-ring)

     (switch-to-buffer buf)
     (buffer-reset-text ,initial-buffer-text)
     (prog1 (progn
              ,@body)
       (kill-buffer buf))))

(defun meep_tests-my-local-ring ()
  "Utility to troubleshoot ringing the bell."
  (printf "<TEST RING>\n"))


;; ---------------------------------------------------------------------------
;; Tests

(defun meep_tests-print-state ()
  "Log the current state."
  (printf
   "log: %S %S (%S) %d-%d \"%s\"\n"
   bray-mode
   (bray-state)
   (buffer-name)
   (mark)
   (point)
   (buffer-string)))

(ert-deftest primitive-insert-word ()
  (let ((text-initial "")
        (text-expected "hello"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      ;; Select the line & increment.
      (simulate-input
        k-meep-insert
        "hello")
      (should (equal 'insert (bray-state)))

      (simulate-input
        (kbd "<escape>"))

      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-word-single ()
  "Ensure changing a word works."
  (let ((text-initial "foo bar baz")
        (text-expected "foo BUZ baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      (simulate-input
        (vconcat
         k-meep-move-symbol-next
         k-meep-move-symbol-next-end
         k-meep-insert-change
         "BUZ"
         k-bray-state-stack-pop))

      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-word-multiple ()
  "Ensure changing multiple words works."
  (let ((text-initial "foo bar baz ~")
        (text-expected "foo BUZ ~"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      (simulate-input
        (vconcat
         k-meep-move-symbol-next
         k-meep-move-symbol-next-end
         "1"
         k-meep-insert-change
         "BUZ"
         k-bray-state-stack-pop))

      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-word-multiple-adjust ()
  "Ensure changing a words works with an adjustment before the change."
  (let ((text-initial "foo bar bazX ~")
        (text-expected "foo BUZX ~"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      (simulate-input
        ;; format-next-line: off
        (vconcat
         k-meep-move-symbol-next
         k-meep-move-symbol-next-end
         "1" ; A second word.
         k-meep-move-char-prev ; One less character.
         k-meep-insert-change
         "BUZ"
         k-bray-state-stack-pop))

      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-comment-bounds-inner-cc ()
  "Ensure change in inner bounds works."
  (let ((text-initial "/* Hello World. */")
        (text-expected "/* Changed! */"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (simulate-input
        ;; format-next-line: off
        (vconcat
         k-meep-move-symbol-next
         k-meep-move-matching-syntax-inner
         k-meep-region-activate-and-reverse-motion
         k-meep-insert-change
         "Changed!"
         k-bray-state-stack-pop))
      ;; (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-comment-bounds-outer-cc ()
  "Ensure change in outer bounds works."
  (let ((text-initial "/* Hello World. */")
        (text-expected "// Changed!"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (simulate-input
        ;; format-next-line: off
        (vconcat
         k-meep-move-symbol-next
         k-meep-move-matching-syntax-outer
         k-meep-region-activate-and-reverse-motion
         k-meep-insert-change
         "// Changed!"
         k-bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-replace-char-region ()
  "Ensure replace char with rectangle mark mode."
  (let ((text-initial "......\n")
        (text-expected ".####.\n"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input
        ;; format-next-line: off
        (vconcat
         k-meep-move-char-next
         k-meep-region-toggle
         k-meep-move-char-next
         "3"
         k-meep-char-replace
         "#"))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-replace-char-region-rectangle ()
  "Ensure replace char with rectangle mark mode."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "......\n"
          "......\n"
          "......\n"
          "......\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "......\n"
          ".####.\n"
          ".####.\n"
          "......\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input
        ;; format-next-line: off
        (vconcat
         k-meep-move-char-next
         k-meep-move-line-next
         k-rectangle-mark-mode
         k-meep-move-char-next
         "3"
         k-meep-move-line-next
         k-meep-char-replace
         "#"))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-region-rectangle ()
  "Ensure insert-change works with rectangle mark mode."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "......\n"
          "......\n"
          "......\n"
          "......\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "......\n"
          ".####.\n"
          ".####.\n"
          "......\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input
        ;; format-next-line: off
        (vconcat
         k-meep-move-char-next
         k-meep-move-line-next
         k-rectangle-mark-mode
         k-meep-move-char-next
         "3"
         k-meep-move-line-next
         k-meep-insert-change
         "####"
         [return]))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(defun meep_tests-run-all ()
  (ert-run-tests-batch))

(provide 'meep_tests)
;;; meep_tests.el ends here
