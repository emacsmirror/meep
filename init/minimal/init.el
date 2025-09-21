;;; init.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A minimal configuration with primitive VIM like key bindings.

;; ----------------------------------------------------------------------------
;; Key-map
;;
;; Primitive VIM like, HJKL motion.
;; Note that MEEP and VIM operate on different principles
;; this is mainly an example for users to start out with
;; who are familiar with VIM.
;;
;; - HJKL motion.
;; - Search.
;; - Visual mode.
;; - Insert mode keys.

(defun my-meep-define-keys (map &rest keybinds)
  (declare (indent 1))
  (pcase-dolist (`(,key . ,def) keybinds)
    (define-key map (kbd key) def)))

(defun my-meep-basis-keys ()
  (my-meep-define-keys meep-state-keymap-motion

    ;; Motion.
    '("h" . meep-move-char-prev)
    '("j" . meep-move-line-next)
    '("k" . meep-move-line-prev)
    '("l" . meep-move-char-next)

    '("b" . meep-move-word-prev)
    '("B" . meep-move-symbol-prev)

    '("w" . meep-move-word-next)
    '("W" . meep-move-symbol-next)

    '("e" . meep-move-word-next-end)
    '("E" . meep-move-symbol-next-end)

    '("f" . meep-move-find-char-on-line-at-next)
    '("F" . meep-move-find-char-on-line-at-prev)

    '("^" . meep-move-line-non-space-beginning)
    '("$" . meep-move-line-end)
    '("%" . meep-move-matching-contextual-outer)

    '("(" . meep-move-sentence-prev)
    '(")" . meep-move-sentence-next)

    '("{" . meep-move-paragraph-prev)
    '("}" . meep-move-paragraph-next)

    ;; Motion (find).
    '("t" . meep-move-find-char-on-line-till-next)
    '("T" . meep-move-find-char-on-line-till-prev)

    '("/" . meep-isearch-regexp-next)
    '("?" . meep-isearch-regexp-prev)
    '("n" . meep-isearch-repeat-next)
    '("N" . meep-isearch-repeat-prev)
    '("*" . meep-isearch-at-point-next)
    '("#" . meep-isearch-at-point-prev)

    ;; Clipboard.
    '("y" . meep-clipboard-only-copy)
    '("p" . meep-clipboard-only-yank)

    ;; Macros.
    '("q" . meep-register-kmacro-start-or-end)
    '("@" . meep-register-jump-to)

    ;; Mark.
    '("m" . point-to-register)
    '("'" . jump-to-register)

    ;; Editing.
    '("r" . meep-char-replace)
    '("R" . meep-insert-overwrite)

    '("J" . meep-join-line-next)

    '("x" . meep-delete-char-next)
    '("X" . meep-delete-char-prev)

    '("d" . meep-clipboard-killring-cut)

    '("u" . undo-only)
    '("U" . undo-redo)

    '("<" . indent-rigidly-left-to-tab-stop)
    '(">" . indent-rigidly-right-to-tab-stop)

    '("." . repeat-fu-execute)

    ;; Insert mode.
    '("i" . meep-insert)
    '("I" . meep-insert-line-beginning)
    '("a" . meep-insert-append)
    '("A" . meep-insert-line-end)
    '("o" . meep-insert-open-below)
    '("O" . meep-insert-open-above)

    ;; Visual mode.
    '("v" . meep-region-toggle)
    '("V" . meep-region-expand-to-line-bounds)
    '("C-v" . rectangle-mark-mode))

  (my-meep-define-keys meep-state-keymap-normal)

  (my-meep-define-keys meep-state-keymap-visual)

  (my-meep-define-keys meep-state-keymap-insert '("<escape>" . bray-state-stack-pop)))


;; ----------------------------------------------------------------------------
;; Emacs Defaults.

;; No need for startup screen.
(setq inhibit-startup-screen t)

;; Don't interrupt the user with things that *might* be confusing.
(setq help-uni-confusables nil)


;; ----------------------------------------------------------------------------
;; Packages

;; Setup packages we depend on.

(defvar my-meep-load-path (file-name-concat (file-name-directory load-file-name) ".." ".."))

;; Defer loading packages by default. Why?
;; .. faster startup for packages which are only activated on certain modes or key bindings.
(setq use-package-always-defer t)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(use-package bray
  :commands (bray-mode)
  :ensure t)

(use-package meep
  :commands (meep-bootstrap-once)
  :ensure nil
  ;; NOTE: point to local path OR, remove once this is in a package-repository.
  :load-path my-meep-load-path)

(use-package repeat-fu
  :commands (repeat-fu-mode)
  :ensure t
  :config (setq repeat-fu-preset 'meep)
  :hook ((bray-mode . (lambda () (repeat-fu-mode (or bray-mode 0))))))

(defun my-meep-setup-once ()
  ;; Extended functions.
  (meep-bootstrap-once)

  (setq meep-state-insert 'insert)
  (setq bray-state-default 'normal)

  (defvar meep-state-hook-insert-enter nil)
  (defvar meep-state-hook-insert-exit nil)

  (defvar meep-state-hook-normal-enter nil)
  (defvar meep-state-hook-normal-exit nil)

  ;; Visual mode.
  (defun meep-mark-hook-activate ()
    "Activate visual state."
    (when (bray-state-derived-p 'normal)
      (bray-state-stack-push 'visual)))
  (defun meep-mark-hook-deactivate ()
    "Activate visual state."
    (when (bray-state-derived-p 'visual)
      (bray-state-stack-pop)))

  (add-hook
   'bray-mode-hook
   (lambda ()
     (cond
      (bray-mode
       (add-hook 'activate-mark-hook #'meep-mark-hook-activate)
       (add-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate))
      (t
       (remove-hook 'activate-mark-hook #'meep-mark-hook-activate)
       (remove-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate)))))
  ;; End visual mode support.

  (add-hook
   'meep-state-hook-insert-enter
   (lambda ()
     (set-mark (point))
     (deactivate-mark)))

  (add-hook
   'meep-state-hook-insert-exit
   (lambda ()
     (deactivate-mark)

     ;; VIM style '^' register for when we leave insert mode.
     (let ((reg meep-state-insert-register))
       (let ((reg-val (get-register reg)))
         (cond
          ((and reg-val (markerp reg-val))
           (set-marker reg-val (point) (current-buffer)))
          (t
           (set-register reg (point-marker))))))))


  (defvar meep-state-keymap-motion (make-keymap))
  (defvar meep-state-keymap-normal (make-keymap))
  (defvar meep-state-keymap-visual (make-keymap))
  (defvar meep-state-keymap-insert (make-keymap))

  ;; Optional, a quick way to mask insertion.
  (define-key meep-state-keymap-motion [remap self-insert-command] 'undefined)

  (setq bray-state-definitions
        (list
         (list
          :id 'normal
          ;; Define.
          :cursor-type 'hollow
          :lighter "<N>"
          :keymaps (list (cons t 'meep-state-keymap-motion) (cons t 'meep-state-keymap-normal))

          :enter-hook 'meep-state-hook-normal-enter
          :exit-hook 'meep-state-hook-normal-exit)

         (list
          :id 'visual
          ;; Define.
          :cursor-type 'hollow
          :lighter "<V>"
          :keymaps (list (cons t 'meep-state-keymap-motion) (cons t 'meep-state-keymap-visual))

          :enter-hook 'meep-state-hook-visual-enter
          :exit-hook 'meep-state-hook-visual-exit)

         (list
          :id 'insert
          ;; Define.
          :cursor-type 'bar
          :lighter "<I>"
          :keymaps (list (cons t 'meep-state-keymap-insert))

          :enter-hook 'meep-state-hook-insert-enter
          :exit-hook 'meep-state-hook-insert-exit

          ;; Optional.
          :is-input t)))

  (my-meep-basis-keys))

(my-meep-setup-once)

;; Enable MEEP for most buffers.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   ;; Enable these modes for all non-special buffers (files).
   (unless (minibufferp)
     (unless (derived-mode-p
              (list 'special-mode 'gud-mode 'term-mode 'inferior-emacs-lisp-mode 'dired-mode))
       (bray-mode 1)))))

;; Don't nag when loading this file.
(put 'elisp-autofmt-load-packages-local 'safe-local-variable (lambda (_) t))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t t)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
;;; init.el ends here
