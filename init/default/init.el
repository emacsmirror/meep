;;; init.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; See: `../../emacs-run-local.sh' for running emacs with this as an INIT file.
;; See: `meep-tests.py' for launching this script for tests.

;; Detect if this is running in a test environment.
;; Otherwise we can assume this is an example for trying out meep.
(defconst my-meep-is-test-env (not (string-empty-p (or (getenv "MEEP_TEST_ENV") "")))
  "True when running tests")

;; ----------------------------------------------------------------------------
;; Key-map
;;
;; Symbols suffixes are as follows:
;;
;; - Command
;;   ... no modifiers.
;; - Command:S
;;   ... holding Shift.
;; - Command/f
;;   ... F-key, then the key without modifiers.
;;
;; +-------------+-------------+-------------+-------------+-------------+  +-------------+-------------+-------------+-------------+-------------+-------------+
;; | Grab        |             |             |             |             |  |             |             |             |             |             |             |
;; | GrabSwap    |             |             |             |             |  |             |             |             |             |             |             |
;; |             |             |             |             |             |  |             |             |             |             |             |             |
;; |             |             |             |             |             |  |             |             |             |             |             |             |
;; |             |             |             |             |             |  |             |             |             |             |             |             |
;; |            `|             |             |             |             |  |             |             |             |             |             |             |
;; +-------------+-------------+-------------+-------------+-------------+  +-------------+-------------+-------------+-------------+-------------+-------------+
;;
;; +-------------+-------------+-------------+-------------+-------------+  +-------------+-------------+-------------+-------------+-------------+-------------+
;; | Repeat      | Register    | CutKillRing | YankKillRing| CopyKillRing|  | MarkBoundsIn| SwapPtMark  | SwapPtMotion| Free        | KeyPad...   | JumpToReg   |
;; | Free:S      | Free:S      | CutClip:S   | PasteClip:S | CopyClip:S  |  | MarkBounds:S|             |             | Free:S      | PointToReg:S| MacroRec:S  |
;; |             |             |             |YankKillCh/s |             |  |             |             |             |             |             |             |
;; |             |             |             |             |             |  |             | AvyNext/f   | AvyPrev/f   |             |             | Fill&Move/s |
;; |             |             |             |             |             |  |             |             |             |             |             |             |
;; |            q|            w|            e|            r|            t|  |            y|            u|            i|            o|            p|            \|
;; +-------------+-------------+-------------+-------------+-------------+  +-------------+-------------+-------------+-------------+-------------+-------------+
;; | Surround... | Ex/Ins...   | Select      | Find...     | ReplaceCh   |  | Left        | Down        | Up          | Right       | JumpSexpIn  | JumpStrCmtIn|
;; | Free:S      | Free:S      | SelLn:S     | Free:S      | InsChar:S   |  | HomeNoWs:S  | ParaDown:S  | ParaUp:S    | EndNoWs:S   | JumpSexp:S  | JumpStrCmt:S|
;; |             | InsPrev/s   | SelBlock/s  |             |             |  | InsBOL/s    | InsBelow/s  | InsAbove/s  | InsEOL/s    |             |             |
;; |             |             |             |             |             |  | Find<Ch/f   |FindRegxNxt/f|FindRegxPrv/f| Find>Ch/f   | GotoLine/f  |             |
;; |             |             |             |             |             |  | Till<Ch:S/f |             |             | Till>Ch:S/f | GotoChar/f:S|             |
;; |            a|            s|            d|            f|            g|  |            h|            j|            k|            l|            ;|            '|
;; +-------------+-------------+-------------+-------------+-------------+  +-------------+-------------+-------------+-------------+-------------+-------------+
;; | Undo        | Insert      | DelChar     | Transpose   | Change      |  | SymBack     | FindNext    | FindPrev    | SymNext     |SymNextEnd   |
;; | Redo:S      | InsOver:S   | BkSpace:S   | DelLine:S   | ChangLine:S |  |SameSynPrev:S|             |             |SameSynNext:S|SameNextEnd:S|
;; |             |             |ShrinkSpace/s|             |             |  | FindRpt</f  |             |             | FindRpt>/f  |             |
;; |             |             |             |             |             |  | TillRpt<:S/f| Downcase/s  | Upcase/s    | TillRpt>:S/f|             |
;; |             |             |             |             |             |  |             | WordNext/f  | WordPrev/f  |             |             |
;; |            z|            x|            c|            v|            b|  |            n|            m|            ,|            .|            /|
;; +-------------+-------------+-------------+-------------+-------------+  +-------------+-------------+-------------+-------------+-------------+
;;
;; Other Bindings:
;;
;; - Tab: Indent Rigidly.
;;
;; - Shift-Delete:    JoinLineEnd.
;; - Shift-BackSpace: JoinLineBeginning.
;;
;; - -: Contract Region.
;; - =: Expand Region.
;;
;; - [: BegOfThing.
;; - ]: EndOfThing.
;;
;; - s 0-9: digit-argument.
;; - s -: negative-argument.

(defun my-key-free ()
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Key Free: %s" (format-kbd-macro keys))))

(defun my-meep-keymap-set-many (map &rest keybinds)
  (declare (indent 1))
  (pcase-dolist (`(,key . ,def) keybinds)
    (keymap-set map key def)))

(defun my-meep-basis-keys ()
  (my-meep-keymap-set-many meep-state-keymap-motion

    '("1" . meep-digit-argument-repeat)
    '("2" . meep-digit-argument-repeat)
    '("3" . meep-digit-argument-repeat)
    '("4" . meep-digit-argument-repeat)
    '("5" . meep-digit-argument-repeat)
    '("6" . meep-digit-argument-repeat)
    '("7" . meep-digit-argument-repeat)
    '("8" . meep-digit-argument-repeat)
    '("9" . meep-digit-argument-repeat)
    '("0" . meep-digit-argument-repeat)
    '("-" . meep-digit-argument-repeat)

    '("`" . meep-region-to-secondary-selection)
    '("~" . meep-region-swap)

    ;; ----
    ;; Left Hand: Row 1.
    ;; ----

    '("q" . repeat-fu-execute)
    '("Q" . my-key-free)

    '("w" . meep-clipboard-register-actions)
    '("W" . my-key-free)

    '("e" . meep-clipboard-killring-cut)
    '("E" . meep-clipboard-only-cut)

    '("r" . meep-clipboard-killring-yank-pop-stack)
    '("R" . meep-clipboard-only-yank)

    '("t" . meep-clipboard-killring-copy)
    '("T" . meep-clipboard-only-copy)

    ;; Left Hand: Row 2.

    ;; NOTE: a more comprehensive surround map is really needed.
    ;; This is only character level surround insertion.
    '("a" . meep-char-surround-insert)
    '("A" . meep-char-surround-insert-lines)

    '("s r" . meep-delete-char-ring-yank)
    '("s c" . meep-space-shrink-contextual)

    '("s s" . meep-insert-at-last)
    '("s d" . rectangle-mark-mode)

    '("s j" . meep-insert-open-below)
    '("s k" . meep-insert-open-above)
    '("s l" . meep-insert-line-end)
    '("s h" . meep-insert-line-beginning)

    '("s m" . downcase-region)
    '("s ," . upcase-region)

    '("s <return>" . fill-region)
    '("s <end>" . end-of-buffer)
    '("s <home>" . beginning-of-buffer)

    ;; Run commands "after" numeric has been set.
    ;; Unlike the default to re-running N times.
    '("s 1" . digit-argument)
    '("s 2" . digit-argument)
    '("s 3" . digit-argument)
    '("s 4" . digit-argument)
    '("s 5" . digit-argument)
    '("s 6" . digit-argument)
    '("s 7" . digit-argument)
    '("s 8" . digit-argument)
    '("s 9" . digit-argument)
    '("s 0" . digit-argument)
    '("s -" . negative-argument)

    '("d" . meep-region-toggle)
    '("D" . meep-region-expand-to-line-bounds)

    '("f h" . meep-move-find-char-on-line-at-prev)
    '("f H" . meep-move-find-char-on-line-till-prev)
    '("f j" . meep-isearch-regexp-next)
    '("f k" . meep-isearch-regexp-prev)
    '("f l" . meep-move-find-char-on-line-at-next)
    '("f L" . meep-move-find-char-on-line-till-next)

    ;; Find "repeat" are below the keys for find.
    '("f ." . meep-move-find-char-on-line-repeat-at-next)
    '("f n" . meep-move-find-char-on-line-repeat-at-prev)
    '("f >" . meep-move-find-char-on-line-repeat-till-next)
    '("f N" . meep-move-find-char-on-line-repeat-till-prev)

    '("f m" . meep-isearch-at-point-next)
    '("f ," . meep-isearch-at-point-prev)

    '("f u" . avy-goto-symbol-1-below)
    '("f i" . avy-goto-symbol-1-above)

    ;; Alternative to VIM's ":" to go to line numbers (frees up a key).
    '("f ;" . goto-line)
    '("f :" . goto-char)

    '("F" . my-key-free)

    '("g" . meep-char-replace)
    '("G" . meep-char-insert)

    ;; Left Hand: Row 3.
    '("z" . undo-only)
    '("Z" . undo-redo)

    '("x" . meep-insert)
    '("X" . meep-insert-overwrite)

    '("c" . meep-delete-char-ring-next)
    '("C" . meep-delete-char-ring-prev)

    '("v" . meep-transpose)
    '("V" . meep-clipboard-killring-cut-line) ; Odd-one out, locate for convenience.

    '("b" . meep-insert-change)
    '("B" . meep-insert-change-lines)

    ;; Right Hand: Row 1.
    '("y" . meep-region-mark-bounds-of-char-inner)
    '("Y" . meep-region-mark-bounds-of-char-outer)

    '("u" . meep-exchange-point-and-mark)
    '("U" . my-key-free)

    '("i" . meep-exchange-point-and-mark-motion)
    '("I" . my-key-free)

    '("o" . my-key-free)
    '("O" . my-key-free)

    '("p" . meep-keypad)
    '("P" . my-key-free)

    ;; Right Hand: Row 2.
    '("h" . meep-move-char-prev)
    '("H" . meep-move-line-non-space-beginning)


    '("j" . meep-move-line-next)
    '("J" . meep-move-paragraph-next)

    '("k" . meep-move-line-prev)
    '("K" . meep-move-paragraph-prev)

    '("l" . meep-move-char-next)
    '("L" . meep-move-line-non-space-end)

    '(";" . meep-move-matching-bracket-inner)
    '(":" . meep-move-matching-bracket-outer)

    '("'" . meep-move-matching-syntax-inner)
    '("\"" . meep-move-matching-syntax-outer)

    ;; Right Hand: Row 3.
    '("n" . meep-move-symbol-prev)
    '("N" . meep-move-same-syntax-and-space-prev)

    '("m" . meep-isearch-repeat-next)
    '("M" . my-key-free)

    '("," . meep-isearch-repeat-prev)
    '("<" . my-key-free)

    '("." . meep-move-symbol-next)
    '(">" . meep-move-same-syntax-and-space-next)

    '("/" . meep-move-symbol-next-end)
    '("?" . meep-move-same-syntax-and-space-next-end)

    ;; Other keys.
    '("\\" . meep-register-jump-to)
    '("|" . meep-register-kmacro-start-or-end)

    '("[" . meep-move-to-bounds-of-thing-beginning)
    '("]" . meep-move-to-bounds-of-thing-end)

    '("-" . meep-region-syntax-contract)
    '("=" . meep-region-syntax-expand)

    '("<tab>" . meep-indent-rigidly)

    '("S-<delete>" . meep-join-line-prev)
    '("S-<backspace>" . meep-join-line-next)

    '("<home>" . meep-move-line-beginning)
    '("<end>" . meep-move-line-end))

  (my-meep-keymap-set-many meep-state-keymap-normal)

  (my-meep-keymap-set-many meep-state-keymap-visual)

  (my-meep-keymap-set-many meep-state-keymap-insert '("<escape>" . bray-state-stack-pop))

  (my-meep-keymap-set-many meep-clipboard-register-map
    '("e" . meep-clipboard-register-cut)
    '("r" . meep-clipboard-register-yank)
    '("t" . meep-clipboard-register-copy)))

;; ---------------------------------------------------------------------------
;; Key-map Clear Defaults

(when my-meep-is-test-env

  ;; Useful for testing.
  (defun undefined-report (&rest ignore)
    (message "UNDEFINED %s" (key-description (this-single-command-keys))))
  (advice-add 'undefined :around #'undefined-report)
  (debug-on-entry 'undefined-report)

  ;; Un-map most keys.
  ;; Since tests should not interfere with them.
  (dolist (key
           (list
            ;; electric indent mode, gets in way of evil-style keys for plugins
            "\C-j" "\C-k" "\C-m" "\C-u" "\C-w"
            "\C-e" ;; Was `move-end-of-line'
            "\C-a" ;; Was `move-beginning-of-line'
            "\C-r" ;; Was `isearch-backward'
            "\C-z" ;; Was `suspend-frame'.
            "\C-d" ;; Was `delete-char'.
            "\C-y" ;; Was `yank'.
            "\M-f" ;; Was `forward-word'.
            "\M-u" ;; Free.
            "\C-n" ;; Was `next-line'.
            "\C-p" ;; Was `previous-line'.
            "\C-s" ;; Was `isearch-forward'.
            "\M-t" ;; Was `transpose-words'.
            "\M-e" ;; Was `forward-sentence'.
            "\C-t" ;; Was `transpose-chars'.
            "\C-\M-t" ;; Was `transpose-sexps'.
            "\M-z" ;; Was zap-to-char (native emacs).
            "\M-\ " ;; Was just-one-space (native emacs).
            "\C-_" ;; Was undo.
            "\C-v" ;; Was `scroll-up-command'.
            "\C-M-j" ;; Was `mark-defun'.
            "\C-M-h" ;; Was `mark-defun'.
            "\C-M-k" ;; Was `kill-sexp'.
            "\C-M-l" ;; Was `reposition-window'.
            "\M-\^?" ;; Was `backward-kill-word' (M-<delete>)
            "\M-j" ;; Was `default-indent-new-line'.
            "\M-k" ;; Was `kill-sentence'.
            "\M-w" ;; Was `kill-ring-save'.
            "\t" ;; Was `indent-for-tab-command'.

            (kbd "C-<backspace>") ;; Was `backward-kill-word'.
            (kbd "C-<delete>") ;; Was `backward-kill-word'.
            (kbd "C-<right>") ;; Was `right-word'.
            (kbd "C-<left>") ;; Was `left-word'.

            (kbd "C-/") ;; Was undo.

            (kbd "<escape>") ;; Needed in the command line for some reason.

            (kbd "<home>") (kbd "<end>") (kbd "S-<home>") (kbd "S-<end>")

            (kbd "C-<backspace>") ;; Was backward-kill-word.
            (kbd "C-<delete>") ;; Was backward-kill-word.
            (kbd "C-/") ;; Was undo.
            (kbd "C-<right>") ;; Was `right-word'.
            (kbd "C-<left>") ;; Was `left-word'.

            ;; F-Keys.
            [f1] [f2] [f3] [f4] [f5] [f6] [f7] [f8] [f9] [f10] [f11] [f12]

            ;; Use find for our own purpose.
            [find]))
    (global-unset-key key)))

(unless my-meep-is-test-env
  (dolist (key
           (list
            "\t" ; MEEP overrides.
            (kbd "<home>") (kbd "<end>") (kbd "S-<home>") (kbd "S-<end>")))
    (global-unset-key key)))


;; ----------------------------------------------------------------------------
;; Emacs Defaults.

;; No need for startup screen.
(setq inhibit-startup-screen t)

;; Don't interrupt the user with things that *might* be confusing.
(setq help-uni-confusables nil)

(setq initial-buffer-choice nil)

;; Don't nag on use of the inactive region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(when my-meep-is-test-env
  ;; Just avoid added overhead.
  (global-eldoc-mode 0)
  ;; This cause a pre-command hook to run that prints a message before each macro.
  ;; Harmless but noisy and unnecessary for tests.
  (tooltip-mode 0)

  ;; Disabled by default in batch mode, needed for tests to work.
  (transient-mark-mode 1))

(unless my-meep-is-test-env
  ;; Disable toolbar.
  (tool-bar-mode -1))


;; ----------------------------------------------------------------------------
;; Emacs Default Modes

;; Give additional hints.
(which-key-mode)


;; ----------------------------------------------------------------------------
;; Packages

;; Use relative paths for packages we depend on.
(when my-meep-is-test-env
  (let ((meep-tests-basedir (file-name-concat (file-name-directory load-file-name) ".." "..")))
    (add-to-list 'load-path meep-tests-basedir)
    (require 'meep)
    (add-to-list 'load-path (file-name-concat meep-tests-basedir ".." "bray"))
    (require 'bray)
    (add-to-list 'load-path (file-name-concat meep-tests-basedir ".." "repeat-fu"))
    (require 'repeat-fu)

    (add-hook 'bray-mode-hook (lambda () (repeat-fu-mode (or bray-mode 0))))))

(defun printf (&rest args)
  (princ (apply #'format args) #'external-debugging-output))


;; Setup packages we depend on.
(unless my-meep-is-test-env

  ;; Load locally.
  (let ((meep-tests-basedir (file-name-concat (file-name-directory load-file-name) ".." "..")))
    (add-to-list 'load-path meep-tests-basedir)
    (require 'meep))

  ;; Defer loading packages by default. Why?
  ;; .. faster startup for packages which are only activated on certain modes or key bindings.
  (setq use-package-always-defer t)

  (with-eval-after-load 'package
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

  (package-initialize)

  ;; Add the ability to upgrade all packages. Why?
  ;; .. adds a quick way to upgrade everything at once.
  (use-package package-utils
    :commands (package-utils-upgrade-all-and-recompile)
    :ensure t)

  ;; Nice theme from VIM Why?
  ;; .. personal preference.
  (use-package inkpot-theme
    :demand t
    :ensure t
    :config (load-theme 'inkpot t))

  (use-package bray
    :commands (bray-mode)
    :ensure t)

  (use-package repeat-fu
    :commands (repeat-fu-mode)
    :ensure t
    :config (setq repeat-fu-preset 'meep)
    :hook ((bray-mode . (lambda () (repeat-fu-mode (or bray-mode 0))))))

  ;; Not exactly core functionality, but nice to have.
  (use-package avy
    :commands (avy-goto-symbol-1-above avy-goto-symbol-1-below)
    :ensure t)

  (use-package command-log-mode
    :ensure t

    :config
    ;; Too small by default.
    (setq command-log-mode-window-size 60)
    ;; Always show the log.
    (add-hook
     'command-log-mode-hook
     (lambda ()
       (when command-log-mode
         (clm/open-command-log-buffer))))))


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
     ;; (set-mark (point))
     (deactivate-mark)

     ;; Testing this out!
     ;; VIM style '^' register for when we leave insert mode.
     (let ((reg ?^))
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
  (define-key meep-state-keymap-motion [remap self-insert-command] 'my-key-free)

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

(unless my-meep-is-test-env

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
  (load custom-file t t))

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
;;; init.el ends here
