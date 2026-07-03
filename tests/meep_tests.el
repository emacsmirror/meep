;;; meep_tests.el --- Testing -*- lexical-binding: t; coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-meep
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Integration tests for MEEP modal editing commands.
;; Tests run in batch mode via meep_tests.py wrapper.

;;; Usage

;; Run via: python tests/meep_tests.py

;;; Code:

;; NOTE: regarding `inhibit-redisplay' some tests disable this because the
;; `indent-rigidly-*' tests exit its transient keymap with `C-a' (unbound
;; here); the resulting `undefined' command forces a ~100ms redisplay per
;; call even in batch.  Binding it to t skips that (~130x faster) without
;; affecting buffer/point assertions.

;; ---------------------------------------------------------------------------
;; Message Capture (suppress minibuffer noise during tests)

(defvar meep-test--captured-messages nil
  "List of messages captured during test execution (newest first).")

(defun meep-test--message-capture (format-string &rest args)
  "Capture message instead of displaying.
Stores formatted message in `meep-test--captured-messages'."
  (when format-string
    (push (apply #'format format-string args) meep-test--captured-messages))
  ;; Return nil like `message' does when format-string is nil.
  nil)

(defun meep-test--set-message-handler (message)
  "Handler for `set-message-function' that suppresses MESSAGE display.
Returns t to indicate the message was handled (prevents echo area display)."
  (when message
    (push message meep-test--captured-messages))
  t)

(defun meep-test-messages ()
  "Return captured messages in chronological order."
  (nreverse (copy-sequence meep-test--captured-messages)))

;; NOTE: Do not remove this function even if it appears unused.
;; It is provided for interactive debugging and for tests that need to
;; check messages at multiple points within a single test body.
(defun meep-test-messages-clear ()
  "Clear captured messages."
  (setq meep-test--captured-messages nil))

;; Store original `read-char' for wrapper.
(defvar meep-test--original-read-char (symbol-function 'read-char))
(defvar meep-test--original-read-char-from-minibuffer (symbol-function 'read-char-from-minibuffer))

(defun meep-test--read-char-no-prompt (&optional _prompt &rest args)
  "Call `read-char' without displaying the prompt.
PROMPT is ignored, ARGS are passed through."
  (apply meep-test--original-read-char nil args))

(defun meep-test--read-char-from-minibuffer-no-prompt (&optional _prompt &rest args)
  "Call `read-char-from-minibuffer' without displaying the prompt.
PROMPT is ignored, ARGS are passed through."
  (apply meep-test--original-read-char-from-minibuffer nil args))

(defmacro with-meep-test-message-capture (&rest body)
  "Execute BODY with messages captured instead of displayed.
Messages are stored in `meep-test--captured-messages'.
Use `meep-test-messages' to get them in chronological order.
Also suppresses minibuffer prompts from interactive specs."
  (declare (indent 0))
  `(let ((meep-test--captured-messages nil)
         (inhibit-message t)
         (echo-keystrokes 0)
         ;; Intercept echo area messages including interactive prompts.
         (set-message-function #'meep-test--set-message-handler)
         (clear-message-function #'ignore)
         (orig-message (symbol-function 'message))
         (orig-read-char (symbol-function 'read-char))
         (orig-read-char-from-minibuffer (symbol-function 'read-char-from-minibuffer))
         ;; Save tooltip-mode state to restore later.
         ;; When enabled, `execute-kbd-macro' calls `tooltip-hide' which calls
         ;; `(message "")' to clear the echo area, polluting captured messages.
         (orig-tooltip-mode (bound-and-true-p tooltip-mode)))
     (unwind-protect
         (progn
           (fset 'message #'meep-test--message-capture)
           (fset 'read-char #'meep-test--read-char-no-prompt)
           (fset 'read-char-from-minibuffer #'meep-test--read-char-from-minibuffer-no-prompt)
           (when (fboundp 'tooltip-mode)
             (tooltip-mode 0))
           ,@body)
       (fset 'message orig-message)
       (fset 'read-char orig-read-char)
       (fset 'read-char-from-minibuffer orig-read-char-from-minibuffer)
       (when (fboundp 'tooltip-mode)
         (tooltip-mode
          (cond
           (orig-tooltip-mode
            1)
           (t
            0)))))))

(defmacro should-error-with-message (form error-type expected-message)
  "Assert FORM signals an error of ERROR-TYPE with EXPECTED-MESSAGE."
  (declare (indent 1))
  (let ((err-sym (make-symbol "err")))
    `(let ((,err-sym (should-error ,form :type ,error-type)))
       (should (equal ,expected-message (error-message-string ,err-sym))))))

;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(require 'seq)
(require 'bray)
(require 'meep)
;; Loads the auto-generated `meep-region-mark-KIND-{inner,outer}' commands.
(require 'meep-region-mark)

;; Key bindings organized by state.
;; Commands are looked up dynamically from bray's state keymaps.

(defvar meep-test-state-commands
  '((normal
     .
     (meep-char-insert
      meep-char-replace
      meep-clipboard-killring-copy
      meep-clipboard-killring-cut
      meep-clipboard-killring-cut-line
      meep-clipboard-killring-yank-pop-stack
      meep-delete-char-ring-next
      meep-delete-char-ring-prev
      meep-delete-char-ring-yank
      meep-insert
      meep-insert-append
      meep-insert-at-last
      meep-insert-change
      meep-insert-change-lines
      meep-insert-line-beginning
      meep-insert-line-end
      meep-insert-open-above
      meep-insert-open-below
      meep-digit-argument-repeat
      meep-indent-rigidly
      meep-insert-overwrite
      meep-isearch-at-point-next
      meep-isearch-at-point-prev
      meep-isearch-repeat-next
      meep-isearch-repeat-prev
      meep-join-line-next
      meep-join-line-prev
      meep-move-by-sexp-out-next
      meep-move-by-sexp-out-prev
      meep-move-by-sexp-over-next
      meep-move-by-sexp-over-prev
      meep-move-char-next
      meep-move-char-prev
      meep-move-find-char-on-line-at-next
      meep-move-find-char-on-line-at-prev
      meep-move-find-char-on-line-repeat-at-next
      meep-move-find-char-on-line-till-next
      meep-move-find-char-on-line-till-prev
      meep-move-line-beginning
      meep-move-line-next
      meep-move-line-non-space-beginning
      meep-move-line-non-space-end
      meep-move-line-prev
      meep-move-matching-bracket-inner
      meep-move-matching-bracket-outer
      meep-move-matching-syntax-inner
      meep-move-matching-syntax-outer
      meep-move-paragraph-next
      meep-move-paragraph-prev
      meep-move-sentence-next
      meep-move-sentence-prev
      meep-move-symbol-next
      meep-move-symbol-next-end
      meep-move-symbol-prev
      meep-move-symbol-prev-end
      meep-move-to-bounds-of-comment
      meep-move-to-bounds-of-comment-inner
      meep-move-to-bounds-of-line
      meep-move-to-bounds-of-line-inner
      meep-move-to-bounds-of-paragraph
      meep-move-to-bounds-of-sentence
      meep-move-to-bounds-of-string
      meep-move-to-bounds-of-string-inner
      meep-move-word-next
      meep-move-word-next-end
      meep-move-word-prev
      meep-move-word-prev-end
      meep-region-activate-and-reverse
      meep-region-activate-and-reverse-motion
      meep-region-activate-or-reverse
      meep-region-expand-to-line-bounds
      meep-region-mark-bounds-of-char-inner
      meep-region-mark-bounds-of-char-outer
      meep-region-swap
      meep-region-syntax-contract
      meep-region-syntax-expand
      meep-region-to-secondary-selection
      meep-region-toggle
      meep-space-shrink-contextual
      meep-transpose
      meep-region-toggle-rectangle))
    (insert
     .
     (meep-delete-char-next
      meep-delete-char-prev
      meep-delete-same-syntax-next
      meep-delete-same-syntax-or-symbol-next
      meep-delete-same-syntax-or-symbol-prev
      meep-delete-same-syntax-prev
      meep-delete-symbol-next
      meep-delete-symbol-prev))
    (visual
     .
     (meep-char-insert
      meep-char-replace
      meep-clipboard-killring-copy
      meep-clipboard-killring-cut
      meep-clipboard-killring-yank-pop-stack
      meep-insert-change
      meep-insert-into-last-copy
      meep-insert-into-last-move
      meep-isearch-repeat-next
      meep-isearch-repeat-prev
      meep-move-char-next
      meep-move-char-prev
      meep-move-line-next
      meep-move-line-prev
      meep-move-symbol-next
      meep-move-symbol-next-end
      meep-move-symbol-prev
      meep-move-word-next
      meep-move-word-prev
      meep-region-activate-and-reverse
      meep-region-activate-and-reverse-motion
      meep-region-activate-or-reverse
      meep-region-disable
      meep-region-expand-to-line-bounds
      meep-region-mark-bounds-of-char-inner
      meep-region-mark-bounds-of-char-outer
      meep-region-swap
      meep-region-syntax-contract
      meep-region-syntax-expand
      meep-region-to-secondary-selection
      meep-region-toggle-rectangle)))
  "Alist mapping states to lists of commands whose keys should be looked up.")

(defun meep-test-get-keymaps-for-state (state)
  "Return the active keymaps for STATE from `bray-state-definitions'."
  (let ((state-def
         (seq-find (lambda (def) (eq (plist-get def :id) state)) bray-state-definitions)))
    (cond
     (state-def
      ;; Extract keymap symbols and resolve to actual keymaps.
      (mapcar (lambda (entry) (symbol-value (cdr entry))) (plist-get state-def :keymaps)))
     (t
      (error "No bray state definition found for: %s" state)))))

(defun meep-test-lookup-key (state command)
  "Find the key sequence bound to COMMAND in STATE's keymaps."
  (let* ((keymaps (meep-test-get-keymaps-for-state state))
         (key (seq-some (lambda (keymap) (where-is-internal command keymap t)) keymaps)))
    (cond
     (key
      key)
     (t
      (error "No key binding for `%s' in state `%s'" command state)))))

(defun meep-test-keymap-alist-create ()
  "Build the alist mapping states to their command-to-key bindings.
Keys are looked up dynamically from bray's state keymaps."
  (mapcar
   (lambda (state-entry)
     (let ((state (car state-entry))
           (commands (cdr state-entry)))
       (cons state (mapcar (lambda (cmd) (cons cmd (meep-test-lookup-key state cmd))) commands))))
   meep-test-state-commands))

(defvar meep-test-keymap-alist nil
  "Alist mapping states to their command-to-key bindings.
Initialized by `meep-test-keymap-alist-init'.")

(defun meep-test-keymap-alist-init ()
  "Initialize `meep-test-keymap-alist' from bray's state keymaps."
  (setq meep-test-keymap-alist (meep-test-keymap-alist-create)))

(defun meep-test-key (state command)
  "Look up the key for COMMAND in STATE.
First checks `meep-test-keymap-alist', then falls back to direct keymap lookup."
  (let ((state-bindings (alist-get state meep-test-keymap-alist)))
    (or
     ;; Try cached alist first.
     (and state-bindings (alist-get command state-bindings))
     ;; Fall back to direct keymap lookup for commands bound in default keymap.
     (meep-test-lookup-key state command))))

(defun meep-test-key-deferred (state command)
  "Return a deferred key spec for COMMAND in STATE.
The spec is resolved later by `simulate-input-for-meep', which
asserts that the current bray state matches STATE at execution time."
  (list 'meep-test-key-deferred state command))

(defun meep-test-key-deferred-p (obj)
  "Return non-nil if OBJ is a deferred key spec."
  (and (listp obj) (eq (car obj) 'meep-test-key-deferred)))

;; These variables are defined at top-level so `meep-test--pre-command-check'
;; can access them, but are dynamically bound (shadowed) by
;; `simulate-input-for-meep' to provide per-invocation state.

(defvar meep-test--expected-states nil
  "Queue of (command . expected-state) pairs for deferred validation.")

(defvar meep-test--deferred-error nil
  "Stores any error encountered during deferred state validation.
Errors cannot be signaled directly from `pre-command-hook' as Emacs
catches and reports them without propagating.  Instead, errors are
stored here and signaled after the keyboard macro completes.")

(defun meep-test--pre-command-check ()
  "Validate state before command execution if an expectation is queued.
Called via `pre-command-hook' during deferred input simulation.
Only checks when `this-command' matches the next expected command,
allowing intermediate commands (like self-insert) to pass through."
  (when (and meep-test--expected-states (not meep-test--deferred-error))
    (let ((expectation (car meep-test--expected-states)))
      (when (eq this-command (car expectation))
        (let ((expected-state (cdr expectation))
              (actual-state (bray-state)))
          (unless (eq actual-state expected-state)
            (setq meep-test--deferred-error
                  (format
                   "State mismatch: expected `%s' but current state is `%s' (for command `%s')"
                   expected-state actual-state this-command)))
          (pop meep-test--expected-states))))))

(defmacro with-temp-hook (hook function &rest body)
  "Execute BODY with FUNCTION temporarily added to HOOK.
FUNCTION is removed from HOOK when BODY completes, even if an error occurs."
  (declare (indent 2))
  `(progn
     (add-hook ,hook ,function)
     (unwind-protect
         (progn
           ,@body)
       (remove-hook ,hook ,function))))

(defmacro simulate-input-deferred-eval (&rest keys)
  "Simulate input with deferred state validation at execution time.

Keys created with `meep-test-key-deferred' have their expected state
validated via `pre-command-hook' at the moment each command executes.

All keys execute as a single keyboard macro.  This is required because
some meep commands (e.g. `meep-region-activate-and-reverse-motion')
check `last-command' to determine the previous motion; executing keys
separately causes these commands to fail.

Plain strings and key vectors pass through without state validation."
  (declare (indent 0))
  `(let ((keys-list (list ,@keys))
         (resolved-keys nil)
         ;; Shadow the global variables to provide per-invocation state.
         (meep-test--expected-states nil)
         (meep-test--deferred-error nil))
     ;; Build resolved keys and queue expected states for deferred specs.
     (dolist (key keys-list)
       (cond
        ((meep-test-key-deferred-p key)
         (let ((state (nth 1 key))
               (command (nth 2 key)))
           (push (cons command state) meep-test--expected-states)
           (push (meep-test-key state command) resolved-keys)))
        (t
         (push key resolved-keys))))
     (setq resolved-keys (nreverse resolved-keys))
     (setq meep-test--expected-states (nreverse meep-test--expected-states))
     ;; Execute with hook for state validation.
     (with-temp-hook 'pre-command-hook #'meep-test--pre-command-check
       (simulate-input
         (apply #'vconcat resolved-keys)))
     ;; Signal any deferred error after execution completes.
     (when meep-test--deferred-error
       (error "%s" meep-test--deferred-error))))

(defmacro simulate-input-for-meep (&rest body)
  "Execute BODY as simulated input with meep-aware expansion.

Quoted plists with :state and :command properties expand to
`meep-test-key-deferred' calls.  All other forms pass through unchanged.

Example:

  (simulate-input-for-meep
    \\='(:state normal :command meep-move-char-next)
    \"3\"
    \\='(:state visual :command meep-region-swap))

expands to:

  (simulate-input-deferred-eval
    (meep-test-key-deferred \\='normal \\='meep-move-char-next)
    \"3\"
    (meep-test-key-deferred \\='visual \\='meep-region-swap))

Both :state and :command are required in each plist.  Unknown keys
or missing required keys signal an error at runtime."
  (declare (indent 0))
  (let ((transformed-forms
         (mapcar
          (lambda (form)
            (cond
             ;; Quoted plist with keyword as first element.
             ((and (consp form)
                   (eq (car form) 'quote)
                   (consp (cadr form))
                   (keywordp (car (cadr form))))
              (let ((plist (cadr form)))
                `(simulate-input-for-meep--validate-and-create ',plist)))
             (t
              form)))
          body)))
    `(simulate-input-deferred-eval
       ,@transformed-forms)))

(defun simulate-input-for-meep--validate-and-create (plist)
  "Validate PLIST and create a deferred key spec.
PLIST must contain exactly :state and :command keys."
  (let ((state (plist-get plist :state))
        (command (plist-get plist :command))
        (unknown-keys nil))
    ;; Check for unknown keys.
    (let ((rest plist))
      (while rest
        (let ((key (car rest)))
          (unless (memq key '(:state :command))
            (push key unknown-keys)))
        (setq rest (cddr rest))))
    ;; Report errors.
    (cond
     (unknown-keys
      (error "simulate-input-for-meep: unknown key(s) %s in %S; expected :state and :command"
             (mapconcat #'symbol-name (nreverse unknown-keys) ", ")
             plist))
     ((and (null state) (null command))
      (error "simulate-input-for-meep: missing :state and :command in %S" plist))
     ((null state)
      (error "simulate-input-for-meep: missing :state in %S" plist))
     ((null command)
      (error "simulate-input-for-meep: missing :command in %S" plist))
     (t
      (meep-test-key-deferred state command)))))

;; Commands that need test key bindings (not bound in init/default/init.el).
;; Keys are auto-generated using function keys with modifiers.

(defconst meep-test-extra-bindings
  '( ;; Movement commands.
    (meep-move-word-next . (normal visual))
    (meep-move-word-prev . (normal visual))
    (meep-move-word-prev-end . (normal))
    (meep-move-word-next-end . (normal))
    (meep-move-symbol-prev-end . (normal))
    (meep-move-sentence-prev . (normal))
    (meep-move-sentence-next . (normal))
    (meep-move-by-sexp-over-prev . (normal))
    (meep-move-by-sexp-over-next . (normal))
    (meep-move-by-sexp-out-next . (normal))
    (meep-move-by-sexp-out-prev . (normal))
    (meep-move-list-item-next . (normal))
    (meep-move-list-item-prev . (normal))
    (meep-move-list-item-next-end . (normal))
    (meep-move-list-item-prev-end . (normal))
    ;; Delete commands (tested in insert state).
    (meep-delete-symbol-next . (insert))
    (meep-delete-symbol-prev . (insert))
    (meep-delete-same-syntax-next . (insert))
    (meep-delete-char-next . (insert))
    (meep-delete-char-prev . (insert))
    (meep-delete-same-syntax-prev . (insert))
    (meep-delete-same-syntax-or-symbol-next . (insert))
    (meep-delete-same-syntax-or-symbol-prev . (insert))
    ;; Insert commands.
    (newline . (insert normal))
    (meep-insert-append . (normal))
    ;; Bounds commands.
    (meep-move-to-bounds-of-line . (normal))
    (meep-move-to-bounds-of-line-inner . (normal))
    (meep-move-to-bounds-of-paragraph . (normal))
    (meep-move-to-bounds-of-sentence . (normal))
    (meep-move-to-bounds-of-string . (normal))
    (meep-move-to-bounds-of-string-inner . (normal))
    (meep-move-to-bounds-of-comment . (normal))
    (meep-move-to-bounds-of-comment-inner . (normal))
    (meep-move-to-bounds-of-list-item . (normal))
    (meep-move-to-bounds-of-list-item-inner . (normal))
    ;; List item text-object mark commands.
    (meep-region-mark-list-item-inner . (normal))
    (meep-region-mark-list-item-outer . (normal))
    ;; Region commands.
    (meep-region-activate-or-reverse . (normal visual))
    (meep-region-disable . (visual))
    ;; Clipboard register commands.
    (meep-clipboard-register-yank-lines . (normal visual)))
  "Commands needing test bindings, with list of states for each.
Each entry is (COMMAND . STATES) where STATES is a list of state symbols.
Commands get the same key in all specified states for consistency.")

(defun meep-test-generate-key (index)
  "Generate a unique key string for INDEX.
Uses function keys f5-f12 with modifier combinations.
Provides 72 unique keys (9 modifiers x 8 function keys)."
  (let* ((fkey-num (+ 5 (mod index 8))) ; f5 through f12
         (modifier-index (/ index 8))
         (modifiers ["" "S-" "C-" "M-" "C-S-" "M-S-" "C-M-" "C-M-S-" "H-"]))
    (cond
     ((>= modifier-index (length modifiers))
      (error "Too many test bindings (%d), max is %d" (1+ index) (* 8 (length modifiers))))
     (t
      (format "%s<f%d>" (aref modifiers modifier-index) fkey-num)))))

(defun meep-test-keymap-for-state (state)
  "Return the keymap variable for STATE."
  (pcase state
    ('normal 'meep-state-keymap-normal)
    ('visual 'meep-state-keymap-visual)
    ('insert 'meep-state-keymap-insert)
    (_ (error "Unknown state: %s" state))))

(defun meep-test-bind-all-commands ()
  "Bind all commands in `meep-test-extra-bindings' to auto-generated keys.
Each command gets the same key across all its specified states.
Signals an error if any key is already bound in the target keymap."
  (let ((index 0))
    (dolist (entry meep-test-extra-bindings)
      (let ((cmd (car entry))
            (states (cdr entry))
            (key (kbd (meep-test-generate-key index))))
        (dolist (state states)
          (let* ((keymap-sym (meep-test-keymap-for-state state))
                 (keymap (and (boundp keymap-sym) (symbol-value keymap-sym))))
            (when keymap
              ;; Error if key already bound.
              (let ((existing (lookup-key keymap key)))
                (when (and existing (not (numberp existing)))
                  (error "Test binding conflict: %s is already bound to `%s' in %s (wanted `%s')"
                         (key-description key)
                         existing
                         keymap-sym
                         cmd)))
              (define-key keymap key cmd))))
        (setq index (1+ index))))))

(meep-test-bind-all-commands)

;; Initialize keymap alist after bray is set up.
(meep-test-keymap-alist-init)

(defun meep-test-position-as-line-column (pos)
  "Return position POS as (line . column) cons cell.
Line is 1-based, column is 0-based character index."
  (save-excursion
    (goto-char pos)
    (cons (line-number-at-pos) (current-column))))

(defun meep-test-point-line-column ()
  "Return point as (line . column) cons cell."
  (meep-test-position-as-line-column (point)))

(defun meep-test-mark-line-column ()
  "Return mark as (line . column) cons cell, or nil if no mark."
  (when (mark t)
    (meep-test-position-as-line-column (mark t))))

(defun meep-test-region-as-string ()
  "Return the region contents as a string.
Signals an error if no region is active."
  (unless (region-active-p)
    (error "meep-test-region-as-string: no active region"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

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
  "Run BODY in a temporary buffer with INITIAL-BUFFER-TEXT.
Messages are captured and not displayed.  Use `meep-test-messages'
to retrieve captured messages for validation."
  (declare (indent 1))
  `(with-meep-test-message-capture
     (let ((buf (generate-new-buffer "untitled"))
           ;; Isolate `kill-ring' state for clipboard tests.
           (kill-ring nil)
           (kill-ring-yank-pointer nil))

       (switch-to-buffer buf)
       (buffer-reset-text ,initial-buffer-text)
       (prog1 (progn
                ,@body)
         (kill-buffer buf)))))

;; Suppress bell during tests.
(setq ring-bell-function #'ignore)

;; NOTE: Do not remove these debugging helpers even if they appear unused.
;; They are useful for interactive debugging during test development.
(defun meep_tests-my-local-ring ()
  "Utility to troubleshoot ringing the bell.
Set `ring-bell-function' to this to see when the bell rings."
  (printf "<TEST RING>\n"))

(defun meep_tests-print-state ()
  "Log the current state for debugging."
  (printf
   "log: %S %S (%S) %d-%d \"%s\"\n"
   bray-mode
   (bray-state)
   (buffer-name)
   (mark)
   (point)
   (buffer-string)))

;; ---------------------------------------------------------------------------
;; Tests

(ert-deftest primitive-insert-word ()
  "Insert text in insert mode.

Verifies: basic text insertion works correctly."
  (let ((text-initial "")
        (text-expected "hello"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        "hello"
        '(:state insert :command bray-state-stack-pop))

      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-word-single ()
  "Ensure changing a word works."
  (let ((text-initial "foo bar baz")
        (text-expected "foo BUZ baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-insert-change)
        "BUZ"
        '(:state insert :command bray-state-stack-pop))

      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-word-multiple ()
  "Ensure changing multiple words works."
  (let ((text-initial "foo bar baz ~")
        (text-expected "foo BUZ ~"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end)
        "1"
        '(:state normal :command meep-insert-change)
        "BUZ"
        '(:state insert :command bray-state-stack-pop))

      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-word-multiple-adjust ()
  "Ensure changing a words works with an adjustment before the change."
  (let ((text-initial "foo bar bazX ~")
        (text-expected "foo BUZX ~"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)

      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end)
        "1" ; A second word.
        '(:state normal :command meep-move-char-prev) ; One less character.
        '(:state normal :command meep-insert-change)
        "BUZ"
        '(:state insert :command bray-state-stack-pop))

      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-comment-bounds-inner-cc ()
  "Ensure change in inner bounds works."
  (let ((text-initial "/* Hello World. */")
        (text-expected "/* Changed! */"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-matching-syntax-inner)
        '(:state normal :command meep-region-activate-and-reverse-motion)
        '(:state visual :command meep-insert-change)
        "Changed!"
        '(:state insert :command bray-state-stack-pop))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-comment-bounds-outer-cc ()
  "Ensure change in outer bounds works."
  (let ((text-initial "/* Hello World. */")
        (text-expected "// Changed!"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-matching-syntax-outer)
        '(:state normal :command meep-region-activate-and-reverse-motion)
        '(:state visual :command meep-insert-change)
        "// Changed!"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-replace-char-region ()
  "Ensure replace char with rectangle mark mode."
  (let ((text-initial "......\n")
        (text-expected ".####.\n"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-char-replace)
        "#")
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
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-char-replace)
        "#")
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-replace-char-end-of-line ()
  "Replace char with point at end of line (no region) is a clean error.

Regression: the no-region fallback spanned the newline, so the column
difference went negative and `make-string' signaled `wrong-type-argument'
instead of a clear message."
  (let ((text-initial "abc\ndef\n"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Point at the end of line 1 (on the newline), not end of buffer.
      (goto-char 4)
      (should (eolp))
      (should-not (eobp))
      (should-error-with-message
          (simulate-input-for-meep
            '(:state normal :command meep-char-replace)
            "x")
        'user-error
        "Cannot replace at the end of the line")
      ;; Buffer is unchanged.
      (should (equal text-initial (buffer-string))))))

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
    (dolist (use-real-insert '(nil t))
      (let* ((meep-repeat-fu-replay use-real-insert)
             ;; Real insert exits insert state; string-rectangle exits the minibuffer.
             (exit-key
              (cond
               (use-real-insert
                (meep-test-key-deferred 'insert 'bray-state-stack-pop))
               (t
                [return]))))
        (with-meep-test text-initial
          (text-mode)
          (bray-mode 1)
          (simulate-input-for-meep
            '(:state normal :command meep-move-char-next)
            '(:state normal :command meep-move-line-next)
            '(:state normal :command meep-region-toggle-rectangle)
            '(:state visual :command meep-move-char-next)
            "3"
            '(:state visual :command meep-move-line-next)
            '(:state visual :command meep-insert-change)
            "####"
            exit-key)
          (should (equal 'normal (bray-state)))
          (should (equal text-expected (buffer-string)))
          ;; Point at end of inserted text on the last affected line.
          (should (equal '(3 . 5) (meep-test-point-line-column))))))))

(ert-deftest primitive-change-region-rectangle-reversed ()
  "Ensure insert-change works with rectangle mark mode (point on first line)."
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
    (dolist (use-real-insert '(nil t))
      (let* ((meep-repeat-fu-replay use-real-insert)
             ;; Real insert exits insert state; string-rectangle exits the minibuffer.
             (exit-key
              (cond
               (use-real-insert
                (meep-test-key-deferred 'insert 'bray-state-stack-pop))
               (t
                [return])))

             ;; Real insert edits the line at point (line 2);
             ;; string-rectangle always leaves point on the last line.
             (point-expected
              (cond
               (use-real-insert
                '(2 . 5))
               (t
                '(3 . 5)))))
        (with-meep-test text-initial
          (text-mode)
          (bray-mode 1)
          ;; Select rectangle from bottom-right to top-left (point on first line).
          (simulate-input-for-meep
            '(:state normal :command meep-move-char-next)
            '(:state normal :command meep-move-line-next)
            '(:state normal :command meep-move-line-next)
            '(:state normal :command meep-region-toggle-rectangle)
            '(:state visual :command meep-move-char-next)
            "3"
            '(:state visual :command meep-move-line-prev)
            '(:state visual :command meep-insert-change)
            "####"
            exit-key)
          (should (equal 'normal (bray-state)))
          (should (equal text-expected (buffer-string)))
          (should (equal point-expected (meep-test-point-line-column))))))))

(ert-deftest primitive-change-region-rectangle-real-insert-newline ()
  "Ensure real insert with newline replays correctly (top-to-bottom)."
  (let ((meep-repeat-fu-replay t)
        (text-initial
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
          ".##\n"
          "##.\n"
          ".##\n"
          "##.\n"
          "......\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select rectangle: point on first line (top-to-bottom replay).
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-prev)
        '(:state visual :command meep-insert-change)
        "##"
        '(:state insert :command newline)
        "##"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-region-rectangle-real-insert-newline-reversed ()
  "Ensure real insert with newline replays correctly (bottom-to-top)."
  (let ((meep-repeat-fu-replay t)
        (text-initial
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
          ".##\n"
          "##.\n"
          ".##\n"
          "##.\n"
          "......\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select rectangle: point on last line (bottom-to-top replay).
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-insert-change)
        "##"
        '(:state insert :command newline)
        "##"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest primitive-change-region-rectangle-real-insert-undo ()
  "Ensure undo reverts the entire rectangle edit (delete + edit + replay) in one step."
  (let ((meep-repeat-fu-replay t)
        (text-initial
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
      ;; Select rectangle: point on first line (top-to-bottom replay).
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-prev)
        '(:state visual :command meep-insert-change)
        "####"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string)))
      ;; A single undo should revert the entire operation.
      (undo)
      (should (equal text-initial (buffer-string))))))

;; Clipboard Tests
;;
;; These tests verify meep's `kill-ring' clipboard operations including:
;; - Region copy/cut/paste (text selected via motion)
;; - Line-wise operations (entire lines as units)
;; - Rectangle operations (2D block selections)
;; - Implied regions (meep's `mark-on-motion' feature)
;; - Error handling for edge cases
;;
;; Key behaviors tested:
;; - Copy preserves original text, cut removes it.
;; - Line-wise paste inserts whole lines regardless of cursor column.
;; - Rectangle paste inserts at cursor column, creating new lines as needed.
;; - Motion commands create an "implied region" that cut/copy can use.
;; - Visual mode operations return to normal state afterward.

;; ---------------------------------------------------------------------------
;; Region Copy/Cut/Paste
;;
;; Basic clipboard operations on text selected via motion commands.
;; Motion from point A to point B creates a region; copy/cut operates on it.

(ert-deftest clipboard-region-copy-paste ()
  "Copy selected text and paste it elsewhere; original remains intact.

Workflow: select 'bar' via motion, copy it, move to end, paste.
Verifies: copy does not modify source, kill-ring contains copied text,
paste inserts at point."
  (let ((text-initial "foo bar baz")
        ;;            "foo bar " + "bar" + "baz" = "foo bar barbaz"
        ;;                         ^^^^ pasted
        (text-expected "foo bar barbaz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Select "bar": move to start of bar, then to end of bar.
      ;; This creates an implied region spanning "bar".
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-clipboard-killring-copy))
      ;; Verify `kill-ring' contains "bar".
      (should (equal "bar" (car kill-ring)))
      ;; Move to start of "baz" and paste "bar" before it.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

(ert-deftest clipboard-region-cut-paste ()
  "Cut selected text and paste it elsewhere; original is removed.

Workflow: select 'bar' via motion, cut it, move forward, paste.
Verifies: cut removes source text, kill-ring contains cut text,
paste inserts at new location."
  (let ((text-initial "foo bar baz")
        ;;            "foo " + " " + "bar" + "baz" = "foo  barbaz"
        ;;                 ^^^ bar removed    ^^^ bar pasted before baz
        (text-expected "foo  barbaz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Select and cut "bar".
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-clipboard-killring-cut))
      ;; Verify `kill-ring' contains "bar".
      (should (equal "bar" (car kill-ring)))
      ;; Buffer is now "foo  baz".  Move to "baz" and paste "bar".
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

(ert-deftest clipboard-region-paste-twice ()
  "Paste can be repeated; kill-ring entry is not consumed.

Workflow: copy 'aa', move to end, paste twice.
Verifies: same content can be pasted multiple times."
  (let ((text-initial "aa bb")
        ;;            "aa bb" + "aa" + "aa" = "aa bbaaaa"
        (text-expected "aa bbaaaa"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Copy "aa".
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-clipboard-killring-copy))
      (should (equal "aa" (car kill-ring)))
      ;; Move to end of buffer (after "bb"), paste twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Implied Region (Mark-on-Motion)
;;
;; Meep's `mark-on-motion' feature: motion commands set the mark, creating
;; an "implied region" from mark to point.  Cut/copy can operate on this
;; region without explicit visual selection.

(ert-deftest clipboard-implied-region-cut ()
  "Cut operates on implied region from motion without visual selection.

Meep sets the mark during motion, so moving from A to B creates a
region spanning A-B.  Cut can then operate on this region directly.

Workflow: move across 'bar' (setting mark), cut (uses implied region).
Verifies: motion creates usable region, cut removes that text,
kill-ring contains the cut text."
  (let ((text-initial "foo bar baz")
        ;;            "foo " + "baz" = "foo  baz" (bar removed)
        (text-expected "foo  baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move to "bar" start, then to "bar" end.
      ;; This motion sets the mark, creating an implied region over "bar".
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end))
      ;; Cut without explicit selection; uses implied region.
      (simulate-input-for-meep
        '(:state normal :command meep-clipboard-killring-cut))
      (should (equal "bar" (car kill-ring)))
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Line-wise Operations
;;
;; Line-wise cut/paste treats entire lines as units.  Key behavior:
;; pasting a line always inserts it as a new line above the current line,
;; regardless of cursor column position within that line.

(ert-deftest clipboard-line-cut-relocate ()
  "Cut a whole line and paste it at a different line position.

Workflow: cut line 2 ('bbb'), move to after line 3, paste.
Verifies: cut-line removes entire line including newline,
paste inserts as whole line."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "aaa\n"
          "bbb\n"
          "ccc\n"))
        ;; Line "bbb" moves from position 2 to after "ccc".
        (text-expected
         ;; format-next-line: off
         (concat
          "aaa\n"
          "ccc\n"
          "bbb\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to line 2 and cut it.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-clipboard-killring-cut-line))
      ;; Verify `kill-ring' contains "bbb\n" (line with newline).
      (should (equal "bbb\n" (car kill-ring)))
      ;; Now on line "ccc".  Move down (to empty area after ccc) and paste.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

(ert-deftest clipboard-line-paste-ignores-column ()
  "Line paste inserts as whole line, ignoring cursor column.

When pasting line-wise content, cursor column is irrelevant: the line
is inserted above the current line at column 0.  This differs from
character-wise paste which inserts at the cursor position.

Workflow: cut 'xxx', move to column 2 of 'abcde', paste.
Verifies: paste inserts 'xxx' as line above, not inline at column 2.
If column mattered, result would be 'abxxxcde' (inline insertion)."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "xxx\n"
          "abcde\n"))
        ;; After cutting "xxx\n", buffer is "abcde\n".
        ;; Move to column 2 (on 'c') and paste.
        ;; CORRECT: "xxx" inserted as line above -> "xxx\nabcde\n"
        ;; WRONG:   "xxx" inserted inline at col 2 -> "abxxxcde\n"
        (text-expected
         ;; format-next-line: off
         (concat
          "xxx\n"
          "abcde\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cut line 1 ("xxx").  Buffer becomes "abcde\n".
      (simulate-input-for-meep
        '(:state normal :command meep-clipboard-killring-cut-line))
      ;; Move right to column 2 (on 'c').
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Paste.  Despite cursor being at column 2, line paste should
      ;; insert "xxx" as a whole line above "abcde".
      (simulate-input-for-meep
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Rectangle Operations
;;
;; Rectangle mode selects a 2D block of text.  Key behaviors:
;; - Copy/cut operates on a rectangular region across multiple lines.
;; - Paste inserts the rectangle at the cursor column.
;; - Subsequent rows create new lines, padded with spaces to align.
;; - Visual mode is exited after the operation, returning to normal state.

(ert-deftest clipboard-rectangle-copy-paste ()
  "Copy a rectangle and paste it; shape is preserved.

Rectangle paste inserts the first row at the cursor position (pushing
existing content right), then creates new lines for subsequent rows.

Workflow: select 2x2 rectangle 'AB/CD', copy, paste at line 3 column 0.
Verifies: rectangle shape preserved, paste pushes content right,
returns to normal state after copy."
  (let ((text-initial
         ;;  01234
         ;; format-next-line: off
         (concat
          ".AB.\n" ; line 1: rectangle spans columns 1-2
          ".CD.\n" ; line 2: rectangle spans columns 1-2
          "....\n")) ; line 3: paste destination
        ;; Rectangle "AB" / "CD" pasted at column 0 of line 3:
        ;; - "AB" inserted at column 0, pushing "...." right -> "AB...."
        ;; - "CD" creates new line 4 at column 0
        (text-expected
         ;; format-next-line: off
         (concat
          ".AB.\n"
          ".CD.\n"
          "AB....\n"
          "CD")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select rectangle from (line 1, col 1) to (line 2, col 2).
      ;; Move to column 1, enter rectangle mode, extend to column 3, down 1 line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-clipboard-killring-copy))
      ;; Should return to normal state after copy.
      (should (equal 'normal (bray-state)))
      ;; Move to line 3, column 0, and paste rectangle.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

(ert-deftest clipboard-rectangle-cut-paste ()
  "Cut a rectangle (removes block), paste elsewhere.

Workflow: select 2x2 rectangle 'AB/CD', cut (removes from source), paste.
Verifies: cut removes rectangular block, paste restores it elsewhere,
returns to normal state after cut."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".AB.\n"
          ".CD.\n"
          "....\n"))
        ;; After cut: columns 1-2 removed from lines 1-2 -> ".." / ".."
        ;; After paste at line 3 col 0: "AB...." / "CD"
        (text-expected
         ;; format-next-line: off
         (concat
          "..\n"
          "..\n"
          "AB....\n"
          "CD")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select and cut the AB/CD rectangle.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-clipboard-killring-cut))
      ;; Should return to normal state after cut.
      (should (equal 'normal (bray-state)))
      ;; After cut, cursor is on line 1.  Move to line 3, paste.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

(ert-deftest clipboard-rectangle-paste-mid-column ()
  "Rectangle paste at non-zero column pads new lines with spaces.

When pasting a rectangle at column N, the first row inserts at column N.
Subsequent rows create new lines and pad with spaces to reach column N.

Workflow: copy 'AB/CD', paste at line 3 column 2.
Verifies: new lines are space-padded to maintain column alignment,
returns to normal state after copy."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AB\n"
          "CD\n"
          "......\n"))
        ;; Paste at column 2:
        ;; - Line 3: ".." + "AB" + "...." = "..AB...."
        ;; - Line 4 (new): "  " (spaces) + "CD" = "  CD"
        (text-expected
         ;; format-next-line: off
         (concat
          "AB\n"
          "CD\n"
          "..AB....\n"
          "  CD")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Copy rectangle AB/CD from lines 1-2, columns 0-1.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-clipboard-killring-copy))
      (should (equal 'normal (bray-state)))
      ;; Move to line 3, column 2, then paste.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

(ert-deftest clipboard-rectangle-single-column ()
  "Single-column (1 char wide) rectangle: vertical strip of characters.

A 1-wide rectangle is a vertical selection.  Copy/paste should work
identically to wider rectangles.

Workflow: select column of 'A/B/C', copy, paste at line 4.
Verifies: single-column rectangles work correctly,
returns to normal state after copy."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".A.\n"
          ".B.\n"
          ".C.\n"
          "...\n"))
        ;; Paste "A" / "B" / "C" at column 0 of line 4:
        ;; - "A" pushes "..." right -> "A..."
        ;; - "B" creates new line
        ;; - "C" creates new line
        (text-expected
         ;; format-next-line: off
         (concat
          ".A.\n"
          ".B.\n"
          ".C.\n"
          "A...\n"
          "B\n"
          "C")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select single-column rectangle: column 1 of lines 1-3.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-clipboard-killring-copy))
      (should (equal 'normal (bray-state)))
      ;; Paste at line 4, column 0.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Error Handling
;;
;; Cut/copy should signal an error when there is no region to operate on.
;; This catches user errors (e.g., pressing cut without selecting anything).

(ert-deftest clipboard-error-cut-no-region ()
  "Cut without any region or motion signals an error.

Without a region (either explicit visual selection or implied from motion),
cut has nothing to operate on and should signal an error.

Workflow: invoke cut immediately without any motion.
Verifies: error is signaled, buffer unchanged, kill-ring empty."
  (let ((text-initial "foo bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Attempt cut with no prior motion or selection.
      (should-error-with-message
          (simulate-input-for-meep
            '(:state normal :command meep-clipboard-killring-cut))
        'error
        "The mark is not set now, so there is no region")
      ;; Kill-ring should remain empty.
      (should (null kill-ring))
      ;; Buffer should be unchanged.
      (should (equal text-initial (buffer-string))))))

(ert-deftest clipboard-error-copy-no-region ()
  "Copy without any region or motion signals an error.

Workflow: invoke copy immediately without any motion.
Verifies: error is signaled, kill-ring remains empty."
  (let ((text-initial "foo bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Attempt copy with no prior motion or selection.
      (should-error-with-message
          (simulate-input-for-meep
            '(:state normal :command meep-clipboard-killring-copy))
        'error
        "The mark is not set now, so there is no region")
      ;; Kill-ring should remain empty.
      (should (null kill-ring)))))

(ert-deftest clipboard-cut-inactive-mark ()
  "Cut acts on a set but inactive mark, even with `mark-even-if-inactive' nil.
The region need not be active; reading the mark directly avoids the
`(mark-inactive)' error `region-beginning' would signal, see
`meep--region-or-mark-bounds'."
  (let ((text-initial "foo bar baz")
        (mark-even-if-inactive nil))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Set a mark at the start, move point along, and leave it inactive.
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (+ (point-min) 3))
      (deactivate-mark)
      (should-not (region-active-p))
      ;; Cut operates on the implied region rather than signalling an error.
      (simulate-input-for-meep
        '(:state normal :command meep-clipboard-killring-cut))
      (should (equal "foo" (car kill-ring)))
      (should (equal " bar baz" (buffer-string))))))

(ert-deftest clipboard-only-copy-no-tty-clipboard ()
  "Copy to system clipboard raises error when no clipboard integration.

Simulates a TTY frame where `interprogram-cut-function' is nil.
Verifies: user-error is raised, buffer unchanged."
  (let ((text-initial "foo bar baz")
        (interprogram-cut-function nil))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Set a region so the nil-function check is reached.
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (should-error-with-message
          (meep-clipboard-only-copy)
        'user-error
        "No clipboard integration available in this terminal")
      (should (equal text-initial (buffer-string))))))

(ert-deftest clipboard-only-cut-no-tty-clipboard ()
  "Cut to system clipboard raises error when no clipboard integration.

Simulates a TTY frame where `interprogram-cut-function' is nil.
Verifies: user-error raised before any deletion, buffer unchanged."
  (let ((text-initial "foo bar baz")
        (interprogram-cut-function nil))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Set a region so the nil-function check is reached.
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (should-error-with-message
          (meep-clipboard-only-cut)
        'user-error
        "No clipboard integration available in this terminal")
      ;; Buffer must be unchanged since error fires before delete.
      (should (equal text-initial (buffer-string))))))

(ert-deftest clipboard-only-yank-no-tty-clipboard ()
  "Yank from system clipboard raises error when no clipboard integration.

Simulates a TTY frame where `interprogram-paste-function' is nil.
Verifies: user-error is raised, buffer unchanged."
  (let ((text-initial "foo bar baz")
        (interprogram-paste-function nil))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      (should-error-with-message
          (meep-clipboard-only-yank)
        'user-error
        "No clipboard integration available in this terminal")
      (should (equal text-initial (buffer-string))))))

(ert-deftest clipboard-yank-empty-kill-ring ()
  "Yank when kill-ring is empty shows message.

Workflow: yank immediately with empty kill-ring.
Verifies: buffer unchanged, informative message displayed."
  (let ((text-initial "unchanged"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Kill-ring starts empty.
      (should (null kill-ring))
      ;; Attempt yank.
      (simulate-input-for-meep
        '(:state normal :command meep-clipboard-killring-yank-pop-stack))
      ;; Buffer unchanged.
      (should (equal text-initial (buffer-string)))
      ;; Verify informative message was displayed.
      (should (equal '("Kill ring is empty") (meep-test-messages))))))

(ert-deftest clipboard-register-yank-lines-no-region ()
  "Register yank-lines inserts at beginning of current line.

Verifies: with no active region, content is inserted at (pos-bol)."
  (let ((text-initial "foo\nbar\nbaz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Pre-load register ?a with "INSERTED\n".
      (set-register ?a "INSERTED\n")
      (setq meep--clipboard-register-current ?a)
      ;; Move point to middle of "bar" line (col 1).
      (goto-char 6)
      (should (equal ?a (char-after)))
      ;; Yank as lines: should insert at start of "bar" line.
      (simulate-input-for-meep
        '(:state normal :command meep-clipboard-register-yank-lines))
      (should (equal "foo\nINSERTED\nbar\nbaz" (buffer-string))))))

(ert-deftest clipboard-register-yank-lines-with-region ()
  "Register yank-lines with active region replaces the region.

Verifies: active region is deleted and register content inserted in its place."
  (let ((text-initial "foo bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Pre-load register ?a with "INSERTED".
      (set-register ?a "INSERTED")
      (setq meep--clipboard-register-current ?a)
      ;; Select "bar" via motion then activate visual region.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      (should (equal 'visual (bray-state)))
      ;; Yank as lines: active region should be replaced.
      (simulate-input-for-meep
        '(:state visual :command meep-clipboard-register-yank-lines))
      (should (equal "foo INSERTED baz" (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Delete Char Ring
;;
;; A mini `kill-ring' for single-character deletions.  Useful for correcting
;; typos: delete wrong chars, then yank them back in the correct position.
;; Consecutive deletes accumulate; any other command clears the ring.
;;
;; Test pattern: delete content from [brackets], yank into (parens).

(ert-deftest delete-char-ring-forward-single ()
  "Delete character at point, yank it elsewhere.

Workflow: delete 'x' forward from [x], move to (), yank.
Verifies: basic forward delete stores in ring, yank restores."
  ;;   [x] ()  ->  [] (x)
  (let ((text-initial "[x] ()")
        (text-expected "[] (x)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: [x] ()
      ;;          ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-next))
      ;; Cursor: [] ()
      ;;          ^
      ;; Move past ']', ' ', '(' to inside parens.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-yank))
      (should (equal text-expected (buffer-string))))))

(ert-deftest delete-char-ring-backward-single ()
  "Delete character before point, yank it elsewhere.

Workflow: position after 'x' in [x], delete backward, yank into ().
Verifies: backward delete stores in ring same as forward."
  ;;   [x] ()  ->  [] (x)
  (let ((text-initial "[x] ()")
        (text-expected "[] (x)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: [x] ()
      ;;            ^  (on ']')
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-prev))
      ;; Cursor: [] ()
      ;;          ^
      ;; Move past ']', ' ', '(' to inside parens.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-yank))
      (should (equal text-expected (buffer-string))))))

(ert-deftest delete-char-ring-consecutive-forward ()
  "Consecutive forward deletes accumulate; yank restores original order.

Workflow: delete 'a', 'b', 'c' forward one at a time, yank all three.
Verifies: ring preserves order, result is 'abc' not 'cba'."
  ;;   [abc] ()  ->  [] (abc)
  (let ((text-initial "[abc] ()")
        (text-expected "[] (abc)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: [abc] ()
      ;;          ^
      ;; Delete 'a', 'b', 'c' in sequence.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-next)
        '(:state normal :command meep-delete-char-ring-next)
        '(:state normal :command meep-delete-char-ring-next))
      ;; Cursor: [] ()
      ;;          ^
      ;; Move past ']', ' ', '(' to inside parens, yank all three.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        [?\C-u ?3]
        '(:state normal :command meep-delete-char-ring-yank))
      (should (equal text-expected (buffer-string))))))

(ert-deftest delete-char-ring-consecutive-backward ()
  "Consecutive backward deletes accumulate; yank restores original order.

Workflow: delete 'c', 'b', 'a' backward one at a time, yank all three.
Verifies: ring preserves text order regardless of delete direction."
  ;;   [abc] ()  ->  [] (abc)
  (let ((text-initial "[abc] ()")
        (text-expected "[] (abc)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: [abc] ()
      ;;             ^  (on ']')
      ;; Delete 'c', 'b', 'a' backward.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        [?\C-u ?3]
        '(:state normal :command meep-delete-char-ring-prev))
      ;; Cursor: [] ()
      ;;          ^
      ;; Move past ']', ' ', '(' to inside parens, yank all three.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        [?\C-u ?3]
        '(:state normal :command meep-delete-char-ring-yank))
      (should (equal text-expected (buffer-string))))))

(ert-deftest delete-char-ring-clears-on-other-command ()
  "Any non-delete command clears the ring.

Workflow: delete 'a', do motion, delete 'b', yank twice.
Verifies: first delete lost after motion, only 'b' in ring,
second yank does nothing (ring exhausted)."
  ;;   [a] [b] ()  ->  [] [] (b)
  (let ((text-initial "[a] [b] ()")
        (text-expected "[] [] (b)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: [a] [b] ()
      ;;          ^
      ;; Delete 'a'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-next))
      ;; Cursor: [] [b] ()
      ;;          ^
      ;; Motion clears ring.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: [] [b] ()
      ;;             ^
      ;; Delete 'b'.  Ring now contains only 'b'.
      (simulate-input-for-meep
        '(:state normal :command meep-delete-char-ring-next))
      ;; Cursor: [] [] ()
      ;;             ^
      ;; Move past ']', ' ', '(' to inside parens.
      ;; First yank restores 'b', second yank does nothing (ring empty).
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-yank)
        '(:state normal :command meep-delete-char-ring-yank))
      (should (equal text-expected (buffer-string))))))

(ert-deftest delete-char-ring-yank-empty-noop ()
  "Yank from empty ring does nothing.

Workflow: yank immediately with no prior deletes.
Verifies: buffer unchanged, no error signaled, message displayed."
  (let ((text-initial "unchanged"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-delete-char-ring-yank))
      (should (equal text-initial (buffer-string)))
      ;; Verify informative message was displayed.
      (should (equal '("Delete char-ring empty") (meep-test-messages))))))

;; ---------------------------------------------------------------------------
;; Region Swap
;;
;; Swaps content between primary region and secondary selection.  Supports:
;; - Contiguous regions (regular text selection)
;; - Line-wise regions (whole lines)
;; - Rectangle regions (2D blocks)
;; - Implied regions (infer shape from secondary selection at cursor)
;;
;; Test pattern: bracketed markers (e.g. [ABC], [WXYZ]) identify swapped content.
;; Secondary selection created first, then swap with primary region.

(ert-deftest region-swap-contiguous-explicit ()
  "Swap two explicitly selected contiguous regions.

Workflow: select [ABC], make secondary, select [XYZ], swap.
Verifies: contents exchange positions, returns to normal state."
  ;;   .[ABC]__[XYZ].  ->  .[XYZ]__[ABC].
  (let ((text-initial ".[ABC]__[XYZ].")
        (text-expected ".[XYZ]__[ABC].")
        (point-expected '(1 . 13))
        (mark-expected '(1 . 8)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [ABC] (positions 1-6), make secondary selection.
      ;; Cursor: .[ABC]__[XYZ].
      ;;          ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[ABC]__[XYZ].
      ;;               ^  (after [ABC])
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Now select [XYZ] (positions 8-13).
      ;; Cursor: .[ABC]__[XYZ].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[ABC]__[XYZ].
      ;;                      ^  (after [XYZ])
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-contiguous-implied ()
  "Swap with implied region inferred from secondary selection.

Workflow: select [ABC], make secondary, move cursor to [XYZ], swap.
Without explicit selection, region is implied from secondary's shape.
Verifies: implied region matches secondary's column width."
  ;;   .[ABC]__[XYZ].  ->  .[XYZ]__[ABC].
  (let ((text-initial ".[ABC]__[XYZ].")
        (text-expected ".[XYZ]__[ABC].")
        (point-expected '(1 . 8))
        (mark-expected '(1 . 13)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [ABC], make secondary selection.
      ;; Cursor: .[ABC]__[XYZ].
      ;;          ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[ABC]__[XYZ].
      ;;               ^
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Move to start of [XYZ] without selecting.
      ;; Cursor: .[ABC]__[XYZ].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Swap with implied region (5 chars from cursor).
      (simulate-input-for-meep
        '(:state normal :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-line-explicit ()
  "Swap two explicitly selected whole lines.

Workflow: select line 2, make secondary, select line 4, swap.
Verifies: entire lines exchange positions."
  ;;   [ABC] line / [XYZ] line  ->  [XYZ] line / [ABC] line
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "header\n"
          "[ABC]\n"
          "middle\n"
          "[XYZ]\n"
          "footer\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "header\n"
          "[XYZ]\n"
          "middle\n"
          "[ABC]\n"
          "footer\n"))
        (point-expected '(5 . 0))
        (mark-expected '(4 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to line 2, select whole line [ABC].
      ;; Cursor line 2: [ABC]
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-expand-to-line-bounds))
      ;; Cursor line 3: middle
      ;;                ^
      (simulate-input-for-meep
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Move to line 4, select whole line [XYZ].
      ;; Cursor line 4: [XYZ]
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-expand-to-line-bounds))
      ;; Cursor line 5: footer
      ;;                ^
      (simulate-input-for-meep
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-line-implied ()
  "Swap line with implied region inferred from secondary selection.

Workflow: select line [ABC], make secondary, move to line [XYZ], swap.
Without explicit selection, implied region matches secondary's line count.
Verifies: line-wise shape preserved for implied region."
  ;;   [ABC] line / [XYZ] line  ->  [XYZ] line / [ABC] line
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "header\n"
          "[ABC]\n"
          "middle\n"
          "[XYZ]\n"
          "footer\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "header\n"
          "[XYZ]\n"
          "middle\n"
          "[ABC]\n"
          "footer\n"))
        (point-expected '(4 . 0))
        (mark-expected '(4 . 5)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to line 2, select whole line [ABC], make secondary.
      ;; Cursor line 2: [ABC]
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-expand-to-line-bounds))
      ;; Cursor line 3: middle
      ;;                ^
      (simulate-input-for-meep
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Move to line 4, swap with implied line region.
      ;; Cursor line 4: [XYZ]
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-line-multiple ()
  "Swap two multi-line regions.

Workflow: select lines 2-3, make secondary, select lines 5-6, swap.
Verifies: multi-line blocks exchange positions."
  ;;   [AB-1]/[CD-2] / [WX-1]/[YZ-2]  ->  [WX-1]/[YZ-2] / [AB-1]/[CD-2]
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "header\n"
          "[AB-1]\n"
          "[CD-2]\n"
          "middle\n"
          "[WX-1]\n"
          "[YZ-2]\n"
          "footer\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "header\n"
          "[WX-1]\n"
          "[YZ-2]\n"
          "middle\n"
          "[AB-1]\n"
          "[CD-2]\n"
          "footer\n"))
        (point-expected '(7 . 0))
        (mark-expected '(5 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to line 2, select lines 2-3.
      ;; Cursor line 2: [AB-1]
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-expand-to-line-bounds))
      ;; Cursor line 4: middle
      ;;                ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Move to line 5, select lines 5-6.
      ;; Cursor line 5: [WX-1]
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-expand-to-line-bounds))
      ;; Cursor line 7: footer
      ;;                ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-error-no-secondary ()
  "Swap without secondary selection signals error.

Workflow: select region, attempt swap without secondary.
Verifies: error signaled, buffer unchanged."
  (let ((text-initial ".[ABC]__[XYZ]."))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [ABC] but don't make secondary.
      ;; Cursor: .[ABC]__[XYZ].
      ;;          ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[ABC]__[XYZ].
      ;;               ^
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state visual :command meep-move-char-next))
      ;; Attempt swap without secondary selection.
      (should-error-with-message
          (simulate-input-for-meep
            '(:state visual :command meep-region-swap))
        'user-error
        "No secondary selection!")
      (should (equal text-initial (buffer-string))))))

(ert-deftest region-swap-error-contiguous-overlapping ()
  "Swap with overlapping contiguous regions signals error.

Workflow: create secondary, select overlapping region, attempt swap.
Verifies: error signaled for overlapping regions, buffer unchanged."
  (let ((text-initial "[ABCXYZ]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [ABC (positions 0-4), make secondary.
      ;; Cursor: [ABCXYZ]
      ;;         ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      ;; Cursor: [ABCXYZ]
      ;;             ^
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select CXYZ] (positions 3-8), overlapping with secondary.
      ;; Cursor: [ABCXYZ]
      ;;            ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command meep-region-toggle))
      ;; Cursor: [ABCXYZ]
      ;;                ^
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state visual :command meep-move-char-next))
      ;; Attempt swap with overlapping regions.
      (should-error-with-message
          (simulate-input-for-meep
            '(:state visual :command meep-region-swap))
        'user-error
        "Region swap unsupported for overlapping regions")
      (should (equal text-initial (buffer-string))))))

(ert-deftest region-swap-error-insufficient-lines ()
  "Swap with implied region fails when not enough lines after cursor.

Workflow: select 3-line region, make secondary, move to last line, attempt swap.
Verifies: error signaled when implied region would extend past buffer end."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "[AB-1]\n"
          "[CD-2]\n"
          "[EF-3]\n"
          "[XY-1]\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select 3 lines, make secondary.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-region-toggle))
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Move to line 4 - only 1 line available, but implied region needs 3.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-beginning))
      ;; Attempt swap with implied region.
      (should-error-with-message
          (simulate-input-for-meep
            '(:state normal :command meep-region-swap))
        'user-error
        "Region swap failed, expected 2 line(s) after the point")
      (should (equal text-initial (buffer-string))))))

(ert-deftest region-swap-error-rectangle-line-mismatch ()
  "Swap rectangles with different line counts signals error.

Workflow: select 2-line rectangle, make secondary, select 3-line rectangle, swap.
Verifies: error signaled for line count mismatch."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".[AB]....[WX].\n"
          ".[CD]....[YZ].\n"
          ".........[UV].\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [AB]/[CD] rectangle (2 lines), make secondary.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [WX]/[YZ]/[UV] rectangle (3 lines).
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state visual :command meep-move-char-next)
        [?\C-u ?2]
        '(:state visual :command meep-move-line-next))
      ;; Attempt swap with mismatched line counts.
      (should-error-with-message
          (simulate-input-for-meep
            '(:state visual :command meep-region-swap))
        'user-error
        "Rectangle line count mismatch (3 and 2)")
      (should (equal text-initial (buffer-string))))))

(ert-deftest region-swap-error-rectangle-overlapping ()
  "Swap with overlapping rectangles signals error.

Workflow: create overlapping rectangle selections, attempt swap.
Verifies: error signaled for overlapping rectangle regions."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".[ABCD].\n"
          ".[EFGH].\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [ABC/[EFG rectangle (columns 1-4), make secondary.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select BCD]/FGH] rectangle (columns 2-5), overlapping at columns 2-4.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command meep-region-toggle-rectangle))
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next))
      ;; Attempt swap with overlapping rectangles.
      (should-error-with-message
          (simulate-input-for-meep
            '(:state visual :command meep-region-swap))
        'user-error
        "Region swap unsupported for overlapping (rectangle) regions")
      (should (equal text-initial (buffer-string))))))

(ert-deftest region-swap-contiguous-explicit-short-to-long ()
  "Swap short region (secondary) with long region (primary).

Workflow: select [A], make secondary, select [VWXYZ], swap.
Verifies: short-to-long swap works correctly."
  ;;   .[A]__[VWXYZ].  ->  .[VWXYZ]__[A].
  (let ((text-initial ".[A]__[VWXYZ].")
        (text-expected ".[VWXYZ]__[A].")
        (point-expected '(1 . 13))
        (mark-expected '(1 . 10)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [A] (3 chars), make secondary.
      ;; Cursor: .[A]__[VWXYZ].
      ;;          ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[A]__[VWXYZ].
      ;;            ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "2"
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [VWXYZ] (7 chars).
      ;; Cursor: .[A]__[VWXYZ].
      ;;               ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        "1"
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[A]__[VWXYZ].
      ;;                      ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "6"
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-contiguous-explicit-long-to-short ()
  "Swap long region (secondary) with short region (primary).

Workflow: select [VWXYZ], make secondary, select [A], swap.
Verifies: long-to-short swap works correctly."
  ;;   .[A]__[VWXYZ].  ->  .[VWXYZ]__[A].
  (let ((text-initial ".[A]__[VWXYZ].")
        (text-expected ".[VWXYZ]__[A].")
        (point-expected '(1 . 8))
        (mark-expected '(1 . 1)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [VWXYZ] (7 chars), make secondary.
      ;; Cursor: .[A]__[VWXYZ].
      ;;               ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        "5"
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[A]__[VWXYZ].
      ;;                      ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "6"
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [A] (3 chars).
      ;; Cursor: .[A]__[VWXYZ].
      ;;          ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle))
      ;; Cursor: .[A]__[VWXYZ].
      ;;            ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "2"
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-rectangle-explicit ()
  "Swap two explicitly selected rectangles.

Workflow: select [AB] rectangle, make secondary, select [YZ] rectangle, swap.
Verifies: rectangular blocks exchange positions."
  ;;   .[AB].   .[YZ].
  ;;   .[AB].   .[YZ].
  ;;   ......   ......
  ;;   .[YZ].   .[AB].
  ;;   .[YZ].   .[AB].
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".[AB].\n"
          ".[AB].\n"
          "......\n"
          ".[YZ].\n"
          ".[YZ].\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          ".[YZ].\n"
          ".[YZ].\n"
          "......\n"
          ".[AB].\n"
          ".[AB].\n"))
        (point-expected '(5 . 5))
        (mark-expected '(4 . 1)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [AB] rectangle: lines 1-2, columns 1-4.
      ;; Cursor line 1: .[AB].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 2: .[AB].
      ;;                     ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [YZ] rectangle: lines 4-5, columns 1-4.
      ;; Cursor line 4: .[YZ].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        "1"
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 5: .[YZ].
      ;;                     ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-rectangle-implied ()
  "Swap rectangle with implied region at cursor position.

Workflow: select [AB] rectangle, make secondary, move to [YZ], swap.
Implied region matches secondary's dimensions.
Verifies: rectangle shape inferred from secondary selection."
  ;;   .[AB].   .[YZ].
  ;;   .[AB].   .[YZ].
  ;;   ......   ......
  ;;   .[YZ].   .[AB].
  ;;   .[YZ].   .[AB].
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".[AB].\n"
          ".[AB].\n"
          "......\n"
          ".[YZ].\n"
          ".[YZ].\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          ".[YZ].\n"
          ".[YZ].\n"
          "......\n"
          ".[AB].\n"
          ".[AB].\n"))
        (point-expected '(4 . 1))
        (mark-expected '(5 . 5)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [AB] rectangle, make secondary.
      ;; Cursor line 1: .[AB].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 2: .[AB].
      ;;                     ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Move to top-left of [YZ] without selecting.
      ;; Cursor line 4: .[YZ].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        "1"
        '(:state normal :command meep-move-line-beginning)
        '(:state normal :command meep-move-char-next))
      ;; Swap with implied rectangle.
      (simulate-input-for-meep
        '(:state normal :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-rectangle-side-by-side ()
  "Swap two rectangles on the same lines but different columns.

Workflow: select [AB] rectangle, make secondary, select [YZ] rectangle, swap.
Rectangles occupy same rows but are horizontally adjacent (not overlapping).
Verifies: side-by-side rectangles swap correctly."
  ;;   .[AB].[YZ].      .[YZ].[AB].
  ;;   .[AB].[YZ].  ->  .[YZ].[AB].
  ;;   .[AB].[YZ].      .[YZ].[AB].
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".[AB].[YZ].\n"
          ".[AB].[YZ].\n"
          ".[AB].[YZ].\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          ".[YZ].[AB].\n"
          ".[YZ].[AB].\n"
          ".[YZ].[AB].\n"))
        (point-expected '(3 . 10))
        (mark-expected '(1 . 6)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [AB] rectangle: 3 lines, columns 1-4.
      ;; Cursor line 1: .[AB].[YZ].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 3: .[AB].[YZ].
      ;;                     ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        "1"
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [YZ] rectangle: 3 lines, columns 6-9.
      ;; Cursor line 1: .[AB].[YZ].
      ;;                      ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        "1"
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 3: .[AB].[YZ].
      ;;                          ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        "1"
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-rectangle-side-by-side-a ()
  "Swap two rectangles where second extends one line above first.

Workflow: select [AB] on lines 2-4, make secondary, select [YZ] on lines 1-3, swap.
Rectangles are horizontally adjacent with partial vertical overlap.
Verifies: second rectangle above first handled correctly."
  ;;   ......[YZ].      ......[AB].
  ;;   .[AB].[YZ].      .[YZ].[AB].
  ;;   .[AB].[YZ].  ->  .[YZ].[AB].
  ;;   .[AB]......      .[YZ]......
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "......[YZ].\n"
          ".[AB].[YZ].\n"
          ".[AB].[YZ].\n"
          ".[AB]......\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "......[AB].\n"
          ".[YZ].[AB].\n"
          ".[YZ].[AB].\n"
          ".[YZ]......\n"))
        (point-expected '(3 . 10))
        (mark-expected '(1 . 6)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [AB] rectangle: lines 2-4, columns 1-4.
      ;; Cursor line 2: .[AB].[YZ].
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 4: .[AB]......
      ;;                     ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        "1"
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [YZ] rectangle: lines 1-3, columns 6-9.
      ;; Cursor line 1: ......[YZ].
      ;;                      ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        "2"
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 3: .[AB].[YZ].
      ;;                          ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        "1"
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest region-swap-rectangle-side-by-side-b ()
  "Swap two rectangles where second extends one line below first.

Workflow: select [AB] on lines 1-3, make secondary, select [YZ] on lines 2-4, swap.
Rectangles are horizontally adjacent with partial vertical overlap.
Verifies: second rectangle below first handled correctly."
  ;;   .[AB]......      .[YZ]......
  ;;   .[AB].[YZ].      .[YZ].[AB].
  ;;   .[AB].[YZ].  ->  .[YZ].[AB].
  ;;   ......[YZ].      ......[AB].
  (let ((text-initial
         ;; format-next-line: off
         (concat
          ".[AB]......\n"
          ".[AB].[YZ].\n"
          ".[AB].[YZ].\n"
          "......[YZ].\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          ".[YZ]......\n"
          ".[YZ].[AB].\n"
          ".[YZ].[AB].\n"
          "......[AB].\n"))
        (point-expected '(4 . 10))
        (mark-expected '(2 . 6)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select [AB] rectangle: lines 1-3, columns 1-4.
      ;; Cursor line 1: .[AB]......
      ;;                 ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 3: .[AB].[YZ].
      ;;                     ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        "1"
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [YZ] rectangle: lines 2-4, columns 6-9.
      ;; Cursor line 2: .[AB].[YZ].
      ;;                      ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle-rectangle))
      ;; Cursor line 4: ......[YZ].
      ;;                          ^
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        "1"
        '(:state visual :command meep-region-swap))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column)))
      (should (equal mark-expected (meep-test-mark-line-column))))))


;; ---------------------------------------------------------------------------
;; Transpose Tests (Basic)

(ert-deftest transpose-char-next ()
  "Transpose characters after forward char motion.

Move forward one char, then transpose.
Verifies: characters before and after cursor swap."
  ;;   ABC  ->  BAC
  (let ((text-initial "ABC")
        (text-expected "BAC")
        (point-expected '(1 . 1)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: ABC
      ;;         ^
      ;; Move forward one char and transpose in same block (preserves `last-command').
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-transpose))
      ;; Result: BAC
      ;;          ^
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-char-prev ()
  "Transpose characters after backward char motion.

Move backward one char, then transpose.
Verifies: characters before and after cursor swap."
  ;;   ABC  ->  ACB  (cursor starts at C, moves to B, swaps B and C)
  (let ((text-initial "ABC")
        (text-expected "ACB")
        (point-expected '(1 . 1)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: ABC
      ;;         ^
      ;; Move forward twice to position on 'C', then backward and transpose.
      ;; All in single block to preserve last-command chain.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command meep-transpose))
      ;; Result: ACB (B and C swapped)
      ;;          ^
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-char-next-repeat ()
  "Transpose characters twice after forward char motion.

Move forward one char, then transpose twice.
Verifies: character moves right twice through repeated transpose."
  ;;   ABCD  ->  BACD  ->  BCAD
  (let ((text-initial "ABCD")
        (text-expected "BCAD")
        (point-expected '(1 . 2)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: ABCD
      ;;         ^
      ;; Move forward one char and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: BCAD (A moved right twice)
      ;;            ^
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-char-prev-repeat ()
  "Transpose characters twice after backward char motion.

Move backward one char, then transpose twice.
Verifies: character moves left twice through repeated transpose."
  ;;   ABCD  ->  ABDC  ->  ADBC
  (let ((text-initial "ABCD")
        (text-expected "ADBC")
        (point-expected '(1 . 1)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Position on D (last character).
      (goto-char (1- (point-max)))
      ;; Cursor: ABCD
      ;;            ^
      ;; Move backward one char and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: ADBC (D moved left twice)
      ;;          ^
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-char-next-at-buffer-end ()
  "Transpose stops when reaching buffer end.

Move forward one char, then transpose four times.
First three succeed, fourth does nothing and prints a message."
  ;;   ABCD
  ;;   -> BACD  (first transpose works)
  ;;   -> BCAD  (second transpose works)
  ;;   -> BCDA  (third transpose works)
  ;;   -> BCDA  (fourth transpose does nothing - at end)
  (let ((text-initial "ABCD")
        (text-expected "BCDA"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move forward one char and transpose four times.
      ;; First three succeed, fourth fails at buffer limit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        [?\C-u ?3]
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first three transposes worked,
      ;; fourth did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-char-prev-at-buffer-start ()
  "Transpose stops when reaching buffer start.

Position on last char, move backward one char, then transpose four times.
First three succeed, fourth does nothing and prints a message."
  ;;   ABCD
  ;;   -> ABDC  (first transpose works)
  ;;   -> ADBC  (second transpose works)
  ;;   -> DABC  (third transpose works)
  ;;   -> DABC  (fourth transpose does nothing - at start)
  (let ((text-initial "ABCD")
        (text-expected "DABC"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Position on last character (D).
      (goto-char (1- (point-max)))
      ;; Move backward one char and transpose four times.
      ;; First three succeed, fourth fails at buffer limit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-prev)
        [?\C-u ?3]
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first three transposes worked,
      ;; fourth did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-word-next-repeat ()
  "Transpose words twice after forward word motion.

Move to second word, then transpose twice.
Verifies: word moves right twice through repeated transpose."
  ;;   one two three four five
  ;;   -> two one three four five
  ;;   -> two three one four five
  (let ((text-initial "one two three four five")
        (text-expected "two three one four five")
        ;; Cursor ends at start of next word after the transposed region.
        (point-expected '(1 . 14)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: one two three four five
      ;;         ^
      ;; Move to second word and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: two three one four five
      ;;                   ^
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-word-next-prefix-arg ()
  "Transpose words with prefix argument.

Move to second word, then transpose with prefix arg 2.
Verifies: word moves right twice with single command."
  (let ((text-initial "one two three four five")
        (text-expected "two three one four five")
        (point-expected '(1 . 14)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to second word, then transpose with C-u 2 prefix.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next)
        [?\C-u ?2]
        '(:state normal :command meep-transpose))
      ;; Result: two three one four five (same as calling transpose twice).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-word-next-at-buffer-end ()
  "Transpose stops when reaching buffer end.

Move to second word, then transpose three times.
First two succeed, third does nothing and prints a message."
  ;;   one two three four
  ;;   -> two one three four  (first transpose works)
  ;;   -> two three one four  (second transpose works)
  ;;   -> two three one four  (third transpose does nothing - motion can't advance)
  (let ((text-initial "one two three four")
        (text-expected "two three one four"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to second word and transpose three times.
      ;; First two succeed, third fails at buffer limit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first two transposes worked,
      ;; third did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-word-prev-at-buffer-start ()
  "Transpose stops when reaching buffer start.

Move to third word, then move backward and transpose three times.
First two succeed, third does nothing and prints a message."
  ;;   one two three four
  ;;   -> one three two four  (first transpose works)
  ;;   -> three one two four  (second transpose works)
  ;;   -> three one two four  (third transpose does nothing - at start)
  (let ((text-initial "one two three four")
        (text-expected "three one two four"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Position at end of buffer.
      (goto-char (point-max))
      ;; Move backward to third word, then transpose three times.
      ;; First two succeed, third fails at buffer limit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-prev)
        '(:state normal :command meep-move-word-prev)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first two transposes worked,
      ;; third did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-word-prev-repeat ()
  "Transpose words twice after backward word motion.

Move to last word, then move backward and transpose twice.
Verifies: word moves left twice through repeated transpose."
  ;;   one two three four five
  ;;   -> one two three five four
  ;;   -> one two five three four
  (let ((text-initial "one two three four five")
        (text-expected "one two five three four"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Position at end of buffer.
      (goto-char (point-max))
      ;; Cursor: one two three four five
      ;;                                ^
      ;; Move backward one word and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-prev)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: one two five three four (five moved left twice)
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-word-next-end-repeat ()
  "Transpose words twice after forward word-end motion.

Move to end of current word, then transpose twice.
Verifies: word moves right twice through repeated transpose.
Note: This tests a different code path than `transpose-word-next-repeat'
where point jumps to word-end rather than word-start."
  ;;   one two three four five
  ;;   -> two one three four five
  ;;   -> two three one four five
  (let ((text-initial "one two three four five")
        (text-expected "two three one four five"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor at start of first word.
      ;; Cursor: one two three four five
      ;;         ^
      ;; Move to end of current word and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next-end)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: two three one four five (one moved right twice)
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-word-prev-end-repeat ()
  "Transpose words twice after backward word-end motion.

Move to end of previous word, then transpose twice.
Verifies: word moves left twice through repeated transpose.
Note: This tests a different code path than `transpose-word-prev-repeat'
where point jumps to word-end rather than word-start."
  ;;   one two three four five
  ;;   -> one two three five four
  ;;   -> one two five three four
  (let ((text-initial "one two three four five")
        (text-expected "one two five three four"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Position at end of buffer.
      (goto-char (point-max))
      ;; Cursor: one two three four five
      ;;                                ^
      ;; Move to end of previous word and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-prev-end)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: one two five three four (five moved left twice)
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-paragraph-next-repeat ()
  "Transpose paragraphs twice after forward paragraph motion.

Move to second paragraph, then transpose twice.
Verifies: paragraph moves down twice through repeated transpose."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "\n"
          "BBB\n"
          "\n"
          "CCC\n"))
        (text-expected
         ;; Note: paragraph transpose includes separators, resulting in
         ;; slightly different whitespace arrangement.
         ;; format-next-line: off
         (concat
          "\n"
          "BBB\n"
          "\n"
          "CCC\n"
          "AAA\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor at start of first paragraph (AAA).
      ;; Move to second paragraph and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-next)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: AAA moved down twice.
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-paragraph-prev-repeat ()
  "Transpose paragraphs twice after backward paragraph motion.

Move to last paragraph, then move backward and transpose twice.
Verifies: paragraph moves up twice through repeated transpose."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "\n"
          "BBB\n"
          "\n"
          "CCC\n"))
        ;; Note: paragraph transpose includes separators, resulting in
        ;; slightly different whitespace arrangement.
        (text-expected
         ;; format-next-line: off
         (concat
          "\n"
          "CCC\n"
          "AAA\n"
          "\n"
          "BBB\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Position at end of buffer (after CCC paragraph).
      (goto-char (point-max))
      ;; Move backward one paragraph and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-prev)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: CCC moved up twice.
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-paragraph-next-at-buffer-end ()
  "Transpose stops when reaching buffer end.

Move to second paragraph, then transpose four times.
First three succeed, fourth does nothing and prints a message."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "\n"
          "BBB\n"
          "\n"
          "CCC\n"
          "\n"
          "DDD\n"))
        (text-expected
         ;; Note: paragraph transpose includes separators.
         ;; format-next-line: off
         (concat
          "\n"
          "BBB\n"
          "\n"
          "CCC\n"
          "\n"
          "DDD\n"
          "AAA\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to second paragraph and transpose four times.
      ;; First three succeed, fourth fails at buffer limit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-next)
        [?\C-u ?3]
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first three transposes worked,
      ;; fourth did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-paragraph-prev-at-buffer-start ()
  "Transpose stops when reaching buffer start.

Position at end, move backward one paragraph, then transpose four times.
First three succeed, fourth does nothing and prints a message."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "\n"
          "BBB\n"
          "\n"
          "CCC\n"
          "\n"
          "DDD\n"))
        (text-expected
         ;; Note: paragraph transpose includes separators.
         ;; format-next-line: off
         (concat
          "\n"
          "DDD\n"
          "AAA\n"
          "\n"
          "BBB\n"
          "\n"
          "CCC\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Position at end of buffer.
      (goto-char (point-max))
      ;; Move backward one paragraph and transpose four times.
      ;; First three succeed, fourth fails at buffer limit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-prev)
        [?\C-u ?3]
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first three transposes worked,
      ;; fourth did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))

;; ---------------------------------------------------------------------------
;; Transpose: List item
;;
;; Transposing list items is a key use of the `list-item' text object.  It needs
;; no special support: the `next'/`prev' motions set the mark on motion like
;; any other motion, and `meep-transpose' acts on that.

(ert-deftest transpose-list-item-next ()
  "Transpose a list item with the following one.

Verifies: forward list item motion + transpose swaps adjacent list items,
separators included, leaving a well-formed list."
  (let ((text-initial "foo(aa, bb, cc)")
        (text-expected "foo(bb, aa, cc)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor on the first list item `aa'.
      ;;     foo(aa, bb, cc)
      ;;         ^ pos 5
      (goto-char 5)
      (should (equal ?a (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next)
        '(:state normal :command meep-transpose))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-list-item-prev ()
  "Transpose a list item with the preceding one.

Verifies: backward list item motion + transpose swaps the landed list item
with its predecessor (the symmetric counterpart of the forward case)."
  (let ((text-initial "foo(aa, bb, cc, dd)")
        (text-expected "foo(aa, cc, bb, dd)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor on the last list item `dd'; `prev' lands on `cc'.
      ;;     foo(aa, bb, cc, dd)
      ;;                     ^ pos 17
      (goto-char 17)
      (should (equal ?d (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-prev)
        '(:state normal :command meep-transpose))
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-list-item-next-repeat ()
  "Drag a list item across the list by repeating transpose.

Verifies: a prefix argument repeats the transpose, moving one list item
past several others in a single command."
  (let ((text-initial "foo(aa, bb, cc, dd)")
        (text-expected "foo(bb, cc, aa, dd)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor on `aa'; move next then transpose twice (C-u 2).
      (goto-char 5)
      (should (equal ?a (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next)
        [?\C-u ?2]
        '(:state normal :command meep-transpose))
      ;; `aa' has moved two positions to the right.
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-line-next ()
  "Transpose lines after forward line motion.

Move down one line, then transpose.
Verifies: current line and previous line swap."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "BBB\n"
          "AAA\n"
          "CCC\n"))
        (point-expected '(2 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: AAA
      ;;                ^
      ;; Move down one line and transpose in same block.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-transpose))
      ;; Result: BBB swapped with AAA.
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-next-mark-position ()
  "Transpose lines after forward line motion places mark at end of previous line.

Move down one line, then transpose.
Verifies: mark is at the end of the previous line (before newline)."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"))
        (mark-expected '(1 . 3))) ;; Line 1, column 3 (end of "BBB")
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: AAA
      ;;                ^
      ;; Move down one line and transpose.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-transpose))
      ;; After transpose: BBB is on line 1, AAA is on line 2.
      ;; Mark should be at end of line 1 (end of "BBB").
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest transpose-line-prev-mark-position ()
  "Transpose lines after backward line motion places mark at beginning of next line.

Move to end of line, then up one line, then transpose.
Verifies: mark is at the beginning of the next line."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"))
        (mark-expected '(2 . 0))) ;; Line 2, column 0 (beginning of "AAA")
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to line 2, then to end of line, then up one line and transpose.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-end)
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-transpose))
      ;; After transpose: BBB is on line 1, AAA is on line 2.
      ;; Mark should be at beginning of line 2.
      (should (equal mark-expected (meep-test-mark-line-column))))))

(ert-deftest transpose-line-prev ()
  "Transpose lines after backward line motion.

Move up one line, then transpose.
Verifies: current line and next line swap."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "BBB\n"
          "AAA\n"
          "CCC\n"))
        (point-expected '(1 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: AAA
      ;;                ^
      ;; Move down to line 2, then up and transpose.
      ;; All in single block to preserve last-command chain.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-transpose))
      ;; Result: AAA swapped with BBB.
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-no-prior-motion ()
  "Transpose with no prior motion.

Verifies: buffer unchanged, informative message displayed."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      ;; Attempt transpose with no prior motion.
      (simulate-input-for-meep
        '(:state normal :command meep-transpose))
      ;; Buffer unchanged.
      (should (equal 'normal (bray-state)))
      (should (equal text-initial (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Verify informative messages were displayed.
      ;; (one from the helper function, one from transpose itself)
      (let ((msgs (meep-test-messages)))
        (should (seq-some (lambda (m) (string-match-p "no mark found" m)) msgs))
        (should (member "Transpose could not find a last-motion" msgs))))))

(ert-deftest transpose-line-next-repeat ()
  "Transpose lines twice after forward line motion.

Move down one line, then transpose twice.
Verifies: line moves down twice through repeated transpose."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"
          "DDD\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "BBB\n"
          "CCC\n"
          "AAA\n"
          "DDD\n"))
        (point-expected '(3 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: AAA
      ;;                ^
      ;; Move down one line and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: AAA moved down twice (BBB up, then CCC up).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-next-repeat-different-lengths ()
  "Transpose lines of different lengths twice.

Move down one line, then transpose twice with lines of varying lengths.
Verifies: correct positioning after swapping lines of different sizes."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBBBBB\n"
          "CCC\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "BBBBBB\n"
          "CCC\n"
          "AAA\n"))
        (point-expected '(3 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: AAA (3 chars)
      ;;                ^
      ;; Move down to BBBBBB (6 chars), then transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: AAA moved down twice (BBBBBB up, then CCC up).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-next-no-trailing-newline ()
  "Transpose lines when buffer has no trailing newline.

Move down one line, then transpose twice in a buffer without trailing newline.
Verifies: line moves to end of buffer correctly without trailing newline."
  (let ((text-initial "A\nBB\nCCC")
        (text-expected "BB\nCCC\nA")
        (point-expected '(3 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: A
      ;;                ^
      ;; Move down one line and transpose twice.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: A moved down twice to become last line (no trailing newline).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-next-after-line-end ()
  "Transpose lines after moving to line end first.

Move to end of line, then down one line, then transpose.
Verifies: lines swap correctly and cursor column matches original mark column."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "A\n"
          "BB\n"
          "CCC\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "BB\n"
          "A\n"
          "CCC\n"))
        ;; Cursor should be at end of line (column 1, matching original mark column).
        (point-expected '(2 . 1)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor at line 1 beginning: A
      ;;                             ^
      ;; Move to end of line (column 1), then down, then transpose.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-end)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-transpose))
      ;; Result: lines 1 and 2 swapped, cursor at column 1 (end of "A").
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-next-prefix-arg ()
  "Transpose lines with prefix argument.

Move down one line, then transpose with prefix arg 2.
Verifies: line moves down twice with single command."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"
          "DDD\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "BBB\n"
          "CCC\n"
          "AAA\n"
          "DDD\n"))
        (point-expected '(3 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: AAA
      ;;                ^
      ;; Move down one line, then transpose with C-u 2 prefix.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        [?\C-u ?2]
        '(:state normal :command meep-transpose))
      ;; Result: AAA moved down twice (same as calling transpose twice).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-prev-prefix-arg ()
  "Transpose lines with prefix argument after backward motion.

Move to last line, move up one line, then transpose with prefix arg 2.
Verifies: line moves up twice with single command (same as two transposes)."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"
          "DDD\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "AAA\n"
          "DDD\n"
          "BBB\n"
          "CCC\n"))
        (point-expected '(2 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to last line (DDD).
      (goto-char (point-max))
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        ;; Move up one line, then transpose with C-u 2 prefix.
        '(:state normal :command meep-move-line-prev)
        [?\C-u ?2]
        '(:state normal :command meep-transpose))
      ;; Result: DDD moved up twice (same as calling transpose twice).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-prev-repeat ()
  "Transpose lines twice after backward line motion.

Move to last line, then move up and transpose twice.
Verifies: line moves up twice through repeated transpose."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"
          "DDD\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "AAA\n"
          "DDD\n"
          "BBB\n"
          "CCC\n"))
        (point-expected '(2 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to last line (DDD).
      (goto-char (point-max))
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        ;; Move up one line and transpose twice.
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: DDD moved up twice (CCC down, then BBB down).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-prev-repeat-different-lengths ()
  "Transpose lines of different lengths twice (backward motion).

Move to last line, then move up and transpose twice with lines of varying lengths.
Verifies: correct positioning after swapping lines of different sizes."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBBBBB\n"
          "CCC\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "CCC\n"
          "AAA\n"
          "BBBBBB\n"))
        (point-expected '(1 . 0)))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to last line (CCC).
      (goto-char (point-max))
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        ;; Move up one line and transpose twice.
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Result: CCC moved up twice (BBBBBB down, then AAA down).
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

(ert-deftest transpose-line-next-at-buffer-end ()
  "Transpose stops when reaching buffer end.

Move down one line, then transpose four times.
First three succeed, fourth does nothing and prints a message."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"
          "DDD\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "BBB\n"
          "CCC\n"
          "DDD\n"
          "AAA\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move down one line and transpose four times.
      ;; First three succeed, fourth fails at buffer limit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        [?\C-u ?3]
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first three transposes worked,
      ;; fourth did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Point and mark must not move on failed transpose.
      ;; After three successful transposes, point is at start of last line (AAA),
      ;; mark is at end of previous line (DDD). Failed transpose must not change these.
      (should (equal 13 (point))) ; Start of "AAA\n"
      (should (equal 12 (mark))) ; End of "DDD\n"
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))

(ert-deftest transpose-line-prev-at-buffer-start ()
  "Transpose stops when reaching buffer start.

Move to last line, then move up and transpose four times.
First three succeed, fourth does nothing and prints a message."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AAA\n"
          "BBB\n"
          "CCC\n"
          "DDD\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "DDD\n"
          "AAA\n"
          "BBB\n"
          "CCC\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to last line (DDD).
      (goto-char (point-max))
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        ;; Move up one line and transpose four times.
        ;; First three succeed, fourth fails at buffer limit.
        '(:state normal :command meep-move-line-prev)
        [?\C-u ?3]
        '(:state normal :command meep-transpose)
        '(:state normal :command meep-transpose))
      ;; Buffer has expected state (confirms first three transposes worked,
      ;; fourth did nothing).
      (should (equal text-expected (buffer-string)))
      ;; Point and mark must not move on failed transpose.
      ;; After three successful transposes, point is at start of first line (DDD),
      ;; mark is at start of second line (AAA). Failed transpose must not change these.
      (should (equal 1 (point))) ; Start of "DDD\n"
      (should (equal 5 (mark))) ; Start of "AAA\n"
      ;; Exactly one message was printed by the failing transpose.
      (should (equal '("Transpose: no element to swap with") (meep-test-messages)))
      (should (equal 'normal (bray-state))))))


;; ---------------------------------------------------------------------------
;; Selection Tests

;; region-activate-or-reverse tests

(ert-deftest selection-activate-or-reverse-when-active ()
  "Reverse point and mark when region is already active.

Verifies: point and mark swap positions, region stays active."
  (let* ((text-initial "ABCDEF")
         (text-expected-region "ABC"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Start selection and move forward.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next))
      ;; Cursor: ABCDEF
      ;;            ^  (point at D, mark at A)
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal text-expected-region (meep-test-region-as-string)))
      ;; Call activate-or-reverse: since region is active, this reverses.
      (simulate-input-for-meep
        '(:state visual :command meep-region-activate-or-reverse))
      ;; Point and mark should be swapped.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 3) (meep-test-mark-line-column)))
      (should (equal text-expected-region (meep-test-region-as-string))))))

(ert-deftest selection-activate-or-reverse-multiple ()
  "Multiple reversals return to original positions.

Verifies: reversing twice restores original point/mark."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Start selection and move forward.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next))
      ;; Cursor: ABCDEF
      ;;            ^  (point at D, mark at A)
      ;; Record original positions and content.
      (let ((orig-point (meep-test-point-line-column))
            (orig-mark (meep-test-mark-line-column))
            (orig-content (meep-test-region-as-string)))
        ;; Reverse twice.
        (simulate-input-for-meep
          '(:state visual :command meep-region-activate-or-reverse)
          '(:state visual :command meep-region-activate-or-reverse))
        ;; Should be back to original.
        (should (equal 'visual (bray-state)))
        (should (equal orig-point (meep-test-point-line-column)))
        (should (equal orig-mark (meep-test-mark-line-column)))
        (should (equal orig-content (meep-test-region-as-string)))))))

;; region-activate-and-reverse-motion tests

(ert-deftest selection-activate-and-reverse-motion-symbol ()
  "Reverse motion after symbol movement.

Verifies: activates region spanning the last motion."
  (let ((text-initial "foo bar baz"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move forward by symbol and immediately reverse (same block preserves `last-command').
      ;; Cursor: foo bar baz
      ;;         ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      ;; Cursor: foo bar baz
      ;;         ^   (point back at start, mark at end of motion)
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 4) (meep-test-mark-line-column)))
      (should (equal "foo " (meep-test-region-as-string))))))

(ert-deftest selection-activate-and-reverse-motion-line ()
  "Reverse motion after line movement.

Verifies: activates region spanning the last line motion."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: ABC
      ;;                ^
      ;; Move down and immediately reverse (same block preserves `last-command').
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      ;; Point should be back at line 1, mark at line 2.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(2 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\n" (meep-test-region-as-string))))))

;; region-syntax-expand tests

(ert-deftest selection-syntax-expand-word ()
  "Expand region to select a word.

Verifies: region covers the word under cursor."
  (let* ((text-initial "ab cd ef")
         (text-expected-region "cd"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into middle of 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand to select 'cd'.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal text-expected-region (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-word-then-space ()
  "Expand region from word to include adjacent space.

Verifies: second expansion includes whitespace."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; First expand: select 'cd'.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "cd" (meep-test-region-as-string)))
      ;; Second expand: include adjacent space.
      (simulate-input-for-meep
        '(:state visual :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal " cd " (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-at-word-start ()
  "Expand region when cursor is at word start.

Verifies: expansion works from word boundary."
  (let* ((text-initial "ab cd ef")
         (text-expected-region "cd"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to start of 'cd'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;            ^
      ;; Expand to select 'cd'.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal text-expected-region (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-from-space ()
  "Expand region when cursor is on whitespace.

Verifies: expansion from whitespace selects adjacent word."
  (let ((text-initial "ab  cd"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to first space (between ab and cd).
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab  cd
      ;;           ^
      ;; Expand selects adjacent word.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 2) (meep-test-mark-line-column)))
      ;; Expect to select "ab" (the word before the space).
      (should (equal "ab" (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-at-buffer-start ()
  "Expand region at buffer start.

Verifies: expansion handles buffer boundary."
  (let ((text-initial "abc def"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor at buffer start.
      ;; Cursor: abc def
      ;;         ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 3) (meep-test-mark-line-column)))
      (should (equal "abc" (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-at-buffer-end ()
  "Expand region at buffer end.

Verifies: expansion handles buffer boundary."
  (let ((text-initial "abc def"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (simulate-input-for-meep
        [?\C-u ?6]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: abc def
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 7) (meep-test-mark-line-column)))
      (should (equal "def" (meep-test-region-as-string))))))

;; region-syntax-contract tests

(ert-deftest selection-syntax-contract-to-word ()
  "Contract region back to word after expansion.

Verifies: contraction returns to previous expansion level."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand twice.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand)
        '(:state visual :command meep-region-syntax-expand))
      (let ((expanded-content (meep-test-region-as-string)))
        ;; Contract once.
        (simulate-input-for-meep
          '(:state visual :command meep-region-syntax-contract))
        ;; Should be back to just 'cd'.
        (should (equal 'visual (bray-state)))
        (should (equal '(1 . 3) (meep-test-point-line-column)))
        (should (equal '(1 . 5) (meep-test-mark-line-column)))
        (should (equal "cd" (meep-test-region-as-string)))
        ;; Verify it was actually larger before.
        (should (> (length expanded-content) 2))))))

(ert-deftest selection-syntax-contract-expand-inverse ()
  "Expand and contract are inverse operations.

Verifies: expand then contract returns to previous selection."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand once to get 'cd'.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (let ((first-expand (meep-test-region-as-string))
            (first-point (meep-test-point-line-column))
            (first-mark (meep-test-mark-line-column)))
        (should (equal "cd" first-expand))
        (should (equal '(1 . 3) first-point))
        (should (equal '(1 . 5) first-mark))
        ;; Expand once more to get ' cd '.
        (simulate-input-for-meep
          '(:state visual :command meep-region-syntax-expand))
        (should (equal " cd " (meep-test-region-as-string)))
        ;; Contract once - should return to 'cd'.
        (simulate-input-for-meep
          '(:state visual :command meep-region-syntax-contract))
        (should (equal 'visual (bray-state)))
        (should (equal first-point (meep-test-point-line-column)))
        (should (equal first-mark (meep-test-mark-line-column)))
        (should (equal first-expand (meep-test-region-as-string)))))))

(ert-deftest selection-syntax-contract-at-minimum ()
  "Contract when already at minimum expansion level.

Verifies: contraction at minimum deactivates region gracefully."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand once to get 'cd'.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "cd" (meep-test-region-as-string)))
      ;; Contract when at minimum - region becomes inactive.
      (simulate-input-for-meep
        '(:state visual :command meep-region-syntax-contract))
      ;; Region deactivates when contracting past minimum.
      (should-not (region-active-p))
      (should (equal 'normal (bray-state))))))

;; region-mark-bounds-of-char tests

(ert-deftest selection-mark-bounds-of-char-inner-parens ()
  "Mark inner content of parentheses.

Verifies: selects content inside delimiters, excluding delimiters."
  (let ((text-initial "a0 (b1 c2) d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the parentheses.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 c2) d3
      ;;              ^
      ;; Mark inner with '(' character.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      ;; Point at start of inner content, mark at end.
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "b1 c2" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-parens ()
  "Mark outer content of parentheses.

Verifies: selects content including delimiters."
  (let ((text-initial "a0 (b1 c2) d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the parentheses.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 c2) d3
      ;;              ^
      ;; Mark outer with '(' character.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "(")
      (should (equal 'visual (bray-state)))
      ;; Point at opening paren, mark after closing paren.
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "(b1 c2)" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-inner-quotes ()
  "Mark inner content of double quotes.

Verifies: selects content inside quotes, excluding quotes."
  (let ((text-initial "a0 \"b1 c2\" d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the quotes.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 "b1 c2" d3
      ;;              ^
      ;; Mark inner with '"' character.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "\"")
      (should (equal 'visual (bray-state)))
      ;; Point at start of inner content, mark at end.
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "b1 c2" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-quotes ()
  "Mark outer content of double quotes.

Verifies: selects content including quotes."
  (let ((text-initial "a0 \"b1 c2\" d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the quotes.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 "b1 c2" d3
      ;;              ^
      ;; Mark outer with '"' character.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "\"")
      (should (equal 'visual (bray-state)))
      ;; Point at opening quote, mark after closing quote.
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "\"b1 c2\"" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-inner-brackets ()
  "Mark inner content of square brackets.

Verifies: selects content inside brackets, excluding brackets."
  (let ((text-initial "a0 [b1 c2] d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the brackets.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 [b1 c2] d3
      ;;              ^
      ;; Mark inner with '[' character.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "[")
      (should (equal 'visual (bray-state)))
      ;; Point at start of inner content, mark at end.
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "b1 c2" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-nested ()
  "Mark bounds in nested delimiters.

Verifies: selects innermost matching pair."
  (let ((text-initial "a0 (b1 (c2) d3) e4"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the inner parentheses.
      (simulate-input-for-meep
        [?\C-u ?7]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 (c2) d3) e4
      ;;                 ^
      ;; Mark inner with '(' - should get innermost.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      ;; Point at start of innermost content, mark at end.
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "c2" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-nested-outer ()
  "Mark outer bounds in nested delimiters.

Verifies: selects innermost matching outer pair."
  (let ((text-initial "a0 (b1 (c2) d3) e4"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move between outer and inner parens (at b1).
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 (c2) d3) e4
      ;;              ^
      ;; Mark outer with '(' - should get the outer pair since cursor is not inside inner.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "(")
      (should (equal 'visual (bray-state)))
      ;; Point at outer open paren, mark after outer close paren.
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 15) (meep-test-mark-line-column)))
      (should (equal "(b1 (c2) d3)" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-bracket-inside-string ()
  "Characterize: a bracket inside a string is counted by the text backend.

The enclosing-bracket search scans the buffer text, not its syntax, so the
parentheses sitting inside the string are matched in preference to the outer
pair.  Pins the text backend explicitly - `emacs-lisp-mode' auto-resolves to the
syntax backend, which instead skips a bracket inside a string (the flip is
`selection-mark-bounds-of-char-syntax-skips-string-bracket')."
  (let ((text-initial "(f \"(x)\")"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move onto `x', inside the string's parentheses.
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state normal :command meep-move-char-next))
      ;; Cursor: (f "(x)")
      ;;              ^
      ;; Mark inner with '(' using the text backend.
      (let ((meep-syntax-backend 'text))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "("))
      (should (equal 'visual (bray-state)))
      ;; The inner parentheses win, not the outer pair: content is "x".
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "x" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-syntax-skips-string-bracket ()
  "The syntax backend skips a bracket inside a string.

With `meep-syntax-backend' set to `syntax' the parentheses inside the string are
ignored and the enclosing pair is the outer one - the flip of
`selection-mark-bounds-of-char-bracket-inside-string' on the same input."
  (let ((text-initial "(f \"(x)\")"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move onto `x', inside the string's parentheses.
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state normal :command meep-move-char-next))
      ;; Mark inner with '(' using the syntax backend.
      (let ((meep-syntax-backend 'syntax))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "("))
      (should (equal 'visual (bray-state)))
      ;; The outer pair wins: the inner parentheses are string contents.
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal '(1 . 8) (meep-test-mark-line-column)))
      (should (equal "f \"(x)\"" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-syntax-nested ()
  "The syntax backend resolves the innermost enclosing pair, ignoring a string bracket.

A nested pair whose inner content holds an open paren inside a string: the syntax
backend nests to the inner real pair and does not count the in-string paren as a
level.  This diverges from text - which counts it and overshoots - so the input
exercises `meep--syntax-enclosing-pair-from-syntax' and fails if the dispatch
falls through to text."
  (let ((text-initial "(a (b \"(\" c) d)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move onto `b', inside the inner parentheses.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next))
      (let ((meep-syntax-backend 'syntax))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "("))
      (should (equal 'visual (bray-state)))
      ;; The inner real pair, not widened by the `(' inside the string; the text
      ;; backend instead overshoots to `b "(" c) d'.
      (should (equal "b \"(\" c" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-syntax-fallback-non-paren ()
  "A non-paren delimiter falls back to the text backend under `syntax'.

`<' has no paren syntax in `text-mode', so the syntax backend cannot match it and
the text scan is used - the mark still succeeds."
  (let ((text-initial "<a b>"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the angle brackets.
      (simulate-input-for-meep
        [?\C-u ?2]
        '(:state normal :command meep-move-char-next))
      (let ((meep-syntax-backend 'syntax))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "<"))
      (should (equal 'visual (bray-state)))
      (should (equal "a b" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-auto-backend-prog-mode ()
  "The nil (auto) backend resolves to syntax in a `prog-mode' buffer.

With `meep-syntax-backend' left at its default, `emacs-lisp-mode' uses the
syntax backend, so a bracket inside a string is skipped without setting the
backend explicitly."
  (let ((text-initial "(f \"(x)\")"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state normal :command meep-move-char-next))
      ;; Auto backend (nil) -> syntax in a prog-mode derivative.
      (let ((meep-syntax-backend nil))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "("))
      (should (equal 'visual (bray-state)))
      (should (equal "f \"(x)\"" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-auto-backend-non-prog ()
  "The nil (auto) backend resolves to text in a non-prog-mode buffer.

The mirror of `selection-mark-bounds-of-char-auto-backend-prog-mode'.  In a
non-prog buffer the text and syntax backends cannot diverge - there is no string
syntax for them to disagree about - so the resolution is asserted directly
through `meep--syntax-backend-resolve' rather than through motion behaviour."
  (with-meep-test ""
    (text-mode)
    (let ((meep-syntax-backend nil))
      (should (eq 'text (meep--syntax-backend-resolve))))))

(ert-deftest selection-mark-bounds-of-char-syntax-count-peels-region ()
  "A count feeds a region back into the syntax backend (the has-region branch).

`(a \"(\" (x) b)' with point on `x' and a count of 2: the first mark finds the
inner `(x)', then the outward peel passes that region to
`meep--syntax-enclosing-pair-from-syntax' (its has-region containment branch),
which resolves the enclosing real pair and skips the `(' inside the string.  The
text backend miscounts the in-string `(', so this exercises the syntax path and
diverges from text."
  (let ((text-initial "(a \"(\" (x) b)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move onto `x', inside the inner parentheses.
      (simulate-input-for-meep
        [?\C-u ?8]
        '(:state normal :command meep-move-char-next))
      (let ((meep-syntax-backend 'syntax))
        (simulate-input-for-meep
          [?\C-u ?2]
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "("))
      (should (equal 'visual (bray-state)))
      ;; The outer real pair's inner content; the `(' inside the string is not a
      ;; level, so it is skipped during the peel.
      (should (equal "a \"(\" (x) b" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-syntax-outer ()
  "Mark-outer resolves the enclosing pair through the syntax backend.

`(a \"(\" b)' with point on `b', marking outer with `(': the syntax backend skips
the `(' inside the string and selects the whole real pair, delimiters included.
The text backend starts at the in-string `(', so this exercises the syntax path
and diverges from text."
  (let ((text-initial "(a \"(\" b)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move onto `b'.
      (simulate-input-for-meep
        [?\C-u ?7]
        '(:state normal :command meep-move-char-next))
      (let ((meep-syntax-backend 'syntax))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-outer)
          "("))
      (should (equal 'visual (bray-state)))
      ;; The whole pair including delimiters; the text backend instead starts at
      ;; the `(' inside the string.
      (should (equal "(a \"(\" b)" (meep-test-region-as-string))))))


;; ---------------------------------------------------------------------------
;; Selection Tests - Additional Coverage

;; activate-or-reverse additional tests

(ert-deftest selection-activate-or-reverse-when-inactive ()
  "Activate region without reversing when region is inactive.

Verifies: activates region at existing mark position without moving point."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Start selection, move, then deactivate.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next))
      ;; Cursor: ABCDEF
      ;;            ^  (point at D, mark at A, content="ABC")
      ;; Deactivate using meep command.
      (simulate-input-for-meep
        '(:state visual :command meep-region-disable))
      (should-not (region-active-p))
      (should (equal 'normal (bray-state)))
      (let ((point-before (meep-test-point-line-column))
            (mark-before (meep-test-mark-line-column)))
        ;; Activate-or-reverse should reactivate without moving.
        (simulate-input-for-meep
          '(:state normal :command meep-region-activate-or-reverse))
        (should (equal 'visual (bray-state)))
        ;; Point should not have moved.
        (should (equal point-before (meep-test-point-line-column)))
        (should (equal mark-before (meep-test-mark-line-column)))
        ;; Content should be same as before deactivation.
        (should (equal "ABC" (meep-test-region-as-string)))))))

;; region-toggle tests

(ert-deftest selection-region-toggle-enable-disable ()
  "Toggle on activates region, deactivate-mark deactivates.

Verifies: basic toggle on behavior and state transitions."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Initially no region.
      ;; Cursor: ABCDEF
      ;;         ^
      (should-not (region-active-p))
      (should (equal 'normal (bray-state)))
      ;; Toggle on.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (should (equal 'visual (bray-state)))
      ;; Move to create a selection.
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next))
      ;; Cursor: ABCDEF
      ;;           ^  (point at C, mark at A)
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "AB" (meep-test-region-as-string)))
      ;; Deactivate using meep command.
      (simulate-input-for-meep
        '(:state visual :command meep-region-disable))
      (should-not (region-active-p))
      (should (equal 'normal (bray-state))))))

(ert-deftest selection-region-toggle-double ()
  "Toggle on then deactivate returns to normal state.

Verifies: region can be activated then deactivated."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: ABCDEF
      ;;         ^
      ;; Initially no region.
      (should-not (region-active-p))
      (let ((point-before (meep-test-point-line-column)))
        ;; Toggle on then immediately off.
        (simulate-input-for-meep
          '(:state normal :command meep-region-toggle))
        (should (equal 'visual (bray-state)))
        (simulate-input-for-meep
          '(:state visual :command meep-region-disable))
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        ;; Point should be unchanged.
        (should (equal point-before (meep-test-point-line-column)))))))

;; activate-and-reverse-motion edge cases

(ert-deftest selection-activate-and-reverse-motion-no-prior-motion ()
  "Reverse motion with no prior motion does nothing.

Verifies: command handles no-motion case gracefully."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: ABCDEF
      ;;         ^
      (let ((point-before (meep-test-point-line-column)))
        ;; No prior motion - should do nothing.
        (simulate-input-for-meep
          '(:state normal :command meep-region-activate-and-reverse-motion))
        ;; Should remain in normal state, no region.
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))))))

(ert-deftest selection-activate-and-reverse-motion-backward ()
  "Reverse motion after backward symbol movement.

Verifies: works correctly with backward motions."
  (let ((text-initial "foo bar baz"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end first.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next))
      ;; Cursor: foo bar baz
      ;;                 ^
      ;; Now move to end of "baz" and reverse.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal '(1 . 11) (meep-test-mark-line-column)))
      (should (equal "baz" (meep-test-region-as-string))))))

;; syntax-expand edge cases

(ert-deftest selection-syntax-expand-single-char ()
  "Expand on single character word.

Verifies: expansion works on single-char words."
  (let ((text-initial "x pq y"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor at 'x'.
      ;; Cursor: x pq y
      ;;         ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 1) (meep-test-mark-line-column)))
      (should (equal "x" (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-empty-buffer ()
  "Expand in empty buffer.

Verifies: handles empty buffer gracefully, informative message displayed."
  (let ((text-initial ""))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: (empty buffer)
      ;;         ^
      (let ((point-before (meep-test-point-line-column)))
        ;; Expand does nothing in empty buffer.
        (simulate-input-for-meep
          '(:state normal :command meep-region-syntax-expand))
        ;; Should remain in normal state, no region, point unchanged.
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))
        ;; Verify informative message was displayed.
        (should (equal '("No syntax around the point (empty buffer?)") (meep-test-messages)))))))

;; syntax-contract edge cases

(ert-deftest selection-syntax-contract-no-region ()
  "Contract when no region is active.

Verifies: command signals error when no region exists."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: ab cd ef
      ;;         ^
      ;; No region active, no mark set.
      (should-not (region-active-p))
      (let ((point-before (meep-test-point-line-column)))
        ;; Contract should signal an error.
        (should-error-with-message
            (simulate-input-for-meep
              '(:state normal :command meep-region-syntax-contract))
          'error
          "The mark is not set now, so there is no region")
        ;; Should remain in normal state, point unchanged.
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))))))

;; mark-bounds-of-char additional delimiter types

(ert-deftest selection-mark-bounds-of-char-curly-braces ()
  "Mark inner content of curly braces.

Verifies: {} delimiters work correctly."
  (let ((text-initial "a0 {b1 c2} d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the braces.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 {b1 c2} d3
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "{")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "b1 c2" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-angle-brackets ()
  "Mark inner content of angle brackets.

Verifies: <> delimiters work correctly."
  (let ((text-initial "a0 <b1 c2> d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the brackets.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 <b1 c2> d3
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "<")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "b1 c2" (meep-test-region-as-string))))))

;; mark-bounds-of-char edge cases

(ert-deftest selection-mark-bounds-of-char-on-delimiter ()
  "Mark bounds when cursor is on the delimiter itself.

Verifies: cursor on delimiter itself does not select content."
  (let ((text-initial "a0 (b1 c2) d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to the opening paren.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 c2) d3
      ;;            ^
      (let ((point-before (meep-test-point-line-column)))
        ;; When on the delimiter, mark-bounds does not select.
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "(")
        ;; Should remain in normal state, no region, point unchanged.
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))))))

(ert-deftest selection-mark-bounds-of-char-zero-count-no-op ()
  "A count of 0 marks nothing and does not error.
`C-u 0' is a no-op - the motion leaves point and the buffer untouched rather than
searching with a zero count, see `meep--region-mark-bounds-of-char-impl'."
  (let ((text-initial "a (b c) d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the parens.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      (let ((point-before (meep-test-point-line-column)))
        ;; `C-u 0' then mark is a no-op: no region, no error, point unchanged.
        (simulate-input-for-meep
          [?\C-u ?0]
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "(")
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))))))

(ert-deftest selection-mark-bounds-of-char-contextual-case-sensitive ()
  "A letter-bearing distinct delimiter pairs case-sensitively.
Under an ambient `case-fold-search', a lowercase `<b>' pair must not match an
uppercase `<B>' token, see `meep--syntax-enclosing-pair-from-text'."
  (let ((text-initial "a <B>content</B> b")
        (meep-match-bounds-of-char-contextual '(("<b>" . "</b>"))))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (setq case-fold-search t)
      ;; Move inside the uppercase `<B>content</B>'.
      (simulate-input-for-meep
        [?\C-u ?6]
        '(:state normal :command meep-move-char-next))
      ;; Return triggers contextual detection; the only candidate is the uppercase
      ;; token, and case-sensitive matching rejects the lowercase `<b>' pair, so
      ;; nothing is marked.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        [return])
      (should-not (region-active-p))
      (should (equal 'normal (bray-state))))))

(ert-deftest selection-mark-bounds-of-char-syntax-ignores-string ()
  "In a `prog-mode' buffer the syntax backend ignores a bracket inside a string.
A `(' inside the string must not be taken as the enclosing open; the real outer
list is selected instead, see `meep-syntax-backend'."
  (let ((text-initial "(a \"(\" b)")
        (meep-syntax-backend nil))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move onto `b', inside the real list but past the string's stray `('.
      (simulate-input-for-meep
        [?\C-u ?6]
        '(:state normal :command meep-move-char-next))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal "(a \"(\" b)" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-count-peels-outward ()
  "A count selects the Nth enclosing pair for distinct brackets.
The count is honoured by peeling outward, see `meep--peel-outward'; `C-u 2'
selects the second enclosing pair rather than the innermost."
  (let ((text-initial "((x))"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move onto `x', inside both pairs.
      (simulate-input-for-meep
        [?\C-u ?2]
        '(:state normal :command meep-move-char-next))
      (simulate-input-for-meep
        [?\C-u ?2]
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "(")
      (should (equal "((x))" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-empty-content ()
  "Mark bounds with empty content inside delimiters.

Verifies: handles () with nothing inside."
  (let ((text-initial "a0 () b1"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to between the parens.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 () b1
      ;;             ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      ;; Region should be active but empty.
      (should (equal 'visual (bray-state)))
      ;; Point and mark are at the same position.
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 4) (meep-test-mark-line-column)))
      (should (equal "" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-single-char-content ()
  "Mark bounds with single character content.

Verifies: handles (x) correctly."
  (let ((text-initial "a0 (x) b1"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the parens.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (x) b1
      ;;             ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "x" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-not-inside-delimiter ()
  "Mark bounds when cursor is not inside any matching delimiter.

Verifies: handles no-match case gracefully."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor in middle of text, no parens anywhere.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      (let ((point-before (meep-test-point-line-column)))
        ;; Try to mark inside parens - should fail gracefully.
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "(")
        ;; Should remain in normal state, no region, point unchanged.
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))))))

(ert-deftest selection-mark-bounds-of-char-wrong-delimiter ()
  "Mark bounds searching for wrong delimiter type.

Verifies: searching for [ when inside () fails gracefully."
  (let ((text-initial "a0 (b1 c2) d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside parentheses.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 c2) d3
      ;;              ^
      (let ((point-before (meep-test-point-line-column)))
        ;; Try to mark with [ - should fail.
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "[")
        ;; Should remain in normal state, no region, point unchanged.
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))))))

(ert-deftest selection-mark-bounds-of-char-unmatched ()
  "Mark bounds with unmatched delimiter.

Verifies: handles only opening paren gracefully."
  (let ((text-initial "a0 (b1 c2 d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move after opening paren.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 c2 d3
      ;;              ^
      (let ((point-before (meep-test-point-line-column)))
        ;; Try to mark - should fail (no closing paren).
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-bounds-of-char-inner)
          "(")
        ;; Should remain in normal state, no region, point unchanged.
        (should-not (region-active-p))
        (should (equal 'normal (bray-state)))
        (should (equal point-before (meep-test-point-line-column)))))))

;; Multi-line tests

(ert-deftest selection-mark-bounds-of-char-multi-line ()
  "Mark bounds spanning multiple lines.

Verifies: delimiters work across line boundaries."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AB (\n"
          "CD EF\n"
          ") GH\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to second line inside parens.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor on line 2: CD EF
      ;;                    ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(3 . 0) (meep-test-mark-line-column)))
      ;; Content should be the multi-line inner text: newline + "CD EF" + newline.
      (should (equal "\nCD EF\n" (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-across-lines ()
  "Expand region that crosses line boundary.

Verifies: syntax expansion works at line boundaries."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor at start of first line.
      ;; Cursor: ABC
      ;;         ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 3) (meep-test-mark-line-column)))
      ;; Should select "ABC".
      (should (equal "ABC" (meep-test-region-as-string))))))

;; Expand-to-line-bounds test

(ert-deftest selection-expand-to-line-bounds ()
  "Expand region to full line bounds.

Verifies: region-expand-to-line-bounds selects entire line."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Start selection in middle of second line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next))
      ;; Cursor line 2: DEF
      ;;                  ^  (point at E, mark at D)
      ;; Region is just "E".
      (should (equal "E" (meep-test-region-as-string)))
      ;; Expand to line bounds.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      ;; Point moves to start of next line, mark to start of current line.
      (should (equal '(3 . 0) (meep-test-point-line-column)))
      (should (equal '(2 . 0) (meep-test-mark-line-column)))
      ;; Region should now cover entire line including newline.
      (should (equal "DEF\n" (meep-test-region-as-string))))))

;; Interaction tests

(ert-deftest selection-mark-bounds-then-reverse ()
  "Mark bounds then reverse point and mark.

Verifies: activate-or-reverse works after mark-bounds."
  (let ((text-initial "a0 (b1 c2) d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside parens.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 c2) d3
      ;;              ^
      ;; Mark inner.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (let ((point-before (meep-test-point-line-column))
            (mark-before (meep-test-mark-line-column)))
        ;; Reverse.
        (simulate-input-for-meep
          '(:state visual :command meep-region-activate-or-reverse))
        ;; Point and mark should be swapped.
        (should (equal 'visual (bray-state)))
        (should (equal mark-before (meep-test-point-line-column)))
        (should (equal point-before (meep-test-mark-line-column)))
        ;; Content should be same.
        (should (equal "b1 c2" (meep-test-region-as-string)))))))

;; Additional outer delimiter tests

(ert-deftest selection-mark-bounds-of-char-outer-curly-braces ()
  "Mark outer content of curly braces.

Verifies: {} delimiters work correctly with outer."
  (let ((text-initial "a0 {b1 c2} d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the braces.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 {b1 c2} d3
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "{")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "{b1 c2}" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-angle-brackets ()
  "Mark outer content of angle brackets.

Verifies: <> delimiters work correctly with outer."
  (let ((text-initial "a0 <b1 c2> d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the brackets.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 <b1 c2> d3
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "<")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "<b1 c2>" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-brackets ()
  "Mark outer content of square brackets.

Verifies: [] delimiters work correctly with outer."
  (let ((text-initial "a0 [b1 c2] d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the brackets.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 [b1 c2] d3
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "[")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "[b1 c2]" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-single-quotes ()
  "Mark outer content of single quotes.

Verifies: '' delimiters work correctly with outer."
  (let ((text-initial "a0 'b1 c2' d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the quotes.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 'b1 c2' d3
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "'")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "'b1 c2'" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-at-closing-delimiter ()
  "Mark bounds when cursor is on the closing delimiter.

Verifies: works when cursor is on closing delimiter."
  (let ((text-initial "a0 (b1 c2) d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to the closing paren.
      (simulate-input-for-meep
        [?\C-u ?8]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 (b1 c2) d3
      ;;                  ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "b1 c2" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-single-quotes ()
  "Mark inner content of single quotes.

Verifies: '' delimiters work correctly."
  (let ((text-initial "a0 'b1 c2' d3"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the quotes.
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 'b1 c2' d3
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "'")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "b1 c2" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-empty ()
  "Mark outer content with empty delimiters.

Verifies: handles () with nothing inside for outer."
  (let ((text-initial "a0 () b1"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to between the parens.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a0 () b1
      ;;             ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "()" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-multi-line ()
  "Mark outer bounds spanning multiple lines.

Verifies: outer selection works across line boundaries."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "AB (\n"
          "CD EF\n"
          ") GH\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to second line inside parens.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor on line 2: CD EF
      ;;                    ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(3 . 1) (meep-test-mark-line-column)))
      ;; Content should include the parens and multi-line text.
      (should (equal "(\nCD EF\n)" (meep-test-region-as-string))))))

;; Additional syntax-expand tests

(ert-deftest selection-syntax-expand-whitespace-only-buffer ()
  "Expand in buffer with only whitespace.

Verifies: handles whitespace-only buffer gracefully."
  (let ((text-initial "   "))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to middle of whitespace.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next))
      ;; Cursor:    (3 spaces)
      ;;          ^
      ;; Expand selects all whitespace.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 3) (meep-test-mark-line-column)))
      (should (equal "   " (meep-test-region-as-string))))))

;; Additional toggle tests

(ert-deftest selection-region-toggle-preserves-point ()
  "Toggle preserves point position.

Verifies: point does not move when toggling on."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to middle.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ABCDEF
      ;;            ^
      (let ((point-before (meep-test-point-line-column)))
        ;; Toggle on.
        (simulate-input-for-meep
          '(:state normal :command meep-region-toggle))
        ;; Should be in visual state with region active.
        (should (equal 'visual (bray-state)))
        ;; Point should be same.
        (should (equal point-before (meep-test-point-line-column)))
        ;; Mark should also be at same position (no selection yet).
        (should (equal point-before (meep-test-mark-line-column)))))))

;; Expand/contract sequence test

(ert-deftest selection-expand-contract-expand-sequence ()
  "Expand, contract, then expand again.

Verifies: expand/contract/expand sequence works correctly."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand once.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (let ((first-expand (meep-test-region-as-string))
            (first-point (meep-test-point-line-column))
            (first-mark (meep-test-mark-line-column)))
        (should (equal "cd" first-expand))
        (should (equal '(1 . 3) first-point))
        (should (equal '(1 . 5) first-mark))
        ;; Expand twice.
        (simulate-input-for-meep
          '(:state visual :command meep-region-syntax-expand))
        (should (equal " cd " (meep-test-region-as-string)))
        (should (equal '(1 . 2) (meep-test-point-line-column)))
        (should (equal '(1 . 6) (meep-test-mark-line-column)))
        ;; Contract back.
        (simulate-input-for-meep
          '(:state visual :command meep-region-syntax-contract))
        (should (equal "cd" (meep-test-region-as-string)))
        (should (equal first-point (meep-test-point-line-column)))
        (should (equal first-mark (meep-test-mark-line-column)))
        ;; Expand again - should go back to larger.
        (simulate-input-for-meep
          '(:state visual :command meep-region-syntax-expand))
        (should (equal " cd " (meep-test-region-as-string)))
        (should (equal '(1 . 2) (meep-test-point-line-column)))
        (should (equal '(1 . 6) (meep-test-mark-line-column)))))))

;; Multiple expansion levels

(ert-deftest selection-syntax-expand-three-levels ()
  "Expand region through three expansion levels.

Verifies: expansion continues beyond word to include more context."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; First expand: select 'cd'.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "cd" (meep-test-region-as-string)))
      ;; Second expand: include adjacent space.
      (simulate-input-for-meep
        '(:state visual :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal " cd " (meep-test-region-as-string)))
      ;; Third expand: select entire buffer.
      (simulate-input-for-meep
        '(:state visual :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 8) (meep-test-mark-line-column)))
      (should (equal "ab cd ef" (meep-test-region-as-string))))))

;; Mixed nested delimiters

(ert-deftest selection-mark-bounds-of-char-mixed-nested-inner ()
  "Mark inner bounds in mixed nested delimiters.

Verifies: inner selection finds enclosing delimiter of specified type."
  (let ((text-initial "a ([b] c) d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside [b] (to 'b').
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a ([b] c) d
      ;;             ^
      ;; Mark inner ( - should find outer parens.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 8) (meep-test-mark-line-column)))
      (should (equal "[b] c" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-mixed-nested-outer ()
  "Mark outer bounds in mixed nested delimiters.

Verifies: outer selection finds enclosing delimiter of specified type."
  (let ((text-initial "a {(b) c} d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside (b) (to 'b').
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a {(b) c} d
      ;;             ^
      ;; Mark outer { - should find outer braces.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        "{")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should (equal "{(b) c}" (meep-test-region-as-string))))))

;; Adjacent delimiters

(ert-deftest selection-mark-bounds-of-char-adjacent-parens ()
  "Mark bounds with adjacent empty parentheses.

Verifies: selects correct pair, not content from adjacent pair."
  (let ((text-initial "a()()b"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to closing paren of first pair.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a()()b
      ;;           ^
      ;; Mark inner (.
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      ;; Point and mark at same position (empty content).
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 2) (meep-test-mark-line-column)))
      (should (equal "" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-adjacent-quotes ()
  "Mark bounds with adjacent empty quotes.

Verifies: selects correct pair of quotes."
  (let ((text-initial "a\"\"b\"\"c"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to closing quote of first pair.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a""b""c
      ;;           ^
      ;; Mark inner ".
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "\"")
      (should (equal 'visual (bray-state)))
      ;; Point and mark at same position (empty content).
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 2) (meep-test-mark-line-column)))
      (should (equal "" (meep-test-region-as-string))))))

;; Reverse selection sizes

(ert-deftest selection-activate-or-reverse-single-char ()
  "Reverse single character selection.

Verifies: point and mark swap correctly for minimal selection."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Create single-char selection.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next))
      ;; Cursor: ABCDEF
      ;;          ^  (point at B, mark at A)
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "A" (meep-test-region-as-string)))
      ;; Reverse.
      (simulate-input-for-meep
        '(:state visual :command meep-region-activate-or-reverse))
      ;; Point and mark swapped.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 1) (meep-test-mark-line-column)))
      (should (equal "A" (meep-test-region-as-string))))))

(ert-deftest selection-activate-or-reverse-multi-line ()
  "Reverse multi-line selection.

Verifies: point and mark swap correctly across lines."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: ABC
      ;;               ^
      ;; Create multi-line selection.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-move-line-next))
      ;; Cursor line 3: GHI  (point at G, mark at A on line 1)
      ;;               ^
      (should (equal 'visual (bray-state)))
      (should (equal '(3 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\nDEF\n" (meep-test-region-as-string)))
      ;; Reverse.
      (simulate-input-for-meep
        '(:state visual :command meep-region-activate-or-reverse))
      ;; Point and mark swapped.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(3 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\nDEF\n" (meep-test-region-as-string))))))

;; Mark bounds from different positions

(ert-deftest selection-mark-bounds-of-char-just-after-opening ()
  "Mark bounds when cursor is just after opening delimiter.

Verifies: works when cursor is at first content character."
  (let ((text-initial "a (b c) d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to just after opening paren (at 'b').
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a (b c) d
      ;;            ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "b c" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-just-before-closing ()
  "Mark bounds when cursor is just before closing delimiter.

Verifies: works when cursor is at last content character."
  (let ((text-initial "a (b c) d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to just before closing paren (at 'c').
      (simulate-input-for-meep
        [?\C-u ?4]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a (b c) d
      ;;              ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "b c" (meep-test-region-as-string))))))

;; Whitespace and newline content

(ert-deftest selection-mark-bounds-of-char-whitespace-only-content ()
  "Mark bounds with whitespace-only content inside delimiters.

Verifies: whitespace content selected correctly."
  (let ((text-initial "a (   ) b"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside parens (to whitespace).
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a (   ) b
      ;;             ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "   " (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-newline-only-content ()
  "Mark bounds with newline-only content inside delimiters.

Verifies: newline content selected correctly."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "a (\n"
          ") b")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: a (
      ;;                ^
      ;; Move to second line (inside parens).
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      ;; Cursor line 2: ) b
      ;;               ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(2 . 0) (meep-test-mark-line-column)))
      (should (equal "\n" (meep-test-region-as-string))))))

;; Unicode content

(ert-deftest selection-syntax-expand-unicode ()
  "Expand region to select unicode word.

Verifies: word boundaries work with multibyte characters."
  ;; Use Chinese characters which are reliably 3 bytes each in UTF-8.
  (let ((text-initial
         ;; format-next-line: off
         (concat
          (string #x4e2d) (string #x6587) " " (string #x6d4b) (string #x8bd5) " "
          (string #x5b57) (string #x7b26)))) ; "中文 测试 字符"
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into first word.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next))
      ;; Cursor: 中文 测试 字符
      ;;          ^
      ;; Expand to select word.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Each Chinese character is 2 columns wide visually.
      (should (equal '(1 . 4) (meep-test-mark-line-column)))
      (should (equal (concat (string #x4e2d) (string #x6587)) (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-unicode-content ()
  "Mark bounds with unicode content inside delimiters.

Verifies: point and mark positions correct with multibyte content."
  ;; Use Chinese characters which are reliably 3 bytes each in UTF-8.
  (let
      ((text-initial (concat "a (" (string #x4e2d) (string #x6587) (string #x5b57) ") b"))) ; "a (中文字) b"
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside parens.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a (中文字) b
      ;;             ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      ;; Each Chinese character is 2 columns wide (3 chars = 6 columns).
      (should (equal '(1 . 9) (meep-test-mark-line-column)))
      (should
       (equal
        (concat (string #x4e2d) (string #x6587) (string #x5b57)) (meep-test-region-as-string))))))

;; Contextual mark bounds

(ert-deftest selection-mark-bounds-of-char-contextual-inner ()
  "Mark inner bounds using contextual detection.

Verifies: pressing Return auto-detects enclosing delimiter."
  (let ((text-initial "a (b c) d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside parens.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a (b c) d
      ;;            ^
      ;; Mark inner with Return (contextual detection).
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        [return])
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "b c" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-contextual-outer ()
  "Mark outer bounds using contextual detection.

Verifies: pressing Return auto-detects enclosing delimiter for outer."
  (let ((text-initial "a (b c) d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside parens.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a (b c) d
      ;;            ^
      ;; Mark outer with Return (contextual detection).
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        [return])
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 7) (meep-test-mark-line-column)))
      (should (equal "(b c)" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-contextual-multi-char-wins ()
  "Contextual mark keeps a multi-char pair over its single-char form.
With `**' before `*' in `meep-match-bounds-of-char-contextual', the `*' candidate
only re-pairs the `**' delimiter, so it must not supersede it, see
`meep--bounds-supersedes-p'; the outer mark spans the whole `**...**' rather than the
`*'-clipped `*bold*' inside it."
  (let ((text-initial "**bold**")
        (meep-match-bounds-of-char-contextual '(("**" . "**") ("*" . "*"))))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor inside `bold'.
      (goto-char 4)
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-outer)
        [return])
      (should (equal 'visual (bray-state)))
      (should (equal "**bold**" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-contextual-quotes ()
  "Mark contextual bounds inside quotes.

Verifies: contextual detection works with quote delimiters."
  (let ((text-initial "a \"b c\" d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside quotes.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: a "b c" d
      ;;            ^
      ;; Mark inner with Return (contextual detection).
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        [return])
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "b c" (meep-test-region-as-string))))))

(ert-deftest selection-mark-bounds-of-char-contextual-outer-with-quotes ()
  "Mark outer bounds using contextual detection.

Verifies: pressing Return auto-detects enclosing delimiter."
  (let ((text-initial "(\"b\" c \"d\")"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside parens (5 times).
      (simulate-input-for-meep
        [?\C-u ?5]
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ("b" c "d")
      ;;              ^
      (should (equal ?c (char-after)))
      ;; Mark inner with Return (contextual detection).
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        [return])
      ;; Marks the whole interior (excluding the parens), rather than only the
      ;; span between the two strings.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal '(1 . 10) (meep-test-mark-line-column)))
      (should (equal "\"b\" c \"d\"" (meep-test-region-as-string))))))

;; Expand to line bounds edge cases

(ert-deftest selection-expand-to-line-bounds-first-line ()
  "Expand to line bounds on first line of multi-line buffer.

Verifies: first line expands correctly with point moving to line 2."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: ABC
      ;;               ^
      ;; Create partial selection on first line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next))
      ;; Cursor: ABC  (point at C, mark at B)
      ;;           ^
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 1) (meep-test-mark-line-column)))
      (should (equal "B" (meep-test-region-as-string)))
      ;; Expand to line bounds.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      ;; Point moves to start of next line, mark to start of current line.
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\n" (meep-test-region-as-string))))))

(ert-deftest selection-expand-to-line-bounds-last-line ()
  "Expand to line bounds on last line without trailing newline.

Verifies: last line expands to end of buffer."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI"))) ; No trailing newline
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to last line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next))
      ;; Cursor line 3: GHI  (point at I, mark at H)
      ;;                  ^
      (should (equal '(3 . 2) (meep-test-point-line-column)))
      (should (equal '(3 . 1) (meep-test-mark-line-column)))
      (should (equal "H" (meep-test-region-as-string)))
      ;; Expand to line bounds.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      ;; Point moves to end of buffer, mark to start of line.
      (should (equal '(3 . 3) (meep-test-point-line-column)))
      (should (equal '(3 . 0) (meep-test-mark-line-column)))
      (should (equal "GHI" (meep-test-region-as-string))))))

(ert-deftest selection-expand-to-line-bounds-single-line-buffer ()
  "Expand to line bounds in single-line buffer.

Verifies: expands to entire buffer content."
  (let ((text-initial "ABCDEF"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Create partial selection in middle.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next))
      ;; Cursor: ABCDEF  (point at D, mark at C)
      ;;            ^
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 2) (meep-test-mark-line-column)))
      (should (equal "C" (meep-test-region-as-string)))
      ;; Expand to line bounds.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      ;; Point at end of buffer, mark at start.
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "ABCDEF" (meep-test-region-as-string))))))

(ert-deftest selection-expand-to-line-bounds-spanning-multiple ()
  "Expand to line bounds when selection spans multiple lines.

Verifies: expands to cover full lines of the span."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: ABC
      ;;               ^
      ;; Create selection spanning lines 1-2 (partial on each line).
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-move-char-next))
      ;; Point on line 2, mark on line 1.
      (should (equal '(2 . 2) (meep-test-point-line-column)))
      (should (equal '(1 . 1) (meep-test-mark-line-column)))
      (should (equal "BC\nDE" (meep-test-region-as-string)))
      ;; Expand to line bounds.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      ;; Point moves to start of line 3, mark to start of line 1.
      (should (equal '(3 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\nDEF\n" (meep-test-region-as-string))))))

(ert-deftest selection-expand-to-line-bounds-extend-no-trailing-newline ()
  "Extend a whole-line selection forward in a buffer with no trailing newline.

Regression: extending past the last line searched downward for a blank line
that never existed, so `line-move' stopped at end-of-buffer without the loop
ever terminating (infinite loop / hard hang)."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI"))) ; No trailing newline.
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select the whole of line 1.
      ;; Cursor line 1: ABC
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\n" (meep-test-region-as-string)))
      ;; Extend again: with the bug this never returns.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      ;; Extends over the remaining non-empty lines to end of buffer.
      (should (equal '(3 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\nDEF\nGHI" (meep-test-region-as-string))))))

(ert-deftest selection-expand-to-line-bounds-extend-backward-at-buffer-start ()
  "Extend a whole-line selection backward when already at the first line.

Regression: the backward branch stepped with `forward-char' -1 past
`point-min', signaling `beginning-of-buffer' instead of stopping cleanly."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select the whole of line 1, then reverse so point is at the top.
      ;; Cursor line 1: ABC
      ;;                ^
      (simulate-input-for-meep
        '(:state normal :command meep-region-expand-to-line-bounds)
        '(:state visual :command meep-region-activate-and-reverse))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(2 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\n" (meep-test-region-as-string)))
      ;; Extend again: with the bug this signals `beginning-of-buffer'.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      ;; Nothing precedes line 1, so the selection is unchanged.
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(2 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\n" (meep-test-region-as-string))))))

(ert-deftest selection-expand-to-line-bounds-extend-backward-through-to-start ()
  "Extend a whole-line selection backward, scanning up to the buffer start.

Regression: the backward scan reached `point-min' with `forward-char' -1,
signaling `beginning-of-buffer' mid-loop instead of terminating."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "ABC\n"
          "DEF\n"
          "GHI\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select the whole of line 2, then reverse so point is at its top.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-region-expand-to-line-bounds)
        '(:state visual :command meep-region-activate-and-reverse))
      (should (equal 'visual (bray-state)))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal '(3 . 0) (meep-test-mark-line-column)))
      (should (equal "DEF\n" (meep-test-region-as-string)))
      ;; Extend again: scans up through line 1 to the buffer start.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(3 . 0) (meep-test-mark-line-column)))
      (should (equal "ABC\nDEF\n" (meep-test-region-as-string))))))

;; Contract after movement

(ert-deftest selection-syntax-contract-after-movement ()
  "Contract after point has moved from expansion position.

Verifies: contract deactivates region when expansion state is lost."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand to word.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "cd" (meep-test-region-as-string)))
      ;; Move point within the selection.
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal "d" (meep-test-region-as-string)))
      ;; Contract - should deactivate since expansion state is lost.
      (simulate-input-for-meep
        '(:state visual :command meep-region-syntax-contract))
      (should-not (region-active-p))
      (should (equal 'normal (bray-state))))))

;; Selection to buffer bounds

(ert-deftest selection-syntax-expand-to-buffer ()
  "Expand until entire buffer is selected.

Verifies: expansion stops when buffer bounds are reached."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand three times to reach buffer bounds.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand))
      (should (equal "cd" (meep-test-region-as-string)))
      (simulate-input-for-meep
        '(:state visual :command meep-region-syntax-expand))
      (should (equal " cd " (meep-test-region-as-string)))
      (simulate-input-for-meep
        '(:state visual :command meep-region-syntax-expand))
      ;; Now at buffer bounds.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 8) (meep-test-mark-line-column)))
      (should (equal "ab cd ef" (meep-test-region-as-string))))))

(ert-deftest selection-syntax-expand-at-buffer-bounds ()
  "Expand when already at buffer bounds.

Verifies: expansion is a no-op at maximum extent."
  (let ((text-initial "ab cd ef"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move into 'cd'.
      (simulate-input-for-meep
        [?\C-u ?3]
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ab cd ef
      ;;             ^
      ;; Expand to buffer bounds.
      (simulate-input-for-meep
        '(:state normal :command meep-region-syntax-expand)
        '(:state visual :command meep-region-syntax-expand)
        '(:state visual :command meep-region-syntax-expand))
      (let ((point-at-max (meep-test-point-line-column))
            (mark-at-max (meep-test-mark-line-column))
            (content-at-max (meep-test-region-as-string)))
        ;; Try to expand further.
        (simulate-input-for-meep
          '(:state visual :command meep-region-syntax-expand))
        ;; Should remain unchanged.
        (should (equal 'visual (bray-state)))
        (should (equal point-at-max (meep-test-point-line-column)))
        (should (equal mark-at-max (meep-test-mark-line-column)))
        (should (equal content-at-max (meep-test-region-as-string)))))))

;; ---------------------------------------------------------------------------
;; Movement

(ert-deftest movement-word-next-basic ()
  "Move forward by word.

Verifies: word-next moves to start of next word."
  (let ((text-initial "hello world foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world foo
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to next word.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next))
      ;; Cursor: hello world foo
      ;;               ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?w (char-after)))
      ;; Move to next word.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next))
      ;; Cursor: hello world foo
      ;;                     ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 12) (meep-test-point-line-column)))
      (should (equal ?f (char-after))))))

(ert-deftest movement-word-prev-basic ()
  "Move backward by word.

Verifies: word-prev moves to start of previous word."
  (let ((text-initial "hello world foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end first.
      (goto-char (point-max))
      ;; Cursor: hello world foo
      ;;                        ^
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after))) ;; at end of buffer
      ;; Move to previous word.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-prev))
      ;; Cursor: hello world foo
      ;;                     ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 12) (meep-test-point-line-column)))
      (should (equal ?f (char-after)))
      ;; Move to previous word.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-prev))
      ;; Cursor: hello world foo
      ;;               ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?w (char-after))))))

(ert-deftest movement-symbol-next-basic ()
  "Move forward by symbol.

Verifies: symbol-next treats underscored identifiers as single unit."
  (let ((text-initial "foo_bar baz_qux end"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: foo_bar baz_qux end
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Move to next symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next))
      ;; Cursor: foo_bar baz_qux end
      ;;                 ^
      ;; Symbol movement skips entire foo_bar.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal ?b (char-after)))
      ;; Move to next symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next))
      ;; Cursor: foo_bar baz_qux end
      ;;                         ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 16) (meep-test-point-line-column)))
      (should (equal ?e (char-after))))))

(ert-deftest movement-symbol-vs-word ()
  "Compare symbol and word movement.

Verifies: word stops at underscore, symbol treats foo_bar as one unit."
  (let ((text-initial "foo_bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: foo_bar baz
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Word movement stops at underscore.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next))
      ;; Cursor: foo_bar baz
      ;;             ^
      ;; Word stops at _ (position 4 is the underscore).
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?b (char-after)))
      ;; Go back to start.
      (goto-char (point-min))
      ;; Symbol movement skips entire identifier.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next))
      ;; Cursor: foo_bar baz
      ;;                 ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal ?b (char-after))))))

(ert-deftest movement-symbol-prev-basic ()
  "Move backward by symbol.

Verifies: symbol-prev moves to start of previous symbol."
  (let ((text-initial "foo_bar baz_qux end"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: foo_bar baz_qux end
      ;;                            ^
      (should (equal '(1 . 19) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after))) ;; at end of buffer
      ;; Move to previous symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-prev))
      ;; Cursor: foo_bar baz_qux end
      ;;                         ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 16) (meep-test-point-line-column)))
      (should (equal ?e (char-after)))
      ;; Move to previous symbol again.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-prev))
      ;; Cursor: foo_bar baz_qux end
      ;;                 ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal ?b (char-after))))))

(ert-deftest movement-symbol-next-end ()
  "Move to end of current/next symbol.

Verifies: symbol-next-end lands after symbol's last character."
  (let ((text-initial "foo_bar baz_qux end"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: foo_bar baz_qux end
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Move to end of current symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next-end))
      ;; Cursor: foo_bar baz_qux end
      ;;                ^
      ;; Point is after 'r', before space.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 7) (meep-test-point-line-column)))
      (should (equal ?r (char-before)))
      (should (equal ?\s (char-after)))
      ;; Move to end of next symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next-end))
      ;; Cursor: foo_bar baz_qux end
      ;;                        ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal ?x (char-before)))
      (should (equal ?\s (char-after))))))

(ert-deftest movement-symbol-with-selection ()
  "Symbol movement extends selection in visual state.

Verifies: moving by symbol in visual state extends region."
  (let ((text-initial "foo_bar baz_qux end"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: foo_bar baz_qux end
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Start selection and move by symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      ;; Cursor: foo_bar baz_qux end
      ;;         ^------^
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal ?b (char-after)))
      (should (equal "foo_bar " (meep-test-region-as-string))))))

(ert-deftest movement-find-char-at-next ()
  "Find character on line (at position).

Verifies: find-at moves to just after the target character."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Find 'o' (at position).
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-at-next)
        "o")
      ;; Cursor: hello world
      ;;              ^
      ;; find-at lands after the 'o' in "hello".
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?o (char-before)))
      (should (equal ?\s (char-after)))
      ;; Find next 'o'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-at-next)
        "o")
      ;; Cursor: hello world
      ;;                  ^
      ;; find-at lands after the 'o' in "world".
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal ?o (char-before))))))

(ert-deftest movement-find-char-till-next ()
  "Find character on line (till position).

Verifies: find-till stops just before the target character."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Find 'o' (till position).
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-till-next)
        "o")
      ;; Cursor: hello world
      ;;             ^
      ;; find-till lands before the 'o' in "hello".
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?o (char-after)))
      (should (equal ?l (char-before)))
      ;; Find next 'o'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-till-next)
        "o")
      ;; Cursor: hello world
      ;;                 ^
      ;; find-till lands before the 'o' in "world".
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 7) (meep-test-point-line-column)))
      (should (equal ?o (char-after))))))

(ert-deftest movement-word-next-end ()
  "Move to end of current/next word.

Verifies: word-next-end lands after word's last character."
  (let ((text-initial "hello world foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world foo
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to end of current word (call directly).
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next-end))
      ;; Cursor: hello world foo
      ;;              ^
      ;; Point is after 'o', before space.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?o (char-before)))
      (should (equal ?\s (char-after)))
      ;; Move to end of next word.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next-end))
      ;; Cursor: hello world foo
      ;;                    ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal ?d (char-before)))
      (should (equal ?\s (char-after))))))

(ert-deftest movement-paragraph-next ()
  "Move to next paragraph boundary.

Verifies: paragraph-next moves to paragraph separator."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "First paragraph.\n"
          "\n"
          "Second paragraph.")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: First paragraph.
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?F (char-after)))
      ;; Move to paragraph boundary (call directly).
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-next))
      ;; Point at blank line (paragraph separator).
      (should (equal 'normal (bray-state)))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal ?\n (char-after)))
      ;; Move again to reach second paragraph.
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-next))
      ;; Point at end of second paragraph.
      (should (equal 'normal (bray-state)))
      (should (equal '(3 . 17) (meep-test-point-line-column)))
      (should (equal nil (char-after))))))

(ert-deftest movement-sentence-next ()
  "Move to next sentence boundary.

Verifies: sentence-next moves past sentence-ending punctuation."
  (let ((text-initial "First sentence.  Second sentence.  Third."))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: First sentence.  Second sentence.  Third.
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?F (char-after)))
      ;; Move to next sentence (call directly).
      (simulate-input-for-meep
        '(:state normal :command meep-move-sentence-next))
      ;; Point after first sentence's period, at first space.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal ?\s (char-after)))
      ;; Move again.
      (simulate-input-for-meep
        '(:state normal :command meep-move-sentence-next))
      ;; Point after second sentence.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 33) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

(ert-deftest movement-find-char-repeat ()
  "Repeat last find-char operation.

Verifies: repeat-at-next continues to next occurrence of same character."
  (let ((text-initial "a-b-c-d-e"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: a-b-c-d-e
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?a (char-after)))
      ;; Find first '-' (find-at lands AFTER the character).
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-at-next)
        "-")
      ;; Cursor: a-b-c-d-e
      ;;           ^
      ;; Position 2 is 'b', just after the first '-'.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal ?- (char-before)))
      (should (equal ?b (char-after)))
      ;; Repeat find.
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-repeat-at-next))
      ;; Cursor: a-b-c-d-e
      ;;             ^
      ;; Position 4 is 'c', just after the second '-'.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?- (char-before)))
      (should (equal ?c (char-after)))
      ;; Repeat again.
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-repeat-at-next))
      ;; Cursor: a-b-c-d-e
      ;;               ^
      ;; Position 6 is 'd', just after the third '-'.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?- (char-before)))
      (should (equal ?d (char-after))))))

(ert-deftest movement-find-char-not-found ()
  "Find character that doesn't exist on line.

Verifies: cursor stays in place, informative message displayed."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      ;; Search for 'z' which doesn't exist.
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-at-next)
        "z")
      ;; Cursor should not move.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Verify informative message was displayed.
      (should (equal '("char z not found") (meep-test-messages))))))

(ert-deftest movement-find-char-repeat-no-prior-char ()
  "Repeat-find with no stored character is a graceful no-op.

Regression: the repeat commands passed the helper's nil straight into
`meep--move-find-impl', where `(char-to-string nil)' signaled
`wrong-type-argument' instead of leaving the informative message."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; No prior find-char, so there is no character to repeat.
      (setq meep--move-find-last-char nil)
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Repeating with nothing stored must not signal.
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-repeat-at-next))
      ;; Cursor stays put, informative message displayed.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '("No last character is set") (meep-test-messages))))))

(ert-deftest movement-matching-bracket ()
  "Jump between matching brackets.

Verifies: moves to matching delimiter when inside parens."
  (let ((text-initial "(foo [bar] baz)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move inside the parens.
      (goto-char 2)
      ;; Cursor: (foo [bar] baz)
      ;;          ^
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Jump to matching bracket (end of outer paren).
      (simulate-input-for-meep
        '(:state normal :command meep-move-matching-bracket-inner))
      ;; Point at closing paren.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 14) (meep-test-point-line-column)))
      (should (equal ?\) (char-after)))
      ;; Jump back to opening paren.
      (simulate-input-for-meep
        '(:state normal :command meep-move-matching-bracket-inner))
      ;; Point back at start of content.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal ?f (char-after))))))

(ert-deftest movement-sexp-over-next ()
  "Move through s-expression.

Verifies: sexp movement navigates through expressions."
  (let ((text-initial "(foo) (bar) (baz)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: (foo) (bar) (baz)
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?\( (char-after)))
      ;; Move through sexp (descends into first).
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-over-next))
      ;; Cursor: (foo) (bar) (baz)
      ;;          ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal ?f (char-after)))
      ;; Continue through sexp.
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-over-next))
      ;; Cursor: (foo) (bar) (baz)
      ;;             ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?\) (char-after)))
      ;; One more to enter next sexp.
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-over-next))
      ;; Cursor: (foo) (bar) (baz)
      ;;                ^
      ;; Jumps to 'b' in "(bar)" at position 7.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 7) (meep-test-point-line-column)))
      (should (equal ?b (char-after))))))

;; ---------------------------------------------------------------------------
;; Join Line

(ert-deftest join-line-next-basic ()
  "Join current line with next line.

Verifies: lines are joined with single space."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "hello\n"
          "world\n"
          "foo")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: hello
      ;;               ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Join with next line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Result: "hello world\nfoo"
      (should (equal 'normal (bray-state)))
      (should (equal "hello world\nfoo" (buffer-string)))
      ;; Point moves to start of joined content (the 'w' in world).
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?w (char-after))))))

(ert-deftest join-line-prev-basic ()
  "Join current line with previous line.

Verifies: current line is appended to previous with single space."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "hello\n"
          "world\n"
          "foo")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to second line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      ;; Cursor line 2: world
      ;;               ^
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?w (char-after)))
      ;; Join with previous line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-prev))
      ;; Result: "hello world\nfoo"
      (should (equal 'normal (bray-state)))
      (should (equal "hello world\nfoo" (buffer-string)))
      ;; Point remains at same buffer position, now on line 1.
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?w (char-after))))))

(ert-deftest join-line-next-with-indentation ()
  "Join lines when next line has leading whitespace.

Verifies: leading whitespace is collapsed to single space."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "hello\n"
          "    world")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: hello
      ;;               ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Join with next line (which has indentation).
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Result: "hello world" (indentation removed).
      (should (equal 'normal (bray-state)))
      (should (equal "hello world" (buffer-string)))
      ;; Point at join position.
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?w (char-after))))))

(ert-deftest join-line-multiple ()
  "Join multiple lines with numeric prefix.

Verifies: numeric prefix joins that many lines."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "line1\n"
          "line2\n"
          "line3\n"
          "line4")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: line1
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?l (char-after)))
      ;; Join 3 lines (current + next 2) using C-u prefix.
      (simulate-input-for-meep
        "\C-u3"
        '(:state normal :command meep-join-line-next))
      ;; Result: "line1 line2 line3 line4".
      (should (equal 'normal (bray-state)))
      (should (equal "line1 line2 line3 line4" (buffer-string)))
      ;; Cursor at last join point.
      (should (equal '(1 . 18) (meep-test-point-line-column)))
      (should (equal ?l (char-after))))))

(ert-deftest join-line-next-at-last-line ()
  "Join line when already on last line.

Verifies: buffer unchanged, informative message displayed."
  (let ((text-initial "only line"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: only line
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      ;; Attempt join with no following line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Buffer unchanged.
      (should (equal 'normal (bray-state)))
      (should (equal text-initial (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Verify informative message was displayed.
      (should (equal '("Join line end: no following line found") (meep-test-messages))))))

(ert-deftest join-line-prev-at-first-line ()
  "Join line when already on first line.

Verifies: buffer unchanged, informative message displayed."
  (let ((text-initial "only line"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: only line
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      ;; Attempt join with no preceding line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-prev))
      ;; Buffer unchanged.
      (should (equal 'normal (bray-state)))
      (should (equal text-initial (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Verify informative message was displayed.
      (should (equal '("Join line beginning: no preceding line found") (meep-test-messages))))))

(ert-deftest join-line-next-python-comment-with-inline-comment ()
  "Join Python comment lines where the second line has an inline comment.

Verifies: only the comment prefix is stripped, not content before an inline comment."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "# A B\n"
          "# C D # E")))
    (with-meep-test text-initial
      (python-mode)
      (bray-mode 1)
      ;; Cursor line 1: # A B
      ;;               ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?# (char-after)))
      ;; Join with next line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Result: "# A B C D # E" (only the leading comment prefix stripped).
      (should (equal 'normal (bray-state)))
      (should (equal "# A B C D # E" (buffer-string)))
      ;; Point at join position.
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?C (char-after))))))

(ert-deftest join-line-next-c-mode-block-comment ()
  "Join a C-style block comment in plain `c-mode'.

Verifies: the leading `* ' continuation marker is stripped by reading
it directly from the buffer's own text - this doesn't depend on
`cc-mode's `c-block-comment-prefix' style variable (only used for
inserting, not recognizing, a prefix), nor on any tree-sitter specific
variable, so the same code path also covers `c-ts-mode' and friends."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "/* Example block.\n"
          " * next line. */")))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor line 1: /* Example block.
      ;;               ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      ;; Join with next line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Result: "/* Example block. next line. */" (the `* ' marker stripped).
      (should (equal 'normal (bray-state)))
      (should (equal "/* Example block. next line. */" (buffer-string)))
      ;; Point at join position.
      (should (equal '(1 . 18) (meep-test-point-line-column)))
      (should (equal ?n (char-after))))))

(ert-deftest join-line-next-c-mode-block-comment-preserves-content-after-marker ()
  "Join a C-style block comment whose second line starts with a `* '
marker immediately followed by content that itself looks marker-like
(a `-' bullet).

Verifies: only the `* ' leader is stripped - content after it (even
punctuation such as `-') is preserved, not swallowed along with it."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "/* Note:\n"
          " * - second point */")))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Join with next line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Result: only the `* ' leader is stripped, the `-' bullet remains.
      (should (equal "/* Note: - second point */" (buffer-string))))))

(ert-deftest join-line-next-c-mode-block-comment-no-marker ()
  "Join a C-style block comment whose second line has no marker at all -
just plain continuation prose (a style some authors use instead of a
per-line `* ').

Verifies: the first letter of the continuation line is not mistaken
for a one-character marker and swallowed."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "/* Example block.\n"
          "continued text with no marker at all. */")))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Join with next line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Result: the leading `c' of "continued" is preserved.
      (should
       (equal "/* Example block. continued text with no marker at all. */" (buffer-string))))))

(ert-deftest join-line-next-c-mode-block-comment-punctuation-start ()
  "Join a C-style block comment whose second line has no marker, but
happens to start with punctuation (a parenthesis) rather than a
letter.

Verifies: the opening `(' is not mistaken for a one-character marker
and swallowed - only characters from the curated marker set (`*',
`/', `#', `;', `-', etc.) are ever treated as a marker."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "/* Example:\n"
          "(x + y) computes something. */")))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Join with next line.
      (simulate-input-for-meep
        '(:state normal :command meep-join-line-next))
      ;; Result: the leading `(' is preserved.
      (should (equal "/* Example: (x + y) computes something. */" (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Delete

(ert-deftest delete-symbol-next-basic ()
  "Delete next symbol.

Verifies: entire symbol including underscores is deleted."
  (let ((text-initial "foo_bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: foo_bar baz
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Enter insert state and delete next symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-symbol-next))
      ;; Result: " baz" (foo_bar deleted).
      (should (equal 'insert (bray-state)))
      (should (equal " baz" (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

(ert-deftest delete-symbol-prev-basic ()
  "Delete previous symbol.

Verifies: entire symbol before point is deleted."
  (let ((text-initial "abc foo_bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move to after foo_bar.
      (goto-char 12) ; Position after space after foo_bar.
      ;; Cursor: abc foo_bar baz
      ;;                     ^
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?\s (char-after)))
      ;; Enter insert state and delete previous symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-symbol-prev))
      ;; Result: "abc  baz" (foo_bar deleted, but spaces remain).
      (should (equal 'insert (bray-state)))
      (should (equal "abc  baz" (buffer-string)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

(ert-deftest delete-symbol-next-at-end ()
  "Delete symbol when at end of text on line.

Verifies: last symbol deleted correctly without error."
  (let ((text-initial "foo bar"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: foo bar
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Move to last symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next))
      ;; Cursor: foo bar
      ;;             ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?b (char-after)))
      ;; Enter insert state and delete symbol at point.
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-symbol-next))
      (should (equal 'insert (bray-state)))
      ;; Last symbol deleted.
      (should (equal "foo " (buffer-string)))
      (should (equal '(1 . 4) (meep-test-point-line-column))))))

(ert-deftest delete-same-syntax-next-basic ()
  "Delete characters of same syntax class.

Verifies: only word characters deleted, stops at underscore."
  (let ((text-initial "foo_bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: foo_bar baz
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Enter insert state and delete same syntax (word chars only).
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-same-syntax-next))
      ;; Result: "_bar baz" (only "foo" deleted, stops at underscore).
      (should (equal 'insert (bray-state)))
      (should (equal "_bar baz" (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?_ (char-after))))))


;; ---------------------------------------------------------------------------
;; Insert Mode

(ert-deftest insert-open-below-basic ()
  "Open new line below and enter insert mode.

Verifies: new line created, cursor on new line, insert state."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "line1\n"
          "line2\n"
          "line3")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: line1
      ;;               ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?l (char-after)))
      ;; Open line below, type text, return to normal.
      (simulate-input-for-meep
        '(:state normal :command meep-insert-open-below)
        "new"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal "line1\nnew\nline2\nline3" (buffer-string)))
      ;; Point stays at end of inserted text.
      (should (equal '(2 . 3) (meep-test-point-line-column))))))

(ert-deftest insert-open-above-basic ()
  "Open new line above and enter insert mode.

Verifies: new line created above, cursor on new line, insert state."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "line1\n"
          "line2\n"
          "line3")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor line 1: line1
      ;;               ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?l (char-after)))
      ;; Move to second line, open line above, type text, return to normal.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-insert-open-above)
        "new"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal "line1\nnew\nline2\nline3" (buffer-string)))
      ;; Point stays at end of inserted text.
      (should (equal '(2 . 3) (meep-test-point-line-column))))))

(ert-deftest insert-line-beginning-basic ()
  "Insert at beginning of line (after indentation).

Verifies: cursor moves to first non-whitespace, enters insert state."
  (let ((text-initial "    hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor:     hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?\s (char-after)))
      ;; Move to middle of line (on the 'w' of 'world').
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next)
        '(:state normal :command meep-move-symbol-next))
      ;; Cursor:     hello world
      ;;                   ^
      (should (equal '(1 . 10) (meep-test-point-line-column)))
      (should (equal ?w (char-after)))
      ;; Insert at line beginning, type text, return to normal.
      (simulate-input-for-meep
        '(:state normal :command meep-insert-line-beginning)
        "X"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal "    Xhello world" (buffer-string)))
      ;; Point stays at end of inserted text.
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?h (char-after))))))

(ert-deftest insert-line-end-basic ()
  "Insert at end of line.

Verifies: cursor moves to end of line, enters insert state."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Insert at line end, type text, return to normal.
      (simulate-input-for-meep
        '(:state normal :command meep-insert-line-end)
        "!"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal "hello world!" (buffer-string)))
      ;; Point stays at end of inserted text (end of buffer).
      (should (equal '(1 . 12) (meep-test-point-line-column))))))

(ert-deftest insert-append-basic ()
  "Insert after current character (append).

Verifies: cursor moves forward one char, enters insert state."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Insert append (after current char), type text, return to normal.
      (simulate-input-for-meep
        '(:state normal :command meep-insert-append)
        "X"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal "hXello world" (buffer-string)))
      ;; Point stays at end of inserted text.
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      (should (equal ?e (char-after))))))

(ert-deftest insert-change-lines ()
  "Change entire line (clear and enter insert mode).

Verifies: line content is deleted and insert mode entered."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "line1\n"
          "line2\n"
          "line3")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to line2.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      ;; Cursor: line2
      ;;         ^
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?l (char-after)))
      ;; Change line.
      (simulate-input-for-meep
        '(:state normal :command meep-insert-change-lines))
      ;; Line is cleared, in insert mode.
      (should (equal 'insert (bray-state)))
      (should (equal "line1\n\nline3" (buffer-string)))
      ;; Point on empty line.
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal ?\n (char-after))))))


;; ---------------------------------------------------------------------------
;; Surround

(ert-deftest surround-insert-parens ()
  "Surround selection with parentheses.

Verifies: selected text wrapped with ()."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Select "hello".
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      ;; Cursor: hello world
      ;;         ^----^
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "hello " (meep-test-region-as-string)))
      ;; Surround with parens (`t x' add verb, then the delimiter).
      (simulate-input-for-meep
        "tx(")
      (should (equal 'normal (bray-state)))
      (should (equal "(hello )world" (buffer-string))))))

(ert-deftest surround-insert-quotes ()
  "Surround selection with double quotes.

Verifies: selected text wrapped with double quotes."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Select "hello".
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "hello " (meep-test-region-as-string)))
      ;; Surround with double quotes (`t x' add verb, then the delimiter).
      (simulate-input-for-meep
        "tx\"")
      (should (equal 'normal (bray-state)))
      (should (equal "\"hello \"world" (buffer-string))))))

(ert-deftest surround-insert-brackets ()
  "Surround selection with square brackets.

Verifies: selected text wrapped with []."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Select "hello".
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "hello " (meep-test-region-as-string)))
      ;; Surround with brackets (`t x' add verb, then the delimiter).
      (simulate-input-for-meep
        "tx[")
      (should (equal 'normal (bray-state)))
      (should (equal "[hello ]world" (buffer-string))))))

(ert-deftest surround-dispatch-add-event-direct ()
  "A direct delimiter binding wraps with that delimiter, like `t SPC ['.

Verifies: `t [' (`meep-surround-add-event') reads `[' from the invoking key and
surrounds the region, skipping the gate's separate delimiter read."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (should (equal "hello " (meep-test-region-as-string)))
      ;; `t [' wraps directly; no second key is read.
      (simulate-input-for-meep
        "t[")
      (should (equal 'normal (bray-state)))
      (should (equal "[hello ]world" (buffer-string))))))

(ert-deftest surround-close-delimiter-key-canonical ()
  "A close-bracket key wraps with the canonical open-first pair.

Verifies: `t ]' / `t )' / `t }' add `[...]' / `(...)' / `{...}', identical to
their open-bracket counterpart - the typed side must not invert the delimiters.
Replace (`t g ]') and type-directed delete (`t c ]') resolve the same pair."
  ;; Add: each close key wraps exactly as its opener does.
  (dolist (entry '(("]" . "[hello ]world") (")" . "(hello )world") ("}" . "{hello }world")))
    (with-meep-test "hello world"
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (should (equal "hello " (meep-test-region-as-string)))
      (simulate-input-for-meep
        (concat "t" (car entry)))
      (should (equal 'normal (bray-state)))
      (should (equal (cdr entry) (buffer-string)))))
  ;; Replace: a close key names the destination pair, open first.
  (with-meep-test "(hello)"
    (text-mode)
    (bray-mode 1)
    (simulate-input-for-meep
      '(:state normal :command meep-move-char-next))
    (simulate-input-for-meep
      "tg]")
    (should (equal "[hello]" (buffer-string))))
  ;; Delete by type: a close key names the pair type to strip.
  (with-meep-test "([hello])"
    (text-mode)
    (bray-mode 1)
    (simulate-input-for-meep
      '(:state normal :command meep-move-char-next)
      '(:state normal :command meep-move-char-next))
    (should (equal ?h (char-after)))
    (simulate-input-for-meep
      "tc]")
    (should (equal "(hello)" (buffer-string)))))

(ert-deftest surround-dispatch-add-lines-event-direct ()
  "A direct delimiter binding wraps the line's content, like `T SPC ['.

Verifies: `T [' (`meep-surround-add-lines-event') reads `[' from the invoking key
and wraps the current line's content, skipping the gate's separate read."
  (let ((text-initial "hello"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; `T [' wraps the line directly; no second key is read.
      (simulate-input-for-meep
        "T[")
      (should (equal 'normal (bray-state)))
      (should (equal "[hello]" (buffer-string))))))

(ert-deftest surround-dispatch-delete ()
  "The surround dispatch deletes the surrounding pair on a double tap.

Verifies: `meep-surround' then `t' strips the enclosing delimiters."
  (let ((text-initial "(hello)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside the parens.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next))
      (should (equal ?h (char-after)))
      ;; `t t' double-tap deletes the surrounding pair.
      (simulate-input-for-meep
        "tt")
      (should (equal 'normal (bray-state)))
      (should (equal "hello" (buffer-string))))))

(ert-deftest surround-delete-top-level-string-syntax-no-op ()
  "Surround-delete inside a top-level string is a no-op under the `syntax' backend.

A string with no enclosing bracket has no paren-syntax pair around point, and the
`syntax' backend drops the auto-detected quote fall-backs, so `t t' finds no pair
and leaves the buffer unchanged.  This is the deliberate, documented trade-off of
the `syntax' backend (see `meep-syntax-backend'); the `text' backend deletes the
quotes instead, see `surround-delete-top-level-string-text-deletes-quotes'."
  (let ((text-initial "\"foo bar\""))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move inside the string, onto `f'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next))
      (should (equal ?f (char-after)))
      ;; Default (nil -> syntax in a prog-mode buffer) backend.
      (let ((meep-syntax-backend nil))
        (simulate-input-for-meep
          "tt"))
      (should (equal 'normal (bray-state)))
      ;; The quotes are not deleted - the buffer is unchanged.
      (should (equal "\"foo bar\"" (buffer-string))))))

(ert-deftest surround-delete-top-level-string-text-deletes-quotes ()
  "Surround-delete inside a top-level string strips its quotes under the `text' backend.

The flip of `surround-delete-top-level-string-syntax-no-op' on the same input: the
`text' backend keeps the auto-detected quote pair, so `t t' deletes the
surrounding quotes."
  (let ((text-initial "\"foo bar\""))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next))
      (should (equal ?f (char-after)))
      (let ((meep-syntax-backend 'text))
        (simulate-input-for-meep
          "tt"))
      (should (equal 'normal (bray-state)))
      (should (equal "foo bar" (buffer-string))))))

(ert-deftest surround-dispatch-replace ()
  "The surround dispatch replaces the surrounding pair.

Verifies: `meep-surround' then `g' then a delimiter replaces the pair."
  (let ((text-initial "(hello)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next))
      ;; `t g [' replaces the surrounding pair to brackets (`g' is the replace key).
      (simulate-input-for-meep
        "tg[")
      (should (equal 'normal (bray-state)))
      (should (equal "[hello]" (buffer-string))))))

(ert-deftest surround-dispatch-delete-by-type ()
  "The surround dispatch deletes the surrounding pair of the read delimiter's type.

Verifies: `meep-surround-delete' then `(' strips the enclosing parens even though a
bracket pair is closer (the read delimiter is a type filter that skips it)."
  (let ((text-initial "([hello])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move inside both pairs (point on `h'); `[]' is the closer pair.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      (should (equal ?h (char-after)))
      ;; `t c (' deletes the parens, skipping the closer brackets.
      (simulate-input-for-meep
        "tc(")
      (should (equal 'normal (bray-state)))
      (should (equal "[hello]" (buffer-string))))))

(ert-deftest surround-dispatch-delete-lines-by-type ()
  "Line-wise type-directed delete strips each line's outermost pair of the read type.

Verifies: `meep-surround-delete-lines' (`T c') routes the `delete' action through
the line-wise path with a single-element pairs list, stripping only that type."
  (let ((text-initial "([x])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; `T c (' strips the line's outer parens line-wise, leaving the brackets.
      (simulate-input-for-meep
        "Tc(")
      (should (equal 'normal (bray-state)))
      (should (equal "[x]" (buffer-string))))))

(ert-deftest surround-dispatch-delete-by-type-repeat ()
  "Repeat a `t c (' type-directed delete with repeat-fu.

Verifies: replaying the `t c (' keys re-runs the gate's `delete' branch, stripping
the next enclosing pair of that type, ((x)) -> (x) -> x."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial "((x))"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Point on `x', inside both pairs.
      (goto-char 3)
      (simulate-input-for-meep
        "tc(")
      (should (equal "(x)" (buffer-string)))
      ;; Repeat with repeat-fu; called directly because it uses
      ;; `execute-kbd-macro' internally which cannot nest in the simulation.
      (repeat-fu-execute 1)
      (should (equal "x" (buffer-string))))))

(ert-deftest surround-dispatch-lines-add ()
  "The line-wise surround dispatch wraps the current line's content.

Verifies: `meep-surround-lines' then a delimiter wraps the line."
  (let ((text-initial "hello"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; `T x' is the line-wise add verb; `(' is the delimiter.
      (simulate-input-for-meep
        "Tx(")
      (should (equal 'normal (bray-state)))
      (should (equal "(hello)" (buffer-string))))))

(ert-deftest surround-dispatch-alias ()
  "An alias wraps with the pair resolved from `meep-surround-pairs'.

Verifies: the add gate then an alias key (`t x *') uses the mode's delimiters.
Aliases are reached through the gate, not bound at the top level."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (setq-local meep-surround-pairs '((bold . ("**" . "**"))))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (should (equal "hello " (meep-test-region-as-string)))
      (simulate-input-for-meep
        "tx*")
      (should (equal 'normal (bray-state)))
      (should (equal "**hello **world" (buffer-string))))))

(ert-deftest surround-dispatch-control-char-rejected ()
  "A control character such as `C-g' is not used as a delimiter.

Verifies: `meep-surround' then `C-g' cancels rather than wrapping with ^G."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (simulate-input-for-meep
        "tx"
        "\C-g")
      (should (equal "hello world" (buffer-string))))))

(ert-deftest surround-dispatch-form-feed-rejected ()
  "Form-feed (^L) is a control character and is not used as a delimiter.

Verifies: `meep-surround' then `C-l' cancels rather than wrapping with ^L."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (simulate-input-for-meep
        "tx"
        "\C-l")
      (should (equal "hello world" (buffer-string))))))

(ert-deftest surround-dispatch-line-feed-rejected ()
  "Line-feed (^J / `LFD') cancels rather than prompting like `RET'.
Only `RET' / `<return>' opens the literal-pair prompt; `C-j' is left out of that
guard so an unbound control key stays free for a later meaning.

Verifies: `meep-surround' then `C-j' cancels rather than reading a pair."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (simulate-input-for-meep
        "tx"
        "\C-j")
      (should (equal "hello world" (buffer-string))))))

(ert-deftest surround-add-literal-delimiters ()
  "Each literal delimiter key wraps with its own pair.
Covers the keys the dedicated `surround-insert-*' tests omit - `{' via the
symmetrical fall-back and `'' which pairs with itself, both resolved by
`meep--surround-pair-from-key'."
  (dolist (entry '(("{" . "{hello }world") ("'" . "'hello 'world")))
    (with-meep-test "hello world"
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (should (equal "hello " (meep-test-region-as-string)))
      (simulate-input-for-meep
        (concat "tx" (car entry)))
      (should (equal 'normal (bray-state)))
      (should (equal (cdr entry) (buffer-string))))))

(ert-deftest surround-add-alias-undefined-no-op ()
  "An alias whose symbol the mode does not define wraps nothing, ASCII or not.
A mnemonic letter alias is suppressed rather than taken literally, and the
suppression spans the whole `[:alnum:]' class, so a non-ASCII letter alias is
suppressed too - reporting the miss rather than wrapping with the letter itself."
  (dolist (key '("b" "é"))
    (with-meep-test "hello world"
      (text-mode)
      (bray-mode 1)
      (setq-local meep-surround-alist '((?b . bold) (?é . emphasis)))
      (setq-local meep-surround-pairs '((italic . ("*" . "*"))))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (simulate-input-for-meep
        (concat "tx" key))
      (should (equal "hello world" (buffer-string)))
      (should (member "No surround for that key" (meep-test-messages))))))

(ert-deftest surround-add-alias-undefined-punctuation-literal ()
  "A punctuation alias the mode leaves undefined falls back to its literal pair.
The backtick maps to the `code' alias; where `code' is absent it still wraps with a
literal backtick - unlike a mnemonic letter alias, which stays suppressed."
  (with-meep-test "hello world"
    (text-mode)
    (bray-mode 1)
    ;; The default alist maps the backtick to `code'; leave `code' undefined.
    (setq-local meep-surround-pairs '((italic . ("*" . "*"))))
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle)
      '(:state visual :command meep-move-symbol-next))
    (simulate-input-for-meep
      "tx`")
    (should (equal 'normal (bray-state)))
    (should (equal "`hello `world" (buffer-string)))))

(ert-deftest surround-add-function-spec ()
  "A function-valued pair spec is called to produce the wrapping delimiters."
  (with-meep-test "hello world"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-surround-alist '((?t . tag)))
    (setq-local meep-surround-pairs (list (cons 'tag (lambda () (cons "<x>" "</x>")))))
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle)
      '(:state visual :command meep-move-symbol-next))
    (simulate-input-for-meep
      "txt")
    (should (equal 'normal (bray-state)))
    (should (equal "<x>hello </x>world" (buffer-string)))))

(ert-deftest surround-add-non-printable-alias ()
  "A non-printable key bound as a configured alias resolves rather than cancelling.
`TAB' is rejected as a literal delimiter, but a `TAB' alias must still wrap through
`meep--surround-pair-from-event' - the keymap binds it as an alias key."
  (with-meep-test "hello world"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-surround-alist '((?\t . code)))
    (setq-local meep-surround-pairs '((code . ("`" . "`"))))
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle)
      '(:state visual :command meep-move-symbol-next))
    (simulate-input-for-meep
      "tx\t")
    (should (equal 'normal (bray-state)))
    (should (equal "`hello `world" (buffer-string)))))

(ert-deftest surround-add-literal-pair-prompt ()
  "`RET' at the delimiter read prompts for an arbitrary pair; an empty close cancels.
A typed open and close wrap the region, the close defaulting to the open's mirror;
clearing the prefilled close submits nothing, which is rejected rather than
wrapping one-sided, see `meep--surround-read-literal-pair'."
  ;; A typed pair wraps the region (accept the mirrored close with `RET').
  (with-meep-test "hello world"
    (text-mode)
    (bray-mode 1)
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle)
      '(:state visual :command meep-move-symbol-next))
    (simulate-input-for-meep
      "tx\r<\r\r")
    (should (equal 'normal (bray-state)))
    (should (equal "<hello >world" (buffer-string))))
  ;; Clearing the prefilled close (`DEL' then `RET') cancels, buffer left as-is.
  (with-meep-test "hello world"
    (text-mode)
    (bray-mode 1)
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle)
      '(:state visual :command meep-move-symbol-next))
    (simulate-input-for-meep
      "tx\r[\r\d\r")
    (should (equal "hello world" (buffer-string)))))

(ert-deftest surround-add-malformed-spec-no-op ()
  "A malformed or signaling pair spec wraps nothing rather than crashing.
A spec that is not a cons of two strings, a half-nil (literal or builder-returned)
or empty pair, or a builder that signals all resolve to no pair - a graceful miss,
see `meep--surround-pair-from-symbol'."
  (dolist (key '("m" "h" "e" "B" "l"))
    (with-meep-test "hello world"
      (text-mode)
      (bray-mode 1)
      (setq-local meep-surround-alist
                  '((?m . str) (?h . half) (?e . empty) (?B . boom) (?l . litbad)))
      (setq-local meep-surround-pairs
                  (list
                   (cons 'str (lambda () "oops"))
                   (cons 'half (lambda () (cons "x" nil)))
                   (cons 'empty (lambda () (cons "" "")))
                   (cons 'boom (lambda () (error "Builder failed")))
                   (cons 'litbad '("x" . nil))))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (simulate-input-for-meep
        (concat "tx" key))
      (should (equal "hello world" (buffer-string))))))

(ert-deftest surround-add-whitespace-alias ()
  "A whitespace (`SPC') alias key resolves to its pair rather than erroring.
The alias binds through its kbd spelling (\"SPC\"), not the raw space that
`keymap-set' rejects, see `meep--surround-each-alias'."
  (with-meep-test "hello world"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-surround-alist '((?\s . spaced)))
    (setq-local meep-surround-pairs '((spaced . ("<" . ">"))))
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle)
      '(:state visual :command meep-move-symbol-next))
    (simulate-input-for-meep
      "tx ")
    (should (equal 'normal (bray-state)))
    (should (equal "<hello >world" (buffer-string)))))

(ert-deftest surround-dispatch-delete-twice ()
  "Two `t t' deletes peel two nested pairs around point.

Verifies: ([{x}]) with point on `x' becomes (x) after two deletes."
  (let ((text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (should (equal ?x (char-after)))
      (simulate-input-for-meep
        "tt"
        "tt")
      (should (equal "(x)" (buffer-string)))
      (should (equal ?x (char-after))))))

(ert-deftest surround-dispatch-delete-repeat ()
  "Repeat a `t t' delete with repeat-fu (`q').

Verifies: after deleting one pair, repeat-fu reproduces the delete on the
next enclosing pair, ([{x}]) -> ([x]) -> (x)."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "tt")
      (should (equal "([x])" (buffer-string)))
      ;; Repeat with repeat-fu; called directly because it uses
      ;; `execute-kbd-macro' internally which cannot nest in the simulation.
      (repeat-fu-execute 1)
      (should (equal "(x)" (buffer-string))))))

(ert-deftest surround-dispatch-add-repeat ()
  "Repeat a `t x' add with repeat-fu (`q').

Verifies: a `t x' delimiter add replays under repeat-fu - the named event command
(`meep-surround-add-event') the run-time delimiter keymap routes to is recorded
and replayed by its key sequence."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial "ab"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 1)
      (simulate-input-for-meep
        "tx(")
      (should (equal "()ab" (buffer-string)))
      ;; Repeat with repeat-fu; called directly because it uses
      ;; `execute-kbd-macro' internally which cannot nest in the simulation.
      (repeat-fu-execute 1)
      (should (equal "(())ab" (buffer-string))))))

;; Delete and change find the pair enclosing their anchor.  The anchor is point
;; (the common case - the pair is removed from inside it) or, when set, an active
;; selection (which lets the search reach a pair *outside* the selection).  An
;; implied region a prior motion left behind is deliberately ignored, else a move
;; that crossed a delimiter on its way in would pull the target outward.  Add, by
;; contrast, wraps the implied region (see `surround-dispatch-add-parens').  The
;; motion runs in its own `simulate-input-for-meep', leaving the mark set, so
;; these pin that the verbs ignore it rather than relying on `last-command'.

(ert-deftest surround-dispatch-delete-ignores-implied-region ()
  "Delete acts around point, not the implied region from a prior motion.

Verifies: after a motion whose span crosses the inner `(', `t' still deletes the
innermost pair around point, not the outer pair enclosing the motion span."
  (let ((text-initial "[(foo)]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Point on the inner `('; the motion spans into `foo', leaving a mark that
      ;; delete must not treat as a selection.
      (goto-char 2)
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next))
      (simulate-input-for-meep
        "tt")
      (should (equal "[foo]" (buffer-string))))))

(ert-deftest surround-dispatch-replace-ignores-implied-region ()
  "Change acts around point, not the implied region from a prior motion.

Verifies: after a motion, `g' then `{' replaces the innermost pair around point."
  (let ((text-initial "[(foo)]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2)
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next))
      (simulate-input-for-meep
        "tg{")
      (should (equal "[{foo}]" (buffer-string))))))

(ert-deftest surround-dispatch-delete-lines-ignores-implied-region ()
  "Line-wise delete acts on the current line, not the implied region's span.

Verifies: after a vertical motion, `T' strips only the line point lands on, not
every line the motion spanned."
  (let ((text-initial "(aa)\n(bb)\n(cc)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Start in line 2 and move up to line 1; the motion spans both, but
      ;; line-wise delete acts only on line 1 (where point lands).
      (goto-char 7)
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev))
      ;; `T T' (the line-wise activation key) deletes via a double-tap.
      (simulate-input-for-meep
        "TT")
      (should (equal "aa\n(bb)\n(cc)" (buffer-string))))))

(ert-deftest surround-dispatch-delete-active-region-finds-outer ()
  "An active selection lets delete reach the pair enclosing the region.

Verifies: a selection running from the inner `(' out past its close cannot be
enclosed by the inner `()', so the outer `[]' is the only enclosing pair and `t'
deletes that - the point-based default would take the inner `()' (see
`surround-dispatch-delete-ignores-implied-region').  This pins the
region-vs-point contrast only; the fixture returns the outer pair with or without
the cursor-on-open behaviour, which is covered by
`surround-replace-cursor-on-open' and
`selection-mark-bounds-of-char-on-delimiter'."
  (let ((text-initial "[(a)b]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select `(a)b': it includes the inner close, so the inner `()' no longer
      ;; encloses the whole selection.
      (goto-char 2)
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char 6)
      (simulate-input-for-meep
        "tt")
      (should (equal "(a)b" (buffer-string))))))

;; By-type surround (`s t' / `s T') is verb-first: the verb comes first
;; (`g' replaces via `meep-surround-replace-by-type', `t' / `DEL' delete via
;; `meep-surround-delete'), then a source delimiter names which pair type to act on
;; - the nearest pair of *that* type, skipping closer pairs of other types, unlike
;; `meep-surround' which takes the closest of any type.

(ert-deftest surround-by-type-delete-targets-named-pair ()
  "Picking a delimiter deletes that pair, skipping nearer pairs of other types.

Verifies: with point inside `([{x}])', `s t t (' removes the `()', even though
`{}' and `[]' enclose point more tightly; `s t t {' removes the middle `{}'.  The
`t' verb deletes, the following delimiter names which type."
  (let ((text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "stt(")
      (should (equal "[{x}]" (buffer-string)))))
  ;; A different source picks a different pair (here the middle `{}').
  (let ((text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "stt{")
      (should (equal "([x])" (buffer-string))))))

(ert-deftest surround-by-type-replace-targets-named-pair ()
  "Picking a delimiter changes that pair, skipping nearer pairs of other types.

Verifies: with point inside `([{x}])', `s t g ( [' changes the `()' to `[]' - the
`g' verb replaces, `(' names the source type, `[' the destination."
  (let ((text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "stg([")
      (should (equal "[[{x}]]" (buffer-string))))))

(ert-deftest surround-region-activate-at-point-inner ()
  "Region-activate selects the inner content of the surrounding pair.

Verifies: `t d' finds the enclosing pair contextually and activates the region over
the text between the delimiters, without editing the buffer."
  (let ((text-initial "(hello)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2) ; Inside the parens, on `h'.
      (simulate-input-for-meep
        "td")
      (should (equal 'visual (bray-state)))
      (should (equal "hello" (meep-test-region-as-string)))
      ;; Never modifies the buffer.
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-region-activate-by-type ()
  "By-type region-activate selects the named pair, skipping nearer pairs.

Verifies: `s t d (' activates the region over the parens' content even though `[]'
encloses point more tightly - the read delimiter is a type filter, as for by-type
delete."
  (let ((text-initial "([hello])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 3) ; On `h', inside both pairs.
      (simulate-input-for-meep
        "std(")
      (should (equal 'visual (bray-state)))
      (should (equal "[hello]" (meep-test-region-as-string)))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-region-activate-count-nth ()
  "A count selects the Nth enclosing pair's content, not the innermost.

Verifies: `C-u 2 t d' in `((x))' activates the outer parens' content, leaving the
inner pair for count 1."
  (let ((text-initial "((x))"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 3) ; On `x'.
      (simulate-input-for-meep
        [?\C-u ?2]
        "td")
      (should (equal 'visual (bray-state)))
      (should (equal "(x)" (meep-test-region-as-string)))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-region-activate-no-pair ()
  "Region-activate with no surrounding pair activates nothing and leaves the buffer.

Verifies: with nothing enclosing point, `t d' reports the miss without entering
visual state."
  (let ((text-initial "foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2)
      (simulate-input-for-meep
        "td")
      (should (equal 'normal (bray-state)))
      (should-not (region-active-p))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-region-activate-works-read-only ()
  "Region-activate selects in a read-only buffer, since it never edits.

Verifies: unlike the delete / replace verbs, the command has no `*' interactive
guard, so it still activates the region when the buffer is read-only."
  (let ((text-initial "(hello)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2)
      (let ((buffer-read-only t))
        (simulate-input-for-meep
          "td"))
      (should (equal 'visual (bray-state)))
      (should (equal "hello" (meep-test-region-as-string)))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-by-type-count-selects-nth ()
  "A numeric prefix selects the Nth enclosing pair of the named type.

Verifies: in `([(x)])', `C-u 2 s t t (' deletes the 2nd enclosing `()' (the
outer), skipping the closer `[]' of another type.  The fixture distinguishes the
count - count 1 would give `([x])', count 2 gives `[(x)]' - so a regression to the
innermost pair is caught (the old `((x))' fixture yielded `(x)' either way)."
  (let ((text-initial "([(x)])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        [?\C-u ?2]
        "stt(")
      (should (equal "[(x)]" (buffer-string))))))

(ert-deftest surround-by-type-pick-does-not-leak ()
  "The pick is one-shot: a later plain `meep-surround' still takes the closest.

Verifies: after a targeted delete, `meep-surround' then `t' removes the innermost
pair of any type, proving the picked pair did not persist."
  (let ((text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      ;; Target the outer `()'.
      (simulate-input-for-meep
        "stt(")
      (should (equal "[{x}]" (buffer-string)))
      ;; A plain surround delete now takes the closest pair (the `{}'), not a
      ;; lingering `()' pick.
      (goto-char 3)
      (simulate-input-for-meep
        "tt")
      (should (equal "[x]" (buffer-string))))))

(ert-deftest surround-by-type-delete-repeat ()
  "Repeat a by-type delete with repeat-fu.
The delete routes each source key to `meep-surround-delete-event', so this pins that
repeat-fu records and replays it: the replayed keys re-run the dispatch.

The first delete only primes repeat-fu's command history.  repeat-fu extracts the
replay macro from its recent-command ring; interactively the replayed `q' key adds
its own entry, but a test must call `repeat-fu-execute' directly (it runs a kbd
macro that cannot nest in the simulation), so it leaves only the commands the test
itself ran - prime the ring with a prior delete rather than replay from a lone one."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial "[x][y][z]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Prime the command ring.
      (goto-char 2)
      (simulate-input-for-meep
        "stt[")
      (should (equal "x[y][z]" (buffer-string)))
      ;; The by-type delete the repeat replays (`t' deletes, `[' names the type).
      (goto-char 3)
      (simulate-input-for-meep
        "stt[")
      (should (equal "xy[z]" (buffer-string)))
      ;; Repeat on the third group; the replayed `s t t [' re-runs the dispatch.
      (goto-char 4)
      (repeat-fu-execute 1)
      (should (equal "xyz" (buffer-string))))))

(ert-deftest surround-by-type-delete-literal-repeat ()
  "Repeat a by-type delete of a typed-literal source with repeat-fu.
A literal source (not an alias or `meep-symmetrical-chars' entry) reaches
`meep-surround-delete-event' through the delete reader's `[t]' catch-all, so this
pins that its key sequence replays.  The first delete primes repeat-fu's command
history, see `surround-by-type-delete-repeat'."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial "\"x\" \"y\" \"z\""))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Prime the command ring.  `"' is a literal source: `s t t' enters delete,
      ;; `"' picks via the catch-all.
      (goto-char 2)
      (simulate-input-for-meep
        "stt\"")
      (should (equal "x \"y\" \"z\"" (buffer-string)))
      ;; The by-type delete the repeat replays.
      (goto-char 4)
      (simulate-input-for-meep
        "stt\"")
      (should (equal "x y \"z\"" (buffer-string)))
      ;; Repeat on the third group; the replayed `s t t "' re-runs the dispatch.
      (goto-char 6)
      (repeat-fu-execute 1)
      (should (equal "x y z" (buffer-string))))))

(ert-deftest surround-by-type-replace-repeat ()
  "Repeat a by-type replace with repeat-fu.
The keymap-traversed replace routes each source key to a destination map ending in
`meep-surround-replace-by-type-event', a named command (it recovers the source from
the key sequence rather than a closure), so this pins that repeat-fu records and
replays it: the replayed `s t g ( [' re-runs the dispatch.

The first replace only primes repeat-fu's command history, see
`surround-by-type-delete-repeat'."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial "(a)(b)(c)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Prime the command ring.
      (goto-char 2)
      (simulate-input-for-meep
        "stg([")
      (should (equal "[a](b)(c)" (buffer-string)))
      ;; The by-type replace the repeat replays (`g' replaces, `(' names the source
      ;; type, `[' the destination).
      (goto-char 5)
      (simulate-input-for-meep
        "stg([")
      (should (equal "[a][b](c)" (buffer-string)))
      ;; Repeat on the third group; the replayed `s t g ( [' re-runs the dispatch.
      (goto-char 8)
      (repeat-fu-execute 1)
      (should (equal "[a][b][c]" (buffer-string))))))

(ert-deftest surround-by-type-replace-prompting-source ()
  "Replace a prompting source pair without losing the destination key.
A function-valued source spec prompts via the minibuffer when it resolves, which
overwrites `last-command-event'; the destination must be the key pressed after the
source, not the prompt's terminating `RET', see
`meep--surround-replace-by-type-from-events'."
  (let ((meep-surround-alist '((?z . ztag)))
        ;; A prompting source: resolving it runs a minibuffer read that clobbers
        ;; `last-command-event', as a tag-name alias would.
        (meep-surround-pairs
         (list
          (cons
           'ztag
           (lambda ()
             (read-string "tag: ")
             (cons "<z>" "</z>")))))
        (text-initial "<z>x</z>"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      ;; `s t g' enters replace, `z' picks the source, `[' replaces with `[]'; the
      ;; trailing `RET' answers the source prompt that fires as the replace commits.
      (simulate-input-for-meep
        "stgz[\r")
      (should (equal "[x]" (buffer-string))))))

(ert-deftest surround-by-type-replace-literal-source ()
  "Replace a typed-literal source pair, reached through the transient pick path.
A literal source (not an alias or `meep-symmetrical-chars' entry) reaches the
destination step via the `[t]' catch-all and `meep-surround-replace-by-type-pick' -
the transient fall-back - so this pins that its replace path works, a different
chain than the keymap-traversed replace."
  (let ((text-initial "\"x\" \"y\""))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2)
      ;; `"' is a literal source: `s t g' enters replace, `"' picks via the
      ;; catch-all, `(' replaces with `()'.
      (simulate-input-for-meep
        "stg\"(")
      (should (equal "(x) \"y\"" (buffer-string))))))

(ert-deftest surround-by-type-replace-pick-repeat ()
  "Repeat a by-type replace whose source is a typed literal, via repeat-fu.
A literal source reaches the destination step through
`meep-surround-replace-by-type-pick' and the named
`meep-surround-replace-by-type-picked-event' (it reads the stashed source, not a
closure), so this pins that the transient pick path replays too.  The first replace
primes repeat-fu's command history, see `surround-by-type-delete-repeat'."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial "\"x\" \"y\" \"z\""))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Prime the command ring.
      (goto-char 2)
      (simulate-input-for-meep
        "stg\"[")
      (should (equal "[x] \"y\" \"z\"" (buffer-string)))
      ;; The pick-path replace the repeat replays.
      (goto-char 6)
      (simulate-input-for-meep
        "stg\"[")
      (should (equal "[x] [y] \"z\"" (buffer-string)))
      ;; Repeat on the third group.
      (goto-char 10)
      (repeat-fu-execute 1)
      (should (equal "[x] [y] [z]" (buffer-string))))))

(ert-deftest surround-by-type-command-installs-transient-map ()
  "Drive the by-type verbs as commands, reaching their transient source map.
The verb-first by-type verbs (`meep-surround-delete', `meep-surround-replace-by-type')
install their source map - and, for replace, its nested destination map - under
`set-transient-map'.  Invoking them directly as commands, rather than through the
`s t' prefix keys the other by-type tests use, pins that the transient chain reaches
both delete and replace."
  ;; Delete via the command path: `(' picks `()' and strips it.
  (let ((text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        '(:state normal :command meep-surround-delete)
        "(")
      (should (equal "[{x}]" (buffer-string)))))
  ;; Replace via the command path: `( [' changes the `()' to `[]'.
  (let ((text-initial "([{x}])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        '(:state normal :command meep-surround-replace-by-type)
        "([")
      (should (equal "[[{x}]]" (buffer-string))))))

;; The `s T' line-wise siblings of `s t': the verb step is the same, with each
;; verb operating on the line's pair of the named type rather than at point.

(ert-deftest surround-by-type-delete-lines ()
  "Line-wise by-type delete strips the line's pair of the named type.

Verifies: `s T t (' deletes the line's `()', skipping the closer `[]', line-wise."
  (let ((text-initial "([x])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        "sTt(")
      (should (equal 'normal (bray-state)))
      (should (equal "[x]" (buffer-string))))))

(ert-deftest surround-by-type-replace-lines ()
  "Line-wise by-type replace changes the line's pair of the named type.

Verifies: `s T g ( [' replaces the line's `()' with `[]', skipping the closer `[]',
line-wise."
  (let ((text-initial "([x])"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        "sTg([")
      (should (equal 'normal (bray-state)))
      (should (equal "[[x]]" (buffer-string))))))

;; Regressions pinned from review of the surround rewrite: same-delimiter finder
;; mispairs (tokens skipped or split), dispatch edge cases (`M-x' key recovery,
;; GUI symbol events, dropped counts) and configuration invalidation.

(ert-deftest surround-delete-region-same-delim-mispair-no-op ()
  "A selection no same-delimiter pair encloses deletes nothing.

Verifies: selecting `*a* ' in `*a* *b*' and deleting reports the miss; the
close search must count the token inside the region rather than skip to the
region end and pair the first pair's open with the *second* pair's open,
corrupting both."
  (let ((text-initial "*a* *b*"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (setq-local meep-surround-pairs '((italic . ("*" . "*"))))
      ;; Select `*a* ' - from the first `*' through the following space.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char 5)
      (simulate-input-for-meep
        "tt")
      (should (equal text-initial (buffer-string)))
      (should (member "No surrounding pair found" (meep-test-messages))))))

(ert-deftest surround-delete-point-inside-doubled-token ()
  "Point between the two characters of a `**' token deletes the `**' pair.

An ordinary cursor position, on the opening or the closing token.  Verifies:
the shorter `*' must not pair the token's own two halves and strip half the
markup - the straddled `**' token is still found and supersedes."
  (dolist (pos '(4 10))
    (with-meep-test "a **bold** b"
      (text-mode)
      (bray-mode 1)
      (setq-local meep-surround-pairs
                  (list (cons 'bold (cons "**" "**")) (cons 'italic (cons "*" "*"))))
      (goto-char pos)
      (simulate-input-for-meep
        "tt")
      (should (equal "a bold b" (buffer-string))))))

(ert-deftest surround-delete-point-inside-multi-char-open-bracket ()
  "Point inside a multi-character opening bracket deletes its pair.

An HTML-style tag pair: on the `b' or `>' of `<b>' the backward scan cannot see
the token point sits inside, so the finder probes for an open token covering
point (`meep--bracket-open-token-start'); on `<' the plain cursor-on-open case
must keep working.  The distinct-bracket sibling of
`surround-delete-point-inside-doubled-token'."
  (dolist (pos '(1 2 3))
    (with-meep-test "<b>bold</b> x"
      (text-mode)
      (bray-mode 1)
      (setq-local meep-surround-pairs '((bold . ("<b>" . "</b>"))))
      (goto-char pos)
      (simulate-input-for-meep
        "tt")
      (should (equal "bold x" (buffer-string))))))

(ert-deftest surround-delete-configured-prefix-of-fallback ()
  "A configured delimiter that prefixes a longer fall-back does not shadow it.

markdown-style: `code' is a single backtick while the contextual fall-backs
hold the doubled token.  Verifies: recognition searches the longer token first
(`meep--surround-recognition-pairs' orders longest-first), else the doubled
token's halves pair and its close is orphaned."
  (with-meep-test "a ``code`` b"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-match-bounds-of-char-contextual '(("``" . "``") ("`" . "`")))
    (setq-local meep-surround-pairs '((code . ("`" . "`"))))
    (goto-char 3) ; On the first backtick of the opening token.
    (simulate-input-for-meep
      "tt")
    (should (equal "a code b" (buffer-string)))))

(ert-deftest surround-delete-region-cursor-on-open-falls-outward ()
  "A selection from an open bracket its own pair cannot contain deletes the outer pair.

Verifies: with `(foo) b' selected in `((foo) bar)', the open at the region start
closes before the region end, so the text-backend finder falls back past it to
the enclosing pair rather than reporting no pair."
  (with-meep-test "((foo) bar)"
    (text-mode)
    (bray-mode 1)
    (goto-char 2) ; On the inner `('.
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle))
    (goto-char 9)
    (simulate-input-for-meep
      "tt")
    (should (equal "(foo) bar" (buffer-string)))))

(ert-deftest surround-by-type-replace-event-via-m-x ()
  "An `M-x' call of the by-type replace event resolves no source delimiter.

`execute-extended-command' records `M-x', the command name and `RET' as the
command keys, so the name's last character sits in the source slot; it was
never typed as a delimiter and must not resolve as one (it deleted the two
literal `t' characters).  Verifies: the buffer is untouched and the miss is
reported."
  (let ((text-initial "say t hello t end"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 8) ; Between the two `t' tokens.
      (simulate-input-for-meep
        [?\M-x]
        "meep-surround-replace-by-type-event\r")
      (should (equal text-initial (buffer-string)))
      (should (member "No surround for that key" (meep-test-messages))))))

(ert-deftest surround-add-syntax-table-paired-bracket ()
  "A bracket paired only by the buffer's syntax table wraps as its proper pair.

Guillemets are not in `meep-symmetrical-chars'; either side must resolve the
`«...»' pair via the syntax table (`matching-paren') rather than doubling the
typed character."
  (dolist (key '("«" "»"))
    (with-meep-test "hello world"
      (text-mode)
      (bray-mode 1)
      ;; Copy the mode's table first: `modify-syntax-entry' edits the current
      ;; table in place, which is shared by every `text-mode' buffer.
      (set-syntax-table (copy-syntax-table (syntax-table)))
      (modify-syntax-entry ?« "(»")
      (modify-syntax-entry ?» ")«")
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      (should (equal "hello " (meep-test-region-as-string)))
      (simulate-input-for-meep
        (concat "tx" key))
      (should (equal "«hello »world" (buffer-string))))))

(ert-deftest surround-add-empty-span-mark-rides-inside ()
  "Wrapping the empty span at point carries a mark set at point inside too.

Verifies: the implied region stays an empty span rather than covering the open
delimiter, which a following implied-region verb (a cut, say) would consume,
unbalancing the pair."
  (with-meep-test "hello"
    (text-mode)
    (bray-mode 1)
    (goto-char 3)
    (set-mark 3)
    (deactivate-mark)
    (simulate-input-for-meep
      "t(")
    (should (equal "he()llo" (buffer-string)))
    (should (eq 4 (point)))
    (should (eq 4 (mark t)))))

(ert-deftest surround-delete-recognizes-in-place-config-change ()
  "An in-place `meep-surround-pairs' edit is picked up by the next delete.

The recognition set is memoized; a `setcdr' of an existing entry must
invalidate it rather than compare the mutated list against itself, see
`meep--surround-recognition-pairs'."
  (with-meep-test "**bold** and __alt__"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-surround-pairs (list (cons 'bold (cons "**" "**"))))
    ;; Prime the memo with a delete that recognizes `**'.
    (goto-char 3)
    (simulate-input-for-meep
      "tt")
    (should (equal "bold and __alt__" (buffer-string)))
    ;; Mutate the existing entry in place; the next delete must see `__'.
    (setcdr (assq 'bold meep-surround-pairs) (cons "__" "__"))
    (goto-char 12) ; Inside `__alt__'.
    (simulate-input-for-meep
      "tt")
    (should (equal "bold and alt" (buffer-string)))))

(ert-deftest surround-add-gui-tab-symbol-alias ()
  "The GUI `tab' symbol event resolves a configured `TAB' alias.

A terminal delivers `TAB' as its ASCII character while a GUI delivers the
symbol event, and the delimiter map's catch-all suppresses the usual
`function-key-map' translation, so the dispatch folds the symbol itself - the
same gesture must wrap on both display types, see
`meep--surround-pair-from-event'."
  (with-meep-test "hello world"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-surround-alist '((?\t . code)))
    (setq-local meep-surround-pairs '((code . ("`" . "`"))))
    (simulate-input-for-meep
      '(:state normal :command meep-region-toggle)
      '(:state visual :command meep-move-symbol-next))
    (simulate-input-for-meep
      "tx"
      [tab])
    (should (equal 'normal (bray-state)))
    (should (equal "`hello `world" (buffer-string)))))

(ert-deftest surround-add-count-through-transient-gate ()
  "A numeric prefix survives the gate verbs' transient delimiter map.

Verifies: `C-u 3 t x (' wraps three times.  The transient map still fires
without the preserved prefix, so a dropped count would pass silently, see
`meep--surround-set-keymap'."
  (with-meep-test "foo"
    (text-mode)
    (bray-mode 1)
    (set-mark 1)
    (goto-char 4)
    (simulate-input-for-meep
      [?\C-u ?3]
      "tx(")
    (should (equal "(((foo)))" (buffer-string)))))


;; ---------------------------------------------------------------------------
;; Space Operations

(ert-deftest space-shrink-multiple-to-one ()
  "Shrink multiple spaces to single space.

Verifies: consecutive spaces reduced to one."
  (let ((text-initial "hello    world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello    world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to the spaces.
      (goto-char 6)
      ;; Cursor: hello    world
      ;;              ^
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?\s (char-after)))
      ;; Shrink spaces.
      (simulate-input-for-meep
        '(:state normal :command meep-space-shrink-contextual))
      (should (equal 'normal (bray-state)))
      (should (equal "hello world" (buffer-string)))
      ;; Point moves to after the single space.
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?w (char-after))))))

(ert-deftest space-shrink-trailing ()
  "Shrink trailing whitespace at end of line.

Verifies: multiple trailing spaces shrunk to one."
  (let ((text-initial "hello   "))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to trailing spaces.
      (goto-char 6)
      ;; Cursor: hello
      ;;              ^
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?\s (char-after)))
      (should (equal 'normal (bray-state)))
      ;; Shrink spaces.
      (simulate-input-for-meep
        '(:state normal :command meep-space-shrink-contextual))
      (should (equal 'normal (bray-state)))
      ;; Multiple trailing spaces shrunk to one.
      (should (equal "hello " (buffer-string)))
      ;; Point moves after the single space (at end of buffer).
      (should (equal '(1 . 6) (meep-test-point-line-column))))))


(ert-deftest space-shrink-blank-lines ()
  "Remove multiple blank lines when cursor is on a blank line.

Verifies: multiple blank lines between paragraphs collapse to one."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "aaa\n"
          "\n"
          "\n"
          "bbb")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to the first blank line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      ;; Shrink blank lines.
      (simulate-input-for-meep
        '(:state normal :command meep-space-shrink-contextual))
      (should (equal "aaa\n\nbbb" (buffer-string)))
      (should (equal '(2 . 0) (meep-test-point-line-column))))))

(ert-deftest space-shrink-many-blank-lines ()
  "Remove many blank lines when cursor is on a blank line.

Verifies: five blank lines collapse to one."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "aaa\n"
          "\n"
          "\n"
          "\n"
          "\n"
          "\n"
          "bbb")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to a blank line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      ;; Shrink blank lines.
      (simulate-input-for-meep
        '(:state normal :command meep-space-shrink-contextual))
      (should (equal "aaa\n\nbbb" (buffer-string))))))

(ert-deftest space-shrink-single-blank-line ()
  "A single blank line between paragraphs is not removed.

Verifies: calling on a single empty line makes no change."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "aaa\n"
          "\n"
          "bbb")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to the blank line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      ;; Shrink: no change expected.
      (simulate-input-for-meep
        '(:state normal :command meep-space-shrink-contextual))
      (should (equal "aaa\n\nbbb" (buffer-string)))
      (should (equal '(2 . 0) (meep-test-point-line-column))))))

(ert-deftest space-shrink-paragraph-boundary ()
  "Trim blank lines at paragraph boundaries from non-blank content.

Verifies: calling on a non-blank line trims blank line runs
above and below to a single blank line each."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "\n"
          "\n"
          "\n"
          "aaa\n"
          "\n"
          "\n"
          "\n"
          "bbb")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to 'aaa' (line 4).
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-line-next))
      (should (equal ?a (char-after)))
      ;; Shrink: trims blank lines above and below to one each.
      (simulate-input-for-meep
        '(:state normal :command meep-space-shrink-contextual))
      (should (equal "\naaa\n\nbbb" (buffer-string))))))

(ert-deftest space-shrink-tab-to-space ()
  "Shrink a single tab to a single space.

Verifies: a tab character is normalized to a space."
  (let ((text-initial "hello\tworld"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to the tab.
      (goto-char 6)
      (should (equal ?\t (char-after)))
      ;; Shrink.
      (simulate-input-for-meep
        '(:state normal :command meep-space-shrink-contextual))
      (should (equal "hello world" (buffer-string)))
      (should (equal ?w (char-after))))))

;; ---------------------------------------------------------------------------
;; Search Operations

(ert-deftest isearch-at-point-next-basic ()
  "Search forward for word at point.

Verifies: jumps to next occurrence of word under cursor, selects match."
  (let ((text-initial "hello world hello again"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world hello again
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Search forward for "hello".
      (simulate-input-for-meep
        '(:state normal :command meep-isearch-at-point-next))
      ;; Should find second "hello", with match selected (visual state).
      (should (equal 'visual (bray-state)))
      ;; Point is after match.
      (should (equal '(1 . 17) (meep-test-point-line-column)))
      (should (equal ?o (char-before)))
      (should (equal ?\s (char-after)))
      ;; Mark is at start of match.
      (should (equal '(1 . 12) (meep-test-mark-line-column)))
      (should (equal "hello" (meep-test-region-as-string))))))

(ert-deftest isearch-at-point-prev-basic ()
  "Search backward for word at point.

Verifies: jumps to previous occurrence of word under cursor, selects match."
  (let ((text-initial "hello world hello again"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world hello again
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to second "hello".
      (goto-char 13)
      ;; Cursor: hello world hello again
      ;;                      ^
      (should (equal '(1 . 12) (meep-test-point-line-column)))
      (should (equal ?h (char-after)))
      ;; Search backward for "hello".
      (simulate-input-for-meep
        '(:state normal :command meep-isearch-at-point-prev))
      ;; Should find first "hello", with match selected.
      (should (equal 'visual (bray-state)))
      ;; Point is at start of match.
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?h (char-after)))
      ;; Mark is after match.
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "hello" (meep-test-region-as-string))))))

(ert-deftest isearch-repeat-next-basic ()
  "Repeat last search forward.

Verifies: continues to next match after initial search."
  (let ((text-initial "hello world hello again hello end"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world hello again hello end
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Initial search for "hello".
      (simulate-input-for-meep
        '(:state normal :command meep-isearch-at-point-next))
      ;; Should be at second "hello" in visual state.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 17) (meep-test-point-line-column)))
      (should (equal '(1 . 12) (meep-test-mark-line-column)))
      (should (equal "hello" (meep-test-region-as-string)))
      ;; Repeat search (from visual state).
      (simulate-input-for-meep
        '(:state visual :command meep-isearch-repeat-next))
      ;; Should be at third "hello".
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 29) (meep-test-point-line-column)))
      (should (equal '(1 . 24) (meep-test-mark-line-column)))
      (should (equal "hello" (meep-test-region-as-string))))))

(ert-deftest isearch-repeat-prev-basic ()
  "Repeat last search backward.

Verifies: continues to previous match after initial search."
  (let ((text-initial "hello world hello again hello end"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor at start.
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to third "hello".
      (goto-char 25)
      ;; Cursor: hello world hello again hello end
      ;;                                  ^
      (should (equal '(1 . 24) (meep-test-point-line-column)))
      (should (equal ?h (char-after)))
      ;; Initial search backward for "hello".
      (simulate-input-for-meep
        '(:state normal :command meep-isearch-at-point-prev))
      ;; Should be at second "hello" in visual state.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 12) (meep-test-point-line-column)))
      (should (equal '(1 . 17) (meep-test-mark-line-column)))
      (should (equal "hello" (meep-test-region-as-string)))
      ;; Repeat search backward (from visual state).
      (simulate-input-for-meep
        '(:state visual :command meep-isearch-repeat-prev))
      ;; Should be at first "hello".
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 5) (meep-test-mark-line-column)))
      (should (equal "hello" (meep-test-region-as-string))))))


;; ---------------------------------------------------------------------------
;; Bounds: String

(ert-deftest bounds-of-string-inner-basic ()
  "Select string content without quotes.

Verifies: inner selection excludes the quote delimiters."
  (let ((text-initial "foo \"bar baz\" qux"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: foo "bar baz" qux
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Move inside the string (to 'b' of bar).
      (goto-char 6)
      ;; Cursor: foo "bar baz" qux
      ;;              ^
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?b (char-after)))
      (should (equal 'normal (bray-state)))
      ;; Move to inner bounds and immediately activate region by reversing.
      ;; Both commands in same block preserves `last-command'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-string-inner)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      ;; Should select "bar baz" without quotes.
      (should (equal 'visual (bray-state)))
      (should (equal "bar baz" (meep-test-region-as-string))))))

(ert-deftest bounds-of-string-outer-basic ()
  "Select string including quotes.

Verifies: outer selection includes the quote delimiters."
  (let ((text-initial "foo \"bar baz\" qux"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: foo "bar baz" qux
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Move inside the string (to 'b' of bar).
      (goto-char 6)
      ;; Cursor: foo "bar baz" qux
      ;;              ^
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?b (char-after)))
      (should (equal 'normal (bray-state)))
      ;; Move to outer bounds and immediately activate region by reversing.
      ;; Both commands in same block preserves `last-command'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-string)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      ;; Should select "\"bar baz\"" including quotes.
      (should (equal 'visual (bray-state)))
      (should (equal "\"bar baz\"" (meep-test-region-as-string))))))


;; ---------------------------------------------------------------------------
;; Bounds: List item
;;
;; The `list-item' text object selects one top-level element of a bracketed,
;; separated list.  The recognized brackets and separators come from
;; `meep-list-item-bounds' (default: parenthesized, comma-separated).

(ert-deftest mark-list-item-inner-basic ()
  "Mark a single list item, excluding separators.

Verifies: the default parenthesized/comma-separated spec selects the
list item at point without surrounding separators or blank-space."
  (let ((text-initial "foo(a, bar, baz)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor: foo(a, bar, baz)
      ;;               ^
      (goto-char 8)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'visual (bray-state)))
      (should (equal "bar" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-outer-trailing-separator ()
  "Mark a list item including its trailing separator.

Verifies: a non-final list item's outer bounds absorb the following
separator and blank-space so removal leaves a well-formed list."
  (let ((text-initial "foo(a, bar, baz)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor on `bar' (a middle list item).
      (goto-char 8)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-outer))
      (should (equal 'visual (bray-state)))
      (should (equal "bar, " (meep-test-region-as-string))))))

(ert-deftest mark-list-item-outer-last-leading-separator ()
  "Mark the final list item including its leading separator.

Verifies: the last list item's outer bounds absorb the preceding
separator and blank-space (there is no trailing separator to take)."
  (let ((text-initial "foo(a, bar, baz)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor on `baz' (the final list item).
      (goto-char 13)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-outer))
      (should (equal 'visual (bray-state)))
      (should (equal ", baz" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-outer-sole-list-item ()
  "The sole list item's outer bounds equal its inner bounds.

Verifies: with no separator in the list there is nothing to absorb, so
`outer' selects just the list item (not the brackets)."
  (let ((text-initial "foo(bar)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor on the only list item `bar'.
      (goto-char 5)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-outer))
      (should (equal 'visual (bray-state)))
      (should (equal "bar" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-inner-on-enclosing-brackets ()
  "Point on a list's own bracket resolves into that list, symmetrically.

Verifies: with point resting on the list's opening bracket the object
selects the first list item, and on the closing bracket the last - the two
edges mirror each other (the opening bracket is not a dead spot)."
  (let ((text-initial "f(a, b, c)"))
    ;; On the opening `(' (pos 2): the first list item.
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 2)
      (should (equal ?\( (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'visual (bray-state)))
      (should (equal "a" (meep-test-region-as-string))))
    ;; On the closing `)' (pos 10): the last list item.
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 10)
      (should (equal ?\) (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'visual (bray-state)))
      (should (equal "c" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-inner-nested-brackets ()
  "Nested brackets do not split a list item - regardless of bracket type.

Verifies: nesting is read from the buffer's syntax tree.  The `,' inside the
nested [] and () sub-expressions are nested deeper than the enclosing `('
list, so the whole bracketed group is a single list item."
  (let ((text-initial "foo(a, [c, (0, 1)], {e, f}, g)"))
    (with-meep-test text-initial
      ;; `c-mode' only for its syntax table (so [] {} () all nest); the spec
      ;; is set here rather than relying on the c-mode preset's data.
      (c-mode)
      (bray-mode 1)
      (setq-local meep-list-item-bounds '(((?\( . ?\)) (","))))
      ;; Cursor on the `[' of the second list item.
      (goto-char 8)
      (should (equal ?\[ (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'visual (bray-state)))
      (should (equal "[c, (0, 1)]" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-inner-brace-group ()
  "A brace group is a single list item; stepping never enters it.

Verifies the reported case: in `a(b, {c, d, e}, f)' the list items of the
`()' list are `b', `{c, d, e}' and `f'.  The commas inside the braces are
nested (per the syntax tree) and never split the enclosing `()' list item."
  (let ((text-initial "a(b, {c, d, e}, f)"))
    (with-meep-test text-initial
      ;; `c-mode' for its syntax table; the spec is set explicitly so the test
      ;; does not depend on the c-mode preset's data.
      (c-mode)
      (bray-mode 1)
      (setq-local meep-list-item-bounds '(((?\( . ?\)) (","))))
      ;; Mark the brace list item from inside it.
      ;;     a(b, {c, d, e}, f)
      ;;          ^ pos 6 (the open brace)
      (goto-char 6)
      (should (equal ?{ (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'visual (bray-state)))
      (should (equal "{c, d, e}" (meep-test-region-as-string)))))
  ;; Stepping forward from `b' must skip straight over the brace group to `f'.
  (let ((text-initial "a(b, {c, d, e}, f)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (setq-local meep-list-item-bounds '(((?\( . ?\)) (","))))
      (goto-char 3)
      (should (equal ?b (char-after)))
      ;; Next list item: the brace group's start (pos 6), NOT inside it.
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next))
      (should (equal 6 (point)))
      (should (equal ?{ (char-after)))
      ;; Next list item: `f' (pos 17) - the brace interior is never entered.
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next))
      (should (equal 17 (point)))
      (should (equal ?f (char-after))))))

(ert-deftest mark-list-item-per-pair-separators ()
  "Each bracket pair uses its own separator.

Verifies the framework, with the spec set explicitly (not via a preset):
`()' separates on `,' while `{}' separates on `;'.  A statement inside a
brace block keeps its own parenthesized commas intact (they are nested), so
`{ g(a, b); h(); }' splits into the statements `g(a, b)' and `h()'."
  (let ((spec '(((?\( . ?\)) (",")) ((?\{ . ?\}) (";")))))
    ;; `()' separates on `,'.
    (let ((text-initial "f(a, b) { x; y; }"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds spec)
        ;; Inside the parens, on `a'.
        (goto-char 3)
        (should (equal ?a (char-after)))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-list-item-inner))
        (should (equal "a" (meep-test-region-as-string)))))
    ;; `{}' separates on `;', not `,'.
    (let ((text-initial "{ g(a, b); h(); }"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds spec)
        ;; Inside the brace block, on the first statement `g(a, b)'.
        ;;     { g(a, b); h(); }
        ;;       ^ pos 3
        (goto-char 3)
        (should (equal ?g (char-after)))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-list-item-inner))
        ;; The whole statement - its `,' is nested inside () so does not split.
        (should (equal "g(a, b)" (meep-test-region-as-string)))))))

(ert-deftest mark-list-item-brace-separator-fallback ()
  "Separator entries for one bracket are tried in order, first match wins.

Verifies the fallback chain (spec set explicitly, not via a preset): a `{}'
that lists `;' then `,' splits a statement block on `;', and an initializer
with no `;' falls back to `,'.  When a `;' is present it always wins (the
`,' is not consulted)."
  (let ((spec '(((?\{ . ?\}) (";")) ((?\{ . ?\}) (",")))))
    ;; Statement block: splits on `;'.
    (let ((text-initial "{ x; y; }"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds spec)
        (goto-char 3)
        (should (equal ?x (char-after)))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-list-item-inner))
        (should (equal "x" (meep-test-region-as-string)))))
    ;; Initializer with no `;': falls back to `,'.
    (let ((text-initial "{ 1, 2, 3 }"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds spec)
        (goto-char 3)
        (should (equal ?1 (char-after)))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-list-item-inner))
        (should (equal "1" (meep-test-region-as-string)))))
    ;; Mixed: a `;' is present, so `;' wins and the `,' stays inside one element.
    (let ((text-initial "{ a, b; c }"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds spec)
        (goto-char 3)
        (should (equal ?a (char-after)))
        (simulate-input-for-meep
          '(:state normal :command meep-region-mark-list-item-inner))
        (should (equal "a, b" (meep-test-region-as-string)))))))

(ert-deftest list-item-trailing-separator ()
  "A trailing separator closes the list, not an empty final list item.

Verifies: in a list with a trailing comma the blank span between that comma
and the close bracket is not a phantom list item.  Stepping forward from the
last list item clamps instead of entering the gap, and marking with point
parked in the gap selects the last real list item."
  ;; Only `()' is involved, so the built-in default spec (parens, comma)
  ;; applies - no preset or override needed.
  ;; format-next-line: off
  (let ((text-initial (concat "fn(\n" "  a,\n" "  b,\n" "  c,\n" ")")))
       ;;                  f1 n2 (3 \n4 _5 _6 a7 ,8 \n9 _10 _11 b12 ,13 \n14
       ;;                  _15 _16 c17 ,18 \n19 )20
       ;; Stepping from `c' does not advance into the trailing gap.
       (with-meep-test text-initial
                       (c-mode)
                       (bray-mode 1)
                       (goto-char 17)
                       (should (equal ?c (char-after)))
                       (simulate-input-for-meep
                        '(:state normal :command meep-move-list-item-next))
                       (should (equal 17 (point))))
       ;; Point parked in the trailing gap marks the last real list item.
       (with-meep-test text-initial
                       (c-mode)
                       (bray-mode 1)
                       ;; On the newline between the trailing `,' and `)'.
                       (goto-char 19)
                       (should (equal ?\n (char-after)))
                       (simulate-input-for-meep
                        '(:state normal :command meep-region-mark-list-item-inner))
                       (should (equal "c" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-inner-separator-in-string ()
  "A separator inside a string does not split a list item.

Verifies: positions inside a string (per the mode's syntax) are skipped
when scanning for separators."
  (let ((text-initial "f(a, \"x, y\", b)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor inside the string `"x, y"'.
      (goto-char 8)
      (should (equal ?\, (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'visual (bray-state)))
      (should (equal "\"x, y\"" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-syntax-propertize ()
  "Bracket scanning works under a custom `syntax-propertize-function'.

Verifies: `meep--list-item-enclosing-bounds' resolves the enclosing list from
the syntax tree (`syntax-ppss' / `scan-sexps').  Finding it forces a propertize
pass - arbitrary regexp code that overwrites the global match data and may move
point - which must not disturb the scan.  With such a function in effect, the
list item must still be marked."
  (let ((text-initial "foo(a, bar, baz)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; A propertize pass that runs a regexp, leaving the match data on a
      ;; word and point away from the brackets being scanned.
      (setq-local syntax-propertize-function
                  (lambda (start end)
                    (goto-char start)
                    (while (re-search-forward "[a-z]+" end t))))
      (syntax-ppss-flush-cache (point-min))
      ;; Cursor on `bar'.
      (goto-char 8)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'visual (bray-state)))
      (should (equal "bar" (meep-test-region-as-string))))))

(ert-deftest mark-list-item-outside-list-not-found ()
  "Marking a list item outside any bracket list does nothing.

Verifies: with no enclosing list, the command leaves normal state and
activates no region."
  (let ((text-initial "no parens here"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      (goto-char 5)
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-list-item-inner))
      (should (equal 'normal (bray-state)))
      (should-not (region-active-p)))))

(ert-deftest move-list-item-next-prev-basic ()
  "Move forward and backward across list items by point.

Verifies: `meep-move-list-item-next' / `-prev' land on the start of the
adjacent list item, confined to the enclosing list."
  (let ((text-initial "foo(aa, bb, cc)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor on `aa' (first list item).
      ;;     foo(aa, bb, cc)
      ;;         ^ pos 5
      (goto-char 5)
      (should (equal ?a (char-after)))
      ;; Next list item: start of `bb' (pos 9).
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next))
      (should (equal 9 (point)))
      (should (equal ?b (char-after)))
      ;; Next list item: start of `cc' (pos 13).
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next))
      (should (equal 13 (point)))
      (should (equal ?c (char-after)))
      ;; Previous list item: back to start of `bb' (pos 9).
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-prev))
      (should (equal 9 (point)))
      (should (equal ?b (char-after))))))

(ert-deftest move-list-item-next-prev-end-basic ()
  "Move to the end of the current/next list item (vim `e'-style).

Verifies: `meep-move-list-item-next-end' lands at the end of the *current*
list item (not the next one), counting the current list item as the first
step; repeating advances to the following list item's end.  `-prev-end'
lands at the end of the previous list item.  The landing is the end of the
list item text, excluding the separator."
  (let ((text-initial "foo(aa, bb, cc)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor inside the first list item `aa'.
      ;;     foo(aa, bb, cc)
      ;;         ^ pos 5
      (goto-char 5)
      (should (equal ?a (char-after)))
      ;; End of THIS list item `aa': after the second `a' (pos 7, the comma).
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next-end))
      (should (equal 7 (point)))
      (should (equal ?\, (char-after)))
      ;; Again: end of the next list item `bb' (pos 11, the comma).
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next-end))
      (should (equal 11 (point)))
      (should (equal ?\, (char-after)))
      ;; End of the previous list item `aa' (pos 7).
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-prev-end))
      (should (equal 7 (point)))
      (should (equal ?\, (char-after))))))

(ert-deftest move-list-item-from-mid-list-item ()
  "Motions count the current list item when point is inside one.

Verifies the vim-style semantics that distinguish `next' (w), `next-end'
\(e) and `prev' (b): starting from the *middle* of a list item, `next-end'
lands at the current list item's end, `prev' at its start, and `next' at the
next list item's start."
  (let ((text-initial "foo(aaa, bbb)"))
    ;;                  f1 o2 o3 (4 a5 a6 a7 ,8 _9 b10 b11 b12 )13
    ;; `next-end' from mid `aaa' -> end of `aaa' (pos 8, the comma).
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 6)
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next-end))
      (should (equal 8 (point))))
    ;; `prev' from mid `aaa' -> start of `aaa' (pos 5), not the previous list item.
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 6)
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-prev))
      (should (equal 5 (point))))
    ;; `next' from mid `aaa' -> start of the next list item `bbb' (pos 10).
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 6)
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-next))
      (should (equal 10 (point))))))

(ert-deftest move-list-item-count-and-clamp ()
  "A numeric prefix moves by several list items and clamps at the list ends.

Verifies: `meep-move-list-item-next'/`-prev' honour a count (the loop through
`meep--forward-multi'), and clamp at the first/last list item when the count
exceeds what the list holds rather than leaving it."
  (let ((text-initial "foo(aa, bb, cc)"))
    ;;                  f1 o2 o3 (4 a5 a6 ,7 _8 b9 b10 ,11 _12 c13 c14 )15
    ;; Count 2: jump from `aa' past `bb' to `cc'.
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 5)
      (simulate-input-for-meep
        [?\C-u ?2]
        '(:state normal :command meep-move-list-item-next))
      (should (equal 13 (point)))
      (should (equal ?c (char-after))))
    ;; Over-count forward clamps at the last list item.
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 5)
      (simulate-input-for-meep
        [?\C-u ?9]
        '(:state normal :command meep-move-list-item-next))
      (should (equal 13 (point))))
    ;; Over-count backward clamps at the first list item.
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 13)
      (simulate-input-for-meep
        [?\C-u ?9]
        '(:state normal :command meep-move-list-item-prev))
      (should (equal 5 (point))))
    ;; `prev' at the first list item does not move.
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 5)
      (simulate-input-for-meep
        '(:state normal :command meep-move-list-item-prev))
      (should (equal 5 (point))))))

(ert-deftest move-list-item-negative-arg-reverses ()
  "A negative prefix reverses each motion's direction.

Verifies the `(< arg 0)' branches of all four motion commands - untested by the
positive-arg cases - which mirror the word/symbol convention.  From `bb' in
`foo(aa, bb, cc)': `next' and `next-end' with -1 step backward to a list item
*start* (here `aa'), while `prev' and `prev-end' with -1 step forward to a list
item *end* (here `bb's, the comma)."
  (let ((text-initial "foo(aa, bb, cc)"))
    ;;                  f1 o2 o3 (4 a5 a6 ,7 _8 b9 b10 ,11 _12 c13 c14 )15
    ;; `next' -1: backward to the start of `aa' (pos 5).
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 9)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        [?\C-u ?- ?1]
        '(:state normal :command meep-move-list-item-next))
      (should (equal 5 (point)))
      (should (equal ?a (char-after))))
    ;; `next-end' -1: likewise backward to the start of `aa' (pos 5).
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 9)
      (simulate-input-for-meep
        [?\C-u ?- ?1]
        '(:state normal :command meep-move-list-item-next-end))
      (should (equal 5 (point)))
      (should (equal ?a (char-after))))
    ;; `prev' -1: forward to the end of `bb' (pos 11, the comma).
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 9)
      (simulate-input-for-meep
        [?\C-u ?- ?1]
        '(:state normal :command meep-move-list-item-prev))
      (should (equal 11 (point)))
      (should (equal ?\, (char-after))))
    ;; `prev-end' -1: likewise forward to the end of `bb' (pos 11).
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      (goto-char 9)
      (simulate-input-for-meep
        [?\C-u ?- ?1]
        '(:state normal :command meep-move-list-item-prev-end))
      (should (equal 11 (point)))
      (should (equal ?\, (char-after))))))

(ert-deftest move-to-bounds-of-list-item-inner-basic ()
  "Move to list item inner bounds, then select by reversing.

Verifies: `meep-move-to-bounds-of-list-item-inner' drives the same bounds
as the mark command via the motion + activate-and-reverse path."
  (let ((text-initial "foo(a, bar, baz)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor at the start of `bar' so moving to the inner end spans it.
      (goto-char 8)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-list-item-inner)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      (should (equal 'visual (bray-state)))
      (should (equal "bar" (meep-test-region-as-string))))))

(ert-deftest move-to-bounds-of-list-item-outer-basic ()
  "Move to the outer bound of the list item (past its trailing separator).

Verifies the outer variant `meep-move-to-bounds-of-list-item': with a
positive list item it moves point to the end of the outer bounds, i.e. past
the list item and its trailing separator.  (The outer bounds are asymmetric,
so this command is tested by where point lands rather than via the
`activate-and-reverse-motion' idiom, which assumes symmetric bounds.)"
  (let ((text-initial "foo(a, bar, qux)"))
    ;;                  f1 o2 o3 (4 a5 ,6 _7 b8 a9 r10 ,11 _12 q13 u14 x15 )16
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Start at the beginning of `bar'.
      (goto-char 8)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-list-item))
      ;; Point lands after "bar, " - on `q' of the next list item.
      (should (equal 13 (point)))
      (should (equal ?q (char-after))))))

(ert-deftest move-to-bounds-of-list-item-outer-reverse-motion ()
  "Outer list item move + reverse-motion must select the whole outer span.

Verifies: `meep-move-to-bounds-of-list-item' (outer) followed by
`meep-region-activate-and-reverse-motion' selects the list item plus its
trailing separator (\"bar, \"), matching `meep-region-mark-list-item-outer'.

The outer list item bounds are asymmetric (they absorb the trailing
separator on one side only), and reverse-motion reconstructs the span by
re-running the motion from the mark.  When that reconstruction is wrong it
selects only the separator (\", \") instead of the list item plus separator."
  (let ((text-initial "foo(a, bar, baz)"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Start at the beginning of `bar' (a middle list item).
      (goto-char 8)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-list-item)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      (should (equal 'visual (bray-state)))
      (should (equal "bar, " (meep-test-region-as-string))))))

(ert-deftest move-to-bounds-of-list-item-outer-backward-boundary ()
  "Backward outer move from a list item's first char steps to the previous one.

Verifies: list items are adjacent, so a list item's first character is also the
previous list item's outer trailing edge.  A backward outer move there targets
the list item *ending* at point (stepping to the previous list item's start, like
symbol/word motions) rather than the one starting there - this is what lets
move + `meep-region-activate-and-reverse-motion' reconstruct the outer span."
  (let ((text-initial "foo(a, bar, baz)"))
    ;;                  f1 o2 o3 (4 a5 ,6 _7 b8 a9 r10 ,11 _12 b13 a14 z15 )16
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; On the first char of `baz' (pos 13), which is `bar's outer trailing
      ;; edge: a backward outer move steps to the start of `bar' (pos 8).
      (goto-char 13)
      (should (equal ?b (char-after)))
      (simulate-input-for-meep
        [?\C-u ?- ?1]
        '(:state normal :command meep-move-to-bounds-of-list-item))
      (should (equal 8 (point)))
      (should (equal ?b (char-after)))
      ;; Mid-list-item is unaffected: from inside `baz' (pos 14) a backward outer
      ;; move lands at `baz's own outer start (pos 11, its leading separator).
      (goto-char 14)
      (simulate-input-for-meep
        [?\C-u ?- ?1]
        '(:state normal :command meep-move-to-bounds-of-list-item))
      (should (equal 11 (point)))
      (should (equal ?\, (char-after))))))

(ert-deftest move-to-bounds-of-list-item-empty-yields-bounds ()
  "An empty list or leading-empty slot yields bounds, not \"not found\".

Verifies the documented `meep--bounds-of-list-item' contract through the public
move command: inside an empty list `()', or at the empty first slot of `(,a)',
`meep-move-to-bounds-of-list-item-inner' resolves (empty) bounds and moves -
it does *not* report \"Not found\" the way it does outside any list.  This pins
the intentional asymmetry with a *trailing* empty slot, which is dropped (see
`list-item-trailing-separator')."
  (let ((not-found "Not found: bounds of list item"))
    ;; Empty list `f()': point between the brackets (pos 3, on `)').
    (let ((text-initial "f()"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds '(((?\( . ?\)) (","))))
        (goto-char 3)
        (simulate-input-for-meep
          '(:state normal :command meep-move-to-bounds-of-list-item-inner))
        (should-not (member not-found (meep-test-messages)))
        (should (equal 3 (point)))))
    ;; Leading-empty slot: the empty first item of `f(,a)' (pos 3, on `,').
    (let ((text-initial "f(,a)"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds '(((?\( . ?\)) (","))))
        (goto-char 3)
        (simulate-input-for-meep
          '(:state normal :command meep-move-to-bounds-of-list-item-inner))
        (should-not (member not-found (meep-test-messages)))
        (should (equal 3 (point)))))
    ;; Contrast: outside any list *does* report "Not found" (proving the checks
    ;; above are not vacuous).
    (let ((text-initial "no parens"))
      (with-meep-test text-initial
        (c-mode)
        (bray-mode 1)
        (setq-local meep-list-item-bounds '(((?\( . ?\)) (","))))
        (goto-char 4)
        (simulate-input-for-meep
          '(:state normal :command meep-move-to-bounds-of-list-item-inner))
        (should (member not-found (meep-test-messages)))))))

(defun meep-test--list-item-mark-inner (text mode &optional bounds)
  "Return the inner `list-item' region selected by the public mark command.
TEXT contains a single `|' marking point (deleted before marking).  MODE is the
major mode enabled so the buffer's comment and bracket syntax is in effect.
BOUNDS overrides `meep-list-item-bounds' (defaults to parenthesized,
comma-separated).  Drives `meep-region-mark-list-item-inner' - the user-facing
command - and returns the marked region, so only external behavior is checked."
  (with-meep-test text
    (funcall mode)
    (bray-mode 1)
    (setq-local meep-list-item-bounds (or bounds '(((?\( . ?\)) (",")))))
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (simulate-input-for-meep
      '(:state normal :command meep-region-mark-list-item-inner))
    (meep-test-region-as-string)))

(ert-deftest mark-list-item-inner-skips-adjacent-comment ()
  "A comment padding a list item is trivia, excluded from the inner selection.

Verifies (via `meep-region-mark-list-item-inner'): a comment between the
separator and the item, after the item, a line comment ahead of or trailing it,
and several comments in a row are all skipped - the selection is just the item."
  (should (equal "e" (meep-test--list-item-mark-inner "a(b, /* c, d, */ |e);" #'c-mode)))
  (should (equal "b" (meep-test--list-item-mark-inner "a(b| /* x, y */, e);" #'c-mode)))
  (should (equal "e" (meep-test--list-item-mark-inner "a(b,\n  // x, y\n  |e);" #'c-mode)))
  ;; A line comment's terminating newline is blank-space the backward scan
  ;; must not stall inside.
  (should (equal "e" (meep-test--list-item-mark-inner "a(b, |e // tail\n , g);" #'c-mode)))
  (should (equal "e" (meep-test--list-item-mark-inner "a(b, /*x*/ /*y*/ |e);" #'c-mode))))

(ert-deftest mark-list-item-inner-point-on-leading-comment ()
  "Point resting on an item's leading comment resolves to that item.

Verifies: the comment between a separator and the item text is trivia owned by
the following item and excluded, even with point inside it; each item keeps its
own leading comment as trivia."
  (should (equal "c" (meep-test--list-item-mark-inner "fn(b, |/*c*/ c, /*d*/ d, e)" #'c-mode)))
  (should (equal "d" (meep-test--list-item-mark-inner "fn(b, /*c*/ c, /*d*/ |d, e)" #'c-mode))))

(ert-deftest mark-list-item-inner-keeps-interior-comment ()
  "A comment between two tokens of one list item is interior and kept.

Verifies: only edge comments are trivia; one surrounded by the item's own
tokens stays in the selection."
  (should (equal "e /* x */ f" (meep-test--list-item-mark-inner "a(|e /* x */ f, g);" #'c-mode))))

(ert-deftest mark-list-item-inner-comment-no-comment-syntax ()
  "Without comment syntax the `/* */' text is literal and stays in the item.

Verifies: comment trivia is read from the mode's syntax; in `fundamental-mode'
`/* c */' is ordinary text and part of the item."
  (should
   (equal "/* c */ e" (meep-test--list-item-mark-inner "a(b, /* c */ |e);" #'fundamental-mode))))

(ert-deftest mark-list-item-inner-union-separators ()
  "Several separator strings in one entry split on any of them (union).

Verifies: unlike a fallback chain (the first matching group wins), every
separator in one group is active at once, so a list mixing `,' and `;' splits
at each."
  (let ((both '(((?\( . ?\)) ("," ";")))))
    (should (equal "a" (meep-test--list-item-mark-inner "(|a, b; c)" #'c-mode both)))
    (should (equal "b" (meep-test--list-item-mark-inner "(a, |b; c)" #'c-mode both)))
    (should (equal "c" (meep-test--list-item-mark-inner "(a, b; |c)" #'c-mode both)))))

(ert-deftest mark-list-item-inner-whitespace-separator ()
  "A `t' separator splits on whitespace runs (e.g. Lisp), not a literal token.

Verifies: consecutive whitespace counts once (no phantom empty items), newlines
and tabs count, whitespace at the list edges is trivia, a separator inside a
nested bracket does not split, and a string's interior whitespace is preserved."
  (let ((ws '(((?\( . ?\)) t))))
    (should (equal "b" (meep-test--list-item-mark-inner "(foo a |b c)" #'emacs-lisp-mode ws)))
    ;; Consecutive whitespace is one separator (no empty item between).
    (should (equal "b" (meep-test--list-item-mark-inner "(foo a  |b  c)" #'emacs-lisp-mode ws)))
    ;; Newlines and indentation count as whitespace.
    (should (equal "b" (meep-test--list-item-mark-inner "(foo\n  a\n  |b)" #'emacs-lisp-mode ws)))
    ;; Leading/trailing whitespace inside the brackets is edge trivia.
    (should (equal "a" (meep-test--list-item-mark-inner "( |a b )" #'emacs-lisp-mode ws)))
    ;; A nested list is a single item; its interior whitespace does not split.
    (should (equal "(b c)" (meep-test--list-item-mark-inner "(a |(b c) d)" #'emacs-lisp-mode ws)))
    ;; Whitespace inside a string is not a separator.
    (should
     (equal "\"a b\"" (meep-test--list-item-mark-inner "(|\"a b\" c)" #'emacs-lisp-mode ws)))))

(ert-deftest bounds-commands-all-resolve-to-commands ()
  "Every `meep-bounds-commands' entry maps a key to a real interactive command.

Verifies the dispatch table has no typos or unbound function names - covering
the `list-item' entries (`i'/`I') added with the text object and all the
others.  A data-shape check on the defcustom, independent of any preset's
contents."
  (dolist (entry meep-bounds-commands)
    (let ((key (nth 0 entry))
          (fn (nth 1 entry))
          (desc (nth 2 entry)))
      (should (characterp key))
      (should (commandp fn))
      (should (stringp desc))))
  ;; The list-item entries are present and point at the right commands.
  (should (eq 'meep-move-to-bounds-of-list-item-inner (nth 1 (assq ?i meep-bounds-commands))))
  (should (eq 'meep-move-to-bounds-of-list-item (nth 1 (assq ?I meep-bounds-commands)))))


;; ---------------------------------------------------------------------------
;; Bounds: Comment

(ert-deftest bounds-of-comment-inner-basic ()
  "Select comment content without delimiters.

Verifies: inner selection excludes comment markers, with the
c-mode preset auto-consulted on demand - no explicit
`meep-preset-ensure' call required."
  (let ((text-initial "foo // bar baz\nqux"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; No `meep-preset-ensure' - the preset is consulted lazily
      ;; via `meep-preset-ensure-variable'; the var stays non-local.
      (should-not (local-variable-p 'meep-bounds-for-inner-comment))
      ;; Cursor: foo // bar baz
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Move inside the comment (to 'b' of bar).
      (goto-char 8)
      ;; Cursor: foo // bar baz
      ;;                ^
      (should (equal '(1 . 7) (meep-test-point-line-column)))
      (should (equal ?b (char-after)))
      (should (equal 'normal (bray-state)))
      ;; Move to inner bounds and immediately activate region by reversing.
      ;; Both commands in same block preserves `last-command'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-comment-inner)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      ;; Should select comment content without //.
      (should (equal 'visual (bray-state)))
      ;; Inner content is "bar baz" (starts after // and space).
      (should (equal "bar baz" (meep-test-region-as-string))))))

(ert-deftest bounds-of-comment-outer-basic ()
  "Select comment including delimiters.

Verifies: outer selection includes comment markers."
  (let ((text-initial "foo // bar baz\nqux"))
    (with-meep-test text-initial
      (c-mode)
      (bray-mode 1)
      ;; Cursor: foo // bar baz
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Move inside the comment (to 'b' of bar).
      (goto-char 8)
      ;; Cursor: foo // bar baz
      ;;                ^
      (should (equal '(1 . 7) (meep-test-point-line-column)))
      (should (equal ?b (char-after)))
      (should (equal 'normal (bray-state)))
      ;; Move to outer bounds and immediately activate region by reversing.
      ;; Both commands in same block preserves `last-command'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-comment)
        '(:state normal :command meep-region-activate-and-reverse-motion))
      ;; Should select entire comment including //.
      (should (equal 'visual (bray-state)))
      (should (equal "// bar baz" (meep-test-region-as-string))))))


;; ---------------------------------------------------------------------------
;; Exchange Point and Mark

(ert-deftest exchange-point-and-mark-basic ()
  "Swap point and mark positions.

Verifies: point and mark positions are exchanged."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Create a selection.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next))
      ;; Selection: hello world
      ;;            ^-----^
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      ;; Exchange point and mark (using renamed function).
      (simulate-input-for-meep
        '(:state visual :command meep-region-activate-and-reverse))
      ;; Now point is at start, mark at end.
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "hello " (meep-test-region-as-string))))))

(ert-deftest exchange-point-and-mark-motion ()
  "Exchange point and mark using motion-based approach.

Verifies: exchange-motion swaps positions correctly."
  (let ((text-initial "hello world foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world foo
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Create a selection across two words and immediately reverse.
      ;; Combining into single block preserves `last-command'.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next)
        '(:state visual :command meep-move-symbol-next)
        '(:state visual :command meep-region-activate-and-reverse-motion))
      ;; Now point is at start, mark at end.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 12) (meep-test-mark-line-column)))
      (should (equal "hello world " (meep-test-region-as-string))))))


;; ---------------------------------------------------------------------------
;; Movement: Completeness

(ert-deftest movement-word-prev-end ()
  "Move to end of previous word.

Verifies: word-prev-end lands after previous word's last character."
  (let ((text-initial "hello world foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: hello world foo
      ;;                        ^
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Move to end of previous word.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-prev-end))
      ;; Cursor: hello world foo
      ;;                    ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal ?d (char-before)))
      (should (equal ?\s (char-after)))
      ;; Move to end of previous word again.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-prev-end))
      ;; Cursor: hello world foo
      ;;              ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal ?o (char-before)))
      (should (equal ?\s (char-after))))))

(ert-deftest movement-symbol-prev-end ()
  "Move to end of previous symbol.

Verifies: symbol-prev-end lands after previous symbol's last character."
  (let ((text-initial "foo_bar baz_qux end"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: foo_bar baz_qux end
      ;;                            ^
      (should (equal '(1 . 19) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Move to end of previous symbol.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-prev-end))
      ;; Cursor: foo_bar baz_qux end
      ;;                        ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal ?x (char-before)))
      (should (equal ?\s (char-after)))
      ;; Move to end of previous symbol again.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-prev-end))
      ;; Cursor: foo_bar baz_qux end
      ;;                ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 7) (meep-test-point-line-column)))
      (should (equal ?r (char-before)))
      (should (equal ?\s (char-after))))))

(ert-deftest movement-find-char-at-prev ()
  "Find character backward on line (at position).

Verifies: find-at-prev moves to the target character when searching backward."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: hello world
      ;;                    ^
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Find 'o' backward (at position).
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-at-prev)
        "o")
      ;; Cursor: hello world
      ;;                ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 7) (meep-test-point-line-column)))
      (should (equal ?o (char-after)))
      ;; Find next 'o' backward.
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-at-prev)
        "o")
      ;; Cursor: hello world
      ;;             ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?o (char-after))))))

(ert-deftest movement-find-char-till-prev ()
  "Find character backward on line (till position).

Verifies: find-till-prev stops just after the target character."
  (let ((text-initial "hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: hello world
      ;;                    ^
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Find 'o' backward (till position).
      (simulate-input-for-meep
        '(:state normal :command meep-move-find-char-on-line-till-prev)
        "o")
      ;; Cursor: hello world
      ;;                 ^
      ;; find-till-prev lands just after the 'o' in "world".
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal ?o (char-before)))
      (should (equal ?r (char-after))))))

(ert-deftest movement-paragraph-prev ()
  "Move to previous paragraph boundary.

Verifies: paragraph-prev moves to paragraph separator."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "First paragraph.\n"
          "\n"
          "Second paragraph.")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: Second paragraph.
      ;;                          ^
      (should (equal '(3 . 17) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Move to previous paragraph boundary.
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-prev))
      ;; Point at blank line (paragraph separator).
      (should (equal 'normal (bray-state)))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal ?\n (char-after)))
      ;; Move again to reach first paragraph.
      (simulate-input-for-meep
        '(:state normal :command meep-move-paragraph-prev))
      ;; Point at start of first paragraph.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?F (char-after))))))

(ert-deftest movement-sentence-prev ()
  "Move to previous sentence boundary.

Verifies: sentence-prev moves to start of current/previous sentence."
  (let ((text-initial "First sentence.  Second sentence.  Third."))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: First sentence.  Second sentence.  Third.
      ;;                                                  ^
      (should (equal '(1 . 41) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Move to previous sentence.
      (simulate-input-for-meep
        '(:state normal :command meep-move-sentence-prev))
      ;; Point at start of "Third."
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 35) (meep-test-point-line-column)))
      (should (equal ?T (char-after)))
      ;; Move again.
      (simulate-input-for-meep
        '(:state normal :command meep-move-sentence-prev))
      ;; Point at start of "Second sentence."
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 17) (meep-test-point-line-column)))
      (should (equal ?S (char-after))))))

(ert-deftest movement-sexp-over-prev ()
  "Move backward through s-expression.

Verifies: sexp-over-prev navigates backward through expressions."
  (let ((text-initial "(foo) (bar) (baz)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: (foo) (bar) (baz)
      ;;                          ^
      (should (equal '(1 . 17) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Move backward through sexp (descends into expression).
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-over-prev))
      ;; Cursor: (foo) (bar) (baz)
      ;;                         ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 16) (meep-test-point-line-column)))
      (should (equal ?\) (char-after)))
      ;; Continue backward.
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-over-prev))
      ;; Cursor: (foo) (bar) (baz)
      ;;                      ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 13) (meep-test-point-line-column)))
      (should (equal ?b (char-after))))))

(ert-deftest sexp-depth-calc-cache-invalidated-on-edit ()
  "Cache is discarded after a length-preserving buffer edit.

Verifies: meep--sexp-depth-calc does not return stale depth after a
char-replacing edit that changes the bracket structure."
  ;; Use '(a (bc))': point at 'c' (pos 6) is clearly interior.
  (with-meep-test "(a (bc))"
    (emacs-lisp-mode)
    (bray-mode 1)
    ;; Point at 'c' (pos 6) inside two levels of parens -> depth 2.
    (goto-char 6)
    (should (equal ?c (char-after)))
    (should (equal 2 (meep--sexp-depth-calc)))
    ;; Replace '(' at pos 4 with 'x': buffer becomes "(a xbc))".
    ;; This is length-preserving so positions do not shift.
    (goto-char 4)
    (delete-char 1)
    (insert "x")
    ;; Back to pos 6 ('c'), now only one enclosing paren -> depth 1.
    (goto-char 6)
    (should (equal ?c (char-after)))
    (should (equal 1 (meep--sexp-depth-calc)))))

(ert-deftest movement-sexp-out-next ()
  "Move out of current s-expression forward.

Verifies: sexp-out-next moves to end of enclosing sexp."
  (let ((text-initial "(outer (inner content) end)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move inside inner.
      (goto-char 9)
      ;; Cursor: (outer (inner content) end)
      ;;                 ^
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?i (char-after)))
      ;; Move out of sexp (goes to outer closing paren).
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-out-next))
      ;; Cursor: (outer (inner content) end)
      ;;                                   ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 26) (meep-test-point-line-column)))
      (should (equal ?\) (char-after))))))

(ert-deftest movement-sexp-out-prev ()
  "Move out of current s-expression backward.

Verifies: sexp-out-prev moves to start of enclosing sexp content."
  (let ((text-initial "(outer (inner content) end)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move inside inner.
      (goto-char 15)
      ;; Cursor: (outer (inner content) end)
      ;;                       ^
      (should (equal '(1 . 14) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?c (char-after)))
      ;; Move out of inner sexp backward.
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-out-prev))
      ;; Cursor: (outer (inner content) end)
      ;;                 ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal ?i (char-after)))
      ;; Move out of outer sexp backward.
      (simulate-input-for-meep
        '(:state normal :command meep-move-by-sexp-out-prev))
      ;; Cursor: (outer (inner content) end)
      ;;          ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal ?o (char-after))))))

(ert-deftest movement-line-non-space-beginning ()
  "Move to first non-whitespace character on line.

Verifies: line-non-space-beginning skips leading whitespace."
  (let ((text-initial "    hello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of line.
      (goto-char (point-max))
      ;; Cursor:     hello world
      ;;                        ^
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Move to first non-space.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-non-space-beginning))
      ;; Cursor:     hello world
      ;;             ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?h (char-after))))))

(ert-deftest movement-line-non-space-end ()
  "Move to last non-whitespace character on line.

Verifies: line-non-space-end stops before trailing whitespace."
  (let ((text-initial "hello world    "))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to last non-space.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-non-space-end))
      ;; Cursor: hello world
      ;;                    ^
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal ?d (char-before)))
      (should (equal ?\s (char-after))))))

(ert-deftest movement-matching-bracket-outer ()
  "Jump to matching bracket (outer).

Verifies: matching-bracket-outer moves past the matching bracket."
  (let ((text-initial "(foo [bar] baz)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Start at opening paren.
      (goto-char 1)
      ;; Cursor: (foo [bar] baz)
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?\( (char-after)))
      ;; Jump to matching bracket (outer) - lands past closing paren.
      (simulate-input-for-meep
        '(:state normal :command meep-move-matching-bracket-outer))
      ;; Point past closing paren.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal nil (char-after))))))

(defun meep_tests-run-all ()
  (ert-run-tests-batch))

;; ---------------------------------------------------------------------------
;; Delete: Completeness

(ert-deftest delete-char-next-basic ()
  "Delete single character forward.

Verifies: delete-char-next removes character at point."
  (let ((text-initial "hello world")
        (text-expected "ello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Enter insert state and delete character at point.
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-char-next))
      ;; Cursor: ello world
      ;;         ^
      (should (equal 'insert (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?e (char-after))))))

(ert-deftest delete-char-prev-basic ()
  "Delete single character backward.

Verifies: delete-char-prev removes character before point."
  (let ((text-initial "hello world")
        (text-expected "hell world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to space.
      (goto-char 6)
      ;; Cursor: hello world
      ;;              ^
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?\s (char-after)))
      ;; Enter insert state and delete character before point.
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-char-prev))
      ;; Cursor: hell world
      ;;             ^
      (should (equal 'insert (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

(ert-deftest delete-same-syntax-prev-basic ()
  "Delete characters of same syntax class backward.

Verifies: only characters of same syntax deleted backward."
  (let ((text-initial "foo_bar baz")
        (text-expected "_bar baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move to underscore.
      (goto-char 4)
      ;; Cursor: foo_bar baz
      ;;            ^
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?_ (char-after)))
      ;; Enter insert state and delete same syntax backward (word chars only).
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-same-syntax-prev))
      ;; Result: "_bar baz" (only "foo" deleted).
      (should (equal 'insert (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?_ (char-after))))))

(ert-deftest delete-same-syntax-or-symbol-next-basic ()
  "Delete same syntax or entire symbol forward.

Verifies: deletes syntax class or full symbol depending on context."
  (let ((text-initial "foo_bar baz")
        (text-expected " baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Cursor: foo_bar baz
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Enter insert state and delete same syntax or symbol next.
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-same-syntax-or-symbol-next))
      ;; Result: " baz" (entire symbol foo_bar deleted).
      (should (equal 'insert (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

(ert-deftest delete-same-syntax-or-symbol-prev-basic ()
  "Delete same syntax or entire symbol backward.

Verifies: deletes syntax class or full symbol depending on context."
  (let ((text-initial "abc foo_bar baz")
        (text-expected "abc  baz"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      ;; Move after foo_bar.
      (goto-char 12)
      ;; Cursor: abc foo_bar baz
      ;;                     ^
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?\s (char-after)))
      ;; Enter insert state and delete same syntax or symbol prev.
      (simulate-input-for-meep
        '(:state normal :command meep-insert)
        '(:state insert :command meep-delete-same-syntax-or-symbol-prev))
      ;; Result: "abc  baz" (entire symbol foo_bar deleted).
      (should (equal 'insert (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 4) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

;; ---------------------------------------------------------------------------
;; Bounds Operations

(ert-deftest bounds-of-line-basic ()
  "Select entire line.

Verifies: bounds-of-line sets mark at start, moves point to end."
  (let* ((text-initial "hello world")
         (text-expected text-initial))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to bounds of line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-line))
      ;; Mark at start, point at end.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal nil (char-after)))
      ;; Buffer unchanged by bounds operation.
      (should (equal text-expected (buffer-string))))))

(ert-deftest bounds-of-line-inner-basic ()
  "Select line content excluding trailing whitespace.

Verifies: bounds-of-line-inner excludes trailing whitespace."
  (let* ((text-initial "hello world  ")
         (text-expected text-initial))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to inner bounds of line (excludes trailing whitespace).
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-line-inner))
      ;; Mark at start, point before trailing spaces.
      ;; "hello world" is 11 chars, so point is at column 11.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal ?\s (char-after)))
      (should (equal ?d (char-before)))
      ;; Buffer unchanged by bounds operation.
      (should (equal text-expected (buffer-string))))))

(ert-deftest bounds-of-paragraph-basic ()
  "Select paragraph.

Verifies: bounds-of-paragraph selects current paragraph."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "First paragraph.\n"
          "\n"
          "Second paragraph."))
        (text-expected
         ;; format-next-line: off
         (concat
          "First paragraph.\n"
          "\n"
          "Second paragraph.")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: First paragraph.
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?F (char-after)))
      ;; Move to bounds of paragraph.
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-paragraph))
      ;; Mark at paragraph start, point at paragraph end (blank line).
      (should (equal 'normal (bray-state)))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal ?\n (char-after)))
      ;; Buffer unchanged by bounds operation.
      (should (equal text-expected (buffer-string))))))

(ert-deftest bounds-of-sentence-basic ()
  "Select sentence.

Verifies: bounds-of-sentence selects current sentence."
  (let* ((text-initial "First sentence.  Second sentence.")
         (text-expected text-initial))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: First sentence.  Second sentence.
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?F (char-after)))
      ;; Move to bounds of sentence.
      (simulate-input-for-meep
        '(:state normal :command meep-move-to-bounds-of-sentence))
      ;; Mark at sentence start, point at sentence end.
      (should (equal 'normal (bray-state)))
      (should (equal '(1 . 15) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal ?. (char-before)))
      ;; Buffer unchanged by bounds operation.
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Clipboard Operations

;; ---------------------------------------------------------------------------
;; Region Operations

(ert-deftest region-expand-to-line-bounds-basic ()
  "Expand selection to full lines.

Verifies: region-expand-to-line-bounds extends to line boundaries."
  (let* ((text-initial "hello world")
         (text-expected text-initial))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Create partial selection using meep commands.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next-end))
      ;; Selection: hello
      (should (equal 'visual (bray-state)))
      (should (equal "hello" (meep-test-region-as-string)))
      ;; Expand to line bounds.
      (simulate-input-for-meep
        '(:state visual :command meep-region-expand-to-line-bounds))
      ;; Selection now covers entire line.
      (should (equal 'visual (bray-state)))
      (should (equal "hello world" (meep-test-region-as-string)))
      (should (equal '(1 . 11) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal nil (char-after)))
      ;; Buffer unchanged by region operation.
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Insert: Completeness

(ert-deftest insert-at-last-basic ()
  "Insert at last edit position.

Verifies: insert-at-last returns to where insert mode was last exited."
  (let ((text-initial "hello world")
        (text-expected "helloX world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Move to position 6, enter insert mode, type, exit.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-insert)
        "X"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string)))
      ;; Move away to beginning using meep movement.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-prev))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Insert at last - should return to where we exited insert mode.
      (simulate-input-for-meep
        '(:state normal :command meep-insert-at-last))
      (should (equal 'insert (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?\s (char-after)))
      ;; Buffer unchanged since no new typing occurred.
      (should (equal text-expected (buffer-string))))))

(ert-deftest insert-into-last-copy-basic ()
  "Copy region and insert at last position.

Verifies: copies selected text to last insert location without deleting original."
  (let ((text-initial "hello world")
        (text-intermediate "helloX world")
        (text-expected "helloXhe world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; First, create an insert point at position 6.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-insert)
        "X"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-intermediate (buffer-string)))
      ;; Move to beginning and select "he".
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-prev)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next))
      (should (equal 'visual (bray-state)))
      (should (equal "he" (meep-test-region-as-string)))
      ;; Copy to last insert position.
      (simulate-input-for-meep
        '(:state visual :command meep-insert-into-last-copy))
      ;; "he" copied to position 6 (after X), original text preserved.
      ;; Ends in insert state at the copied location.
      (should (equal 'insert (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 8) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

(ert-deftest insert-into-last-move-basic ()
  "Move region to last position.

Verifies: moves selected text to last insert location, deleting original."
  (let ((text-initial "hello world")
        (text-intermediate "helloX world")
        (text-expected "lloXhe world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; First, create an insert point at position 6.
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-next-end)
        '(:state normal :command meep-insert)
        "X"
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal text-intermediate (buffer-string)))
      ;; Move to beginning and select "he".
      (simulate-input-for-meep
        '(:state normal :command meep-move-symbol-prev)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next))
      (should (equal 'visual (bray-state)))
      (should (equal "he" (meep-test-region-as-string)))
      ;; Move to last insert position.
      (simulate-input-for-meep
        '(:state visual :command meep-insert-into-last-move))
      ;; "he" moved to position (original deleted).
      ;; Ends in insert state at the moved location.
      (should (equal 'insert (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal ?\s (char-after))))))

(ert-deftest insert-overwrite-basic ()
  "Toggle overwrite mode.

Verifies: insert-overwrite enters insert state with overwrite-mode enabled."
  (let* ((text-initial "hello world")
         (text-expected text-initial))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      (should-not overwrite-mode)
      ;; Enter overwrite mode and exit immediately.
      (simulate-input-for-meep
        '(:state normal :command meep-insert-overwrite)
        '(:state insert :command bray-state-stack-pop))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Overwrite mode should be disabled on exit.
      (should-not overwrite-mode)
      ;; Buffer unchanged since no actual typing occurred.
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Search: Regex

(ert-deftest isearch-repeat-next-with-regex ()
  "Repeat regex search forward.

Verifies: isearch-repeat-next works with regex patterns."
  (let* ((text-initial "foo123 bar456 foo789")
         (text-expected text-initial))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: foo123 bar456 foo789
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?f (char-after)))
      ;; Set up regex search state.
      (setq isearch-string "foo[0-9]+")
      (setq isearch-regexp t)
      (setq isearch-forward t)
      ;; Repeat search forward.
      (simulate-input-for-meep
        '(:state normal :command meep-isearch-repeat-next))
      ;; Should find first "foo123" and select it.
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 6) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "foo123" (meep-test-region-as-string)))
      ;; Repeat again to find second match.
      (simulate-input-for-meep
        '(:state visual :command meep-isearch-repeat-next))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 20) (meep-test-point-line-column)))
      (should (equal '(1 . 14) (meep-test-mark-line-column)))
      (should (equal "foo789" (meep-test-region-as-string)))
      (should (equal nil (char-after)))
      ;; Buffer unchanged by search.
      (should (equal text-expected (buffer-string))))))

(ert-deftest isearch-repeat-prev-with-regex ()
  "Repeat regex search backward.

Verifies: isearch-repeat-prev works with regex patterns."
  (let* ((text-initial "foo123 bar456 foo789")
         (text-expected text-initial))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to end of buffer.
      (goto-char (point-max))
      ;; Cursor: foo123 bar456 foo789
      ;;                             ^
      (should (equal '(1 . 20) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal nil (char-after)))
      ;; Set up regex search state for backward search.
      (setq isearch-string "foo[0-9]+")
      (setq isearch-regexp t)
      (setq isearch-forward nil)
      ;; Repeat search backward.
      (simulate-input-for-meep
        '(:state normal :command meep-isearch-repeat-prev))
      ;; Should find "foo789" (closest match going backward).
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 14) (meep-test-point-line-column)))
      (should (equal '(1 . 20) (meep-test-mark-line-column)))
      (should (equal "foo789" (meep-test-region-as-string)))
      ;; Repeat again to find first match.
      (simulate-input-for-meep
        '(:state visual :command meep-isearch-repeat-prev))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal '(1 . 6) (meep-test-mark-line-column)))
      (should (equal "foo123" (meep-test-region-as-string)))
      (should (equal ?f (char-after)))
      ;; Buffer unchanged by search.
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Miscellaneous

(ert-deftest char-insert-basic ()
  "Insert character at point.

Verifies: char-insert inserts the specified character."
  (let ((text-initial "hello world")
        (text-expected "Xhello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Insert character.
      (simulate-input-for-meep
        '(:state normal :command meep-char-insert)
        "X")
      ;; Character inserted at point.
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal ?h (char-after))))))

(ert-deftest char-insert-with-count ()
  "Insert character multiple times.

Verifies: char-insert respects count argument."
  (let ((text-initial "hello world")
        (text-expected "XXXhello world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Insert character 3 times using C-u prefix.
      (simulate-input-for-meep
        "\C-u3"
        '(:state normal :command meep-char-insert)
        "X")
      ;; Three characters inserted.
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 3) (meep-test-point-line-column)))
      (should (equal ?h (char-after))))))

(ert-deftest char-insert-replace-selection ()
  "Replace selection with character.

Verifies: char-insert replaces active region with character."
  (let ((text-initial "hello world")
        (text-expected "X world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello world
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Select "hello" using meep commands.
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-symbol-next-end))
      (should (equal 'visual (bray-state)))
      (should (equal "hello" (meep-test-region-as-string)))
      ;; Replace selection with X.
      (simulate-input-for-meep
        '(:state visual :command meep-char-insert)
        "X")
      ;; Selection replaced, state returns to normal.
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal ?X (char-after))))))

(ert-deftest surround-add-lines-basic ()
  "Surround current line with character pair.

Verifies: `T x' (`meep-surround-add-lines') wraps the line with matching delimiters."
  (let ((text-initial "hello")
        (text-expected "(hello)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Cursor: hello
      ;;         ^
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      (should (equal 'normal (bray-state)))
      (should (equal ?h (char-after)))
      ;; Surround current line with parens (`T x' line-wise add verb).
      (simulate-input-for-meep
        "Tx(")
      ;; Line surrounded, cursor moves right due to inserted open paren.
      ;; Cursor: (hello)
      ;;          ^
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal ?h (char-after))))))

(ert-deftest surround-replace-cursor-on-open ()
  "Replace the surrounding pair with point on the opening delimiter.

Verifies: the surround finder matches an opening delimiter at point, not only
one strictly before it; point on the open behaves like point on the close."
  (let ((text-initial "a(bc)d")
        (text-expected "a[bc]d"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2) ; On the opening "(".
      (should (equal ?\( (char-after)))
      ;; `t g [' replaces the surrounding pair with brackets.
      (simulate-input-for-meep
        "tg[")
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-replace-lines-current ()
  "Line-wise replace swaps the current line's pair when no region is active."
  (let ((text-initial "(a)")
        (text-expected "[a]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2) ; Inside the pair.
      ;; `T g [' replaces the line's pair with brackets.
      (simulate-input-for-meep
        "Tg[")
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-implied-region ()
  "Add wraps an inactive mark's span, falling back to point when no mark is set.
A set mark is the selection even when the region is inactive, so add wraps
mark..point; with no mark, an empty pair is inserted at point.  See
`meep--region-or-mark-bounds'."
  ;; Inactive mark: the mark..point span is wrapped.  `mark-even-if-inactive' nil
  ;; (a common user setting) so the span must be read from the mark directly.
  (let ((text-initial "hello world")
        (text-expected "hel(lo world)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (let ((mark-even-if-inactive nil))
        (goto-char 4) ; hel|lo world
        (set-mark (point-max))
        (deactivate-mark)
        (should-not (region-active-p))
        (simulate-input-for-meep
          "t("))
      (should (equal text-expected (buffer-string)))))
  ;; No mark at all: an empty pair is inserted at point.
  (let ((text-initial "hello world")
        (text-expected "hel()lo world"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-marker (mark-marker) nil)
      (goto-char 4)
      (should (null (mark t)))
      (simulate-input-for-meep
        "t(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-zero-count-clamps-to-one ()
  "A zero count wraps once, matching the delete/replace clamp.
`C-u 0' must not be a silent no-op."
  (let ((text-initial "foo")
        (text-expected "(foo)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-mark 1)
      (goto-char 4)
      (simulate-input-for-meep
        [?\C-u ?0]
        "t(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-lines-respects-partial-columns ()
  "Line-wise add wraps only the selected columns on a partial first / last line.
Add is constructive, so a boundary line wraps its selected span, not text outside
the selection."
  (let ((text-initial "abc def\nghi jkl")
        (text-expected "abc (def)\n(ghi) jkl"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 5) ; Start at "def" on line 1.
      (set-mark 13) ; End after "ghi" on line 2.
      (simulate-input-for-meep
        "T(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-lines-contracts-boundary-indentation ()
  "Line-wise add over partial lines leaves blank-space outside the delimiters."
  (let ((text-initial "    foo bar\n  baz qux")
        (text-expected "    foo (bar)\n  (baz) qux"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 9) ; Start at "bar" on line 1.
      (set-mark 18) ; End after "baz" on line 2.
      (simulate-input-for-meep
        "T(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-lines-full-lines-wrap-whole ()
  "Line-wise add over fully-selected lines wraps each whole line."
  (let ((text-initial "abc\nghi")
        (text-expected "(abc)\n(ghi)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (set-mark (point-max))
      (simulate-input-for-meep
        "T(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-lines-empty-mark-wraps-line ()
  "Line-wise add with a set-but-unmoved mark wraps the line, like the no-mark case.
A mark at point is an empty span, not a selection."
  (let ((text-initial "foo")
        (text-expected "(foo)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2)
      (set-mark 2) ; Mark at point: an empty (P . P) span.
      (should (eq (mark t) (point)))
      (simulate-input-for-meep
        "T(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-lines-region-edge-before-indent ()
  "Line-wise add leaves the region's left edge before the indentation.
Regression: the tail fixup shifted point/mark by the prefix length even when the
prefix went in past the indent, not at the region edge."
  (let ((text-initial "   foo\n   bar\n")
        (text-expected "   [foo]\n   [bar]\n"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-mark 1) ; Left edge at column 0, before the indent.
      (goto-char (point-max))
      (simulate-input-for-meep
        "T[")
      (should (equal text-expected (buffer-string)))
      (should (eq 1 (mark t))))))

(ert-deftest surround-add-content-starts-with-open-delimiter ()
  "Add tracks the inserted open with a marker, not by matching text.
When the wrapped content begins with the same character as the open delimiter,
point and the mark must still land just inside the inserted pair - a text-match
on the prefix could not tell the inserted open from the identical leading
content.  See `meep--surround-add-impl'."
  (let ((text-initial "(abc")
        (text-expected "((abc)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-mark 1) ; Left edge before the leading `('.
      (goto-char 5) ; Right edge after `abc'.
      (simulate-input-for-meep
        "t(")
      (should (equal text-expected (buffer-string)))
      ;; Just inside the inserted outer pair, not on the delimiters.
      (should (eq 2 (mark t)))
      (should (eq 6 (point))))))

(ert-deftest surround-add-lines-trims-blank-space ()
  "Line-wise add wraps each line's content, not its surrounding blank-space.
Pins the deliberate trim: leading and trailing whitespace stay outside the
delimiters and an all-blank line is left unwrapped (an intended change from the
old full-line wrap, which included the indentation).  See
`meep--surround-add-impl'."
  (let ((text-initial "  foo  \n   \n  bar")
        (text-expected "  (foo)  \n   \n  (bar)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-mark (point-min))
      (goto-char (point-max))
      (simulate-input-for-meep
        "T(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-lines-active-region-marks-inside ()
  "Line-wise add over an active region steps the left edge just inside the open.
The complement of `surround-add-lines-region-edge-before-indent' (indented, where
the edge stays put): with no indentation the prefix goes in at the edge, so the
edge steps inside the inserted open."
  (let ((text-initial "foo\nbar")
        (text-expected "(foo)\n(bar)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-mark 1)
      (goto-char (point-max))
      (simulate-input-for-meep
        "T(")
      (should (equal text-expected (buffer-string)))
      (should (eq 2 (mark t))))))

(ert-deftest surround-add-rectangle-keeps-multi-row-selection ()
  "Rectangle add wraps each row's column span; left corner steps inside the open.
The left corner (mark, top-left) advances past the inserted open on its row;
the right corner (point) stays on its original row.  See `meep--surround-add-impl'."
  (let ((text-initial "abcd\nefgh\nijkl")
        (text-expected "a(bc)d\ne(fg)h\ni(jk)l"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2) ; Row 1, column 1 (before "b").
      (set-mark 2)
      (rectangle-mark-mode 1)
      (goto-char 14) ; Row 3, column 3 (before "l").
      (simulate-input-for-meep
        "t(")
      (should (equal text-expected (buffer-string)))
      ;; Corners stay on their respective rows.
      (should (eq 1 (line-number-at-pos (mark t))))
      (should (eq 3 (line-number-at-pos (point)))))))

(ert-deftest surround-add-rectangle-single-row-adjusts-left-edge ()
  "Rectangle surround-add steps the left corner inside the open on a single row.
Validated by inserting a marker character at each edge; the result encodes both
positions in the expected text.
Regression: the rectangle path skipped the left-edge adjustment, leaving
point before the inserted open.  See `meep--surround-add-impl'."
  (let ((text-initial "abcde")
        (text-expected "a(|bc|)de"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        "l2" ; Move to column 3 (before "d"): one step, then repeat x2.
        "sd" ; Activate rectangle mark; anchor at column 3.
        "hh" ; Move left to column 1 (before "b"); point becomes left corner.
        "t(" ; Surround: "bc" -> "(bc)"; point steps inside "(".
        "G|" ; Insert "|" at point (just inside "(").
        "ud" ; Activate region, swap point/mark, deactivate: now at right edge.
        "G|") ; Insert "|" at right edge (just before ")").
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-rectangle-multiple-rows-adjusts-left-edge ()
  "Rectangle surround-add over 3 rows steps the top-left corner inside the open.
Validated by inserting a marker character at each edge; the result encodes both
positions in the expected text.
See `meep--surround-add-impl'."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "abcd\n"
          "efgh\n"
          "ijkl"))
        (text-expected
         ;; format-next-line: off
         (concat
          "a(|bc)d\n"
          "e(fg)h\n"
          "i(jk|)l")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        "jj" ; Move to row 3, col 0 (before "i").
        "l2" ; Move to col 3 (before "l"): one step, then repeat x2.
        "sd" ; Activate rectangle mark; anchor at (row 3, col 3).
        "kk" ; Move up to row 1, col 3 (before "d").
        "hh" ; Move left to col 1 (before "b"); point becomes top-left corner.
        "t(" ; Surround: wraps bc/fg/jk; top-left steps inside "(" on row 1.
        "G|" ; Insert "|" at point (just inside "(" on row 1).
        "ud" ; Activate region, swap point/mark, deactivate: now on row 3.
        "G|") ; Insert "|" at right edge (just before ")" on row 3).
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-add-rectangle-skips-short-row ()
  "Rectangle add skips a row with no character in the column span.
A row too short to reach the rectangle's columns has an empty span, so it is left
unwrapped while the other rows wrap, see `meep--surround-row-span'."
  (let ((text-initial "abcd\nx\nijkl")
        (text-expected "a(bc)d\nx\ni(jk)l"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2) ; Row 1, column 1 (before "b").
      (set-mark 2)
      (rectangle-mark-mode 1)
      (goto-char 11) ; Row 3, column 3 (before "l").
      (simulate-input-for-meep
        "t(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-innermost ()
  "Delete removes the innermost enclosing pair around point."
  (let ((text-initial "[{foo}]")
        (text-expected "[foo]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-syntax-skips-string ()
  "Under the syntax backend, surround delete skips a string's quotes.

In a `prog-mode' buffer the backend auto-resolves to `syntax', which recognizes
only syntax-table brackets - so point inside a string finds the enclosing
bracket, not the quotes."
  (let ((text-initial "(foo \"bar baz\")")
        (text-expected "foo \"bar baz\""))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      (goto-char 7) ;; inside the string, on `bar'
      (simulate-input-for-meep
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-text-takes-string-quotes ()
  "Under the text backend, surround delete in a string takes the quotes.

The text backend recognizes quotes, so point inside the string deletes the
quote pair - the contrast to `surround-delete-syntax-skips-string'."
  (let ((text-initial "(foo \"bar baz\")")
        (text-expected "(foo bar baz)"))
    (with-meep-test text-initial
      (emacs-lisp-mode)
      (bray-mode 1)
      (goto-char 7)
      (let ((meep-syntax-backend 'text))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-replace-innermost ()
  "Replace swaps the innermost enclosing pair."
  (let ((text-initial "[{foo}]")
        (text-expected "[(foo)]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "tg(")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-replace-multi-char ()
  "Replace swaps the surrounding pair for multi-character delimiters."
  (let ((text-initial "(foo)")
        (text-expected "**foo**"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 3) ; Inside "foo".
      (setq-local meep-surround-pairs '((bold . ("**" . "**"))))
      (simulate-input-for-meep
        "tg*")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-count-nth ()
  "A count deletes the Nth enclosing pair, leaving the inner one intact."
  (let ((text-initial "[{foo}]")
        (text-expected "{foo}"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        [?\C-u ?2]
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-count-past-depth-clamps ()
  "A count past the nesting depth deletes the outermost pair, not nothing."
  (let ((text-initial "[{x}]")
        (text-expected "{x}"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 3)
      (simulate-input-for-meep
        [?\C-u ?9]
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-replace-count-past-depth-clamps ()
  "A count past the nesting depth changes the outermost pair, not nothing."
  (let ((text-initial "[{x}]")
        (text-expected "<{x}>"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 3)
      (simulate-input-for-meep
        [?\C-u ?9]
        "tg<")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-count-clamped-to-one ()
  "A zero count peels one layer for both line-wise and region delete."
  (let ((text-initial "(a)")
        (text-expected "a"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        [?\C-u ?0]
        "TT")
      (should (equal text-expected (buffer-string)))))
  (let ((text-initial "(a)")
        (text-expected "a"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2)
      (simulate-input-for-meep
        [?\C-u ?0]
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-edges-strip-selection ()
  "Delete strips a selection that already includes the delimiters."
  (let ((text-initial "(foo)")
        (text-expected "foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-edges-strip-outermost ()
  "Edges-first strips the outer pair when the whole nested span is selected.
A selection of `[{foo}]' has the outer brackets at its own edges, so they are the
pair stripped and the inner `{}' is kept - the contrast to the point-anchored
`surround-delete-innermost', which takes the inner pair."
  (let ((text-initial "[{foo}]")
        (text-expected "{foo}"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-sets-mark-to-opposite-end ()
  "Delete leaves point and mark spanning the unwrapped content, region inactive.
Point lands just inside the former open, the mark (inactive) just inside the
former close, so the content is reachable as a region without re-selecting."
  (let ((text-initial "[{foo}]")
        (text-expected "[foo]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (let ((meep-surround-mark-result t))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string)))
      (should (eq (point) 2))
      (should (eq (mark t) 5))
      (should-not (region-active-p))
      (should (equal (buffer-substring (point) (mark t)) "foo")))))

(ert-deftest surround-replace-sets-mark-to-opposite-end ()
  "Replace leaves point and mark spanning the content between the new delimiters."
  (let ((text-initial "[{foo}]")
        (text-expected "[(foo)]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (let ((meep-surround-mark-result t))
        (simulate-input-for-meep
          "tg("))
      (should (equal text-expected (buffer-string)))
      (should (eq (point) 3))
      (should (eq (mark t) 6))
      (should (equal (buffer-substring (point) (mark t)) "foo")))))

(ert-deftest surround-delete-mark-result-nil-keeps-point ()
  "With `meep-surround-mark-result' nil, delete leaves point put and sets no mark."
  (let ((text-initial "[{foo}]")
        (text-expected "[foo]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-marker (mark-marker) nil)
      (goto-char 4) ; First `o' of `foo'.
      (let ((meep-surround-mark-result nil))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string)))
      ;; Same `o', shifted left by the removed opening delimiter.
      (should (eq (point) 3))
      (should (eq (char-after) ?o))
      (should (null (mark t))))))

(ert-deftest surround-delete-lines-leaves-mark-unset ()
  "Line-wise delete does not leak a stray mark, unlike the region path."
  (let ((text-initial "(foo)")
        (text-expected "foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (set-marker (mark-marker) nil)
      (goto-char 3)
      (should (null (mark t)))
      (let ((meep-surround-mark-result t))
        (simulate-input-for-meep
          "TT"))
      (should (equal text-expected (buffer-string)))
      (should (null (mark t))))))

(ert-deftest surround-delete-multi-char ()
  "Delete removes multi-character delimiters."
  (let ((text-initial "**foo**")
        (text-expected "foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (let ((meep-match-bounds-of-char-contextual '(("**" . "**"))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-multi-char-not-split-by-single ()
  "With both `**' and `*' recognized, point on a `**' token deletes the whole pair.
The shorter `*' must not split the `**' token into two single delimiters, whether
point sits on the opening or the closing `**'."
  ;; Point on the opening `**'.
  (let ((text-initial "**foo**")
        (text-expected "foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 1)
      (let ((meep-match-bounds-of-char-contextual '(("**" . "**") ("*" . "*"))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string)))))
  ;; Point on the closing `**'.
  (let ((text-initial "**foo**")
        (text-expected "foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 6)
      (let ((meep-match-bounds-of-char-contextual '(("**" . "**") ("*" . "*"))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-distinct-bracket-case-sensitive ()
  "Distinct multi-char delimiters match case-sensitively, ignoring `case-fold-search'.
A differing-case token (here `<B>') is not the configured `<b>' delimiter, so the
valid `<b>...</b>' pair is still found - both via the outward finder and the
edges-strip path."
  ;; Outward finder: point inside the lowercase pair.
  (let ((text-initial "<b>x<B>y</b>")
        (text-expected "x<B>y"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4) ; On "x", inside the lowercase pair.
      (let ((meep-match-bounds-of-char-contextual '(("<b>" . "</b>")))
            (case-fold-search t))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string)))))
  ;; Edges strip: a whole-pair selection is balanced only when the inner `<B>'
  ;; is not counted as an opening `<b>'.
  (let ((text-initial "<b>x<B>y</b>")
        (text-expected "x<B>y"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (let ((meep-match-bounds-of-char-contextual '(("<b>" . "</b>")))
            (case-fold-search t))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-same-delimiter-across-lines ()
  "Delete strips the nearer quotes, not a wrong enclosing bracket."
  (let ((text-initial "foo(\"first\nbar\" \"second\")")
        (text-expected "foo(\"first\nbar\" second)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 20) ; Inside "second".
      (let ((meep-match-bounds-of-char-contextual '(("\"" . "\"") ("(" . ")"))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-same-delimiter-stray-on-earlier-line ()
  "Delete strips a single-line quote pair despite a stray quote on an earlier line."
  (let ((text-initial "(intro \" note\nthe \"word\" x)")
        (text-expected "(intro \" note\nthe word x)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 21) ; Inside "word".
      (let ((meep-match-bounds-of-char-contextual '(("\"" . "\"") ("(" . ")"))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-same-delimiter-stray-earlier-paragraph ()
  "Delete strips a multi-line span's quotes despite a stray quote in a prior paragraph.
The multi-line `first' span defeats the per-line opener parity, so the
paragraph-scope fall-back runs; counting from `point-min' would let the lone quote
in the first paragraph flip the parity and reject the real `second' pair, while
counting from the paragraph recovers it - a blank line breaks a same-delimiter
span."
  (let ((text-initial "x\"y\n\nfoo(\"first\nbar\" \"second\")")
        (text-expected "x\"y\n\nfoo(\"first\nbar\" second)"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 25) ; Inside "second".
      (let ((meep-match-bounds-of-char-contextual '(("\"" . "\"") ("(" . ")"))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-same-delimiter-cursor-on-open ()
  "Delete strips the pair with point on the opening quote, like point on the close."
  (let ((text-initial "a\"bc\"d")
        (text-expected "abcd"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 2) ; On the opening ".
      (let ((meep-match-bounds-of-char-contextual '(("\"" . "\""))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-same-delimiter-cursor-on-open-multiline ()
  "Point on an opening quote finds that quote's pair, not a prior token's span."
  (let ((text-initial "\"line one\ntwo\" plain \"three\"")
        (text-expected "\"line one\ntwo\" plain three"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 22) ; On the opening quote of "three".
      (let ((meep-match-bounds-of-char-contextual '(("\"" . "\""))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-same-delimiter-cursor-on-open-after-blank ()
  "Cursor-on-open does not pair across a blank line with a stray in a prior paragraph."
  (let ((text-initial "p's\n\n'XY' q")
        (text-expected "p's\n\nXY q"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 6) ; On the opening ' of 'XY'.
      (let ((meep-match-bounds-of-char-contextual '(("'" . "'"))))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-configured-bracket-without-syntax ()
  "Delete finds a configured bracket pair even when its open lacks paren syntax."
  (let ((text-initial "a<x>b")
        (text-expected "axb"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (with-syntax-table (make-syntax-table)
        (modify-syntax-entry ?< ".")
        (modify-syntax-entry ?> ".")
        (let ((meep-surround-pairs '((angle . ("<" . ">"))))
              (meep-match-bounds-of-char-contextual nil)
              (meep-symmetrical-chars nil))
          (goto-char 3) ; Inside the `<x>'.
          (simulate-input-for-meep
            "tt")
          (should (equal text-expected (buffer-string))))))))

(ert-deftest surround-delete-configured-bracket-under-syntax-backend ()
  "The syntax backend trusts an explicit non-paren pair from `meep-surround-pairs'.

The backend narrows only the generic fall-backs to paren-syntax brackets; an
explicit configured pair is exempt and located by the text backend, so a non-paren
`<' `>' still deletes - the contrast to `surround-delete-syntax-skips-string',
where the quote pair is a fall-back and so is dropped."
  (let ((text-initial "a<x>b")
        (text-expected "axb"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (with-syntax-table (make-syntax-table)
        (modify-syntax-entry ?< ".")
        (modify-syntax-entry ?> ".")
        (let ((meep-syntax-backend 'syntax)
              (meep-surround-pairs '((angle . ("<" . ">"))))
              (meep-match-bounds-of-char-contextual nil)
              (meep-symmetrical-chars nil))
          (goto-char 3) ; Inside the `<x>'.
          (simulate-input-for-meep
            "tt")
          (should (equal text-expected (buffer-string))))))))

(ert-deftest surround-delete-configured-quote-under-syntax-backend ()
  "The syntax backend trusts an explicit `meep-surround-pairs' quote.
The backend drops auto-detected quote fall-backs but exempts an explicit configured
quote, located by the text fall-back, so it still deletes - the quote counterpart of
`surround-delete-configured-bracket-under-syntax-backend'."
  (let ((text-initial "foo `bar` baz")
        (text-expected "foo bar baz"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (let ((meep-syntax-backend 'syntax)
            (meep-surround-pairs '((code . ("`" . "`"))))
            (meep-match-bounds-of-char-contextual nil)
            (meep-symmetrical-chars nil))
        (goto-char 7) ; Inside `bar`.
        (simulate-input-for-meep
          "tt")
        (should (equal text-expected (buffer-string)))))))

(ert-deftest surround-delete-syntax-drops-markup-fallback ()
  "A generic markup fall-back deletes under `text' but is dropped under `syntax'.
The backend narrows generic fall-backs to paren brackets; an auto-detected markup
pair - not an explicit `meep-surround-pairs' entry, which would be trusted - is
dropped under `syntax', so delete skips it.  Same fixture and point, only the
backend differs, see `meep--surround-fallback-recognizable-p'."
  ;; Text backend: the markup fall-back is recognized and deletes.
  (let ((text-initial "<b>foo</b>")
        (text-expected "foo"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (let ((meep-syntax-backend 'text)
            (meep-match-bounds-of-char-contextual '(("<b>" . "</b>")))
            (meep-symmetrical-chars nil))
        (goto-char 5) ; Inside "foo".
        (simulate-input-for-meep
          "tt")
        (should (equal text-expected (buffer-string))))))
  ;; Syntax backend: the same fall-back is dropped, so delete finds nothing.
  (let ((text-initial "<b>foo</b>"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (let ((meep-syntax-backend 'syntax)
            (meep-match-bounds-of-char-contextual '(("<b>" . "</b>")))
            (meep-symmetrical-chars nil))
        (goto-char 5) ; Inside "foo".
        (simulate-input-for-meep
          "tt")
        (should (equal text-initial (buffer-string)))))))

(ert-deftest surround-delete-operator-bracket-tracks-syntax ()
  "An operator `<' `>' is unrecognized for delete until given paren syntax.
The fall-back drops a single-char bracket lacking paren syntax, so delete finds
nothing; promoting it to paren syntax is picked up on the next delete because the
recognition memo carries a syntax snapshot, see `meep--surround-recognition-pairs'."
  (with-meep-test "a<x>b"
    (text-mode)
    (bray-mode 1)
    (with-syntax-table (make-syntax-table)
      (let ((meep-match-bounds-of-char-contextual nil)
            (meep-symmetrical-chars '(("<" . ">")))
            (meep-syntax-backend 'text))
        ;; `<' `>' as punctuation: not a recognized bracket, so delete finds nothing.
        (modify-syntax-entry ?< ".")
        (modify-syntax-entry ?> ".")
        (goto-char 3) ; Inside `<x>'.
        (simulate-input-for-meep
          "tt")
        (should (equal "a<x>b" (buffer-string)))
        ;; Promote `<' `>' to paren syntax: the next delete recomputes and strips them.
        (modify-syntax-entry ?< "(>")
        (modify-syntax-entry ?> ")<")
        (goto-char 3)
        (simulate-input-for-meep
          "tt")
        (should (equal "axb" (buffer-string)))))))

(ert-deftest surround-delete-ignores-function-spec-pair ()
  "A function-valued surround pair is not auto-recognized for delete.
A function spec yields delimiters only when its key is pressed (add); it is skipped
from the recognition set, so delete does not strip it, see
`meep--surround-recognition-pairs'."
  (with-meep-test "<x>foo</x>"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-match-bounds-of-char-contextual nil)
    (setq-local meep-symmetrical-chars nil)
    (setq-local meep-surround-pairs (list (cons 'tag (lambda () (cons "<x>" "</x>")))))
    (goto-char 5) ; Inside "foo".
    (simulate-input-for-meep
      "tt")
    (should (equal "<x>foo</x>" (buffer-string)))))

(ert-deftest surround-delete-ignores-malformed-literal-pair ()
  "A malformed `(OPEN . nil)' literal pair is filtered from recognition, not used.
A forgotten close would hand the finder a nil delimiter; the recognition filter
drops the invalid pair, so delete finds nothing rather than crashing, see
`meep--surround-recognition-pairs'."
  (with-meep-test "axb"
    (text-mode)
    (bray-mode 1)
    (setq-local meep-match-bounds-of-char-contextual nil)
    (setq-local meep-symmetrical-chars nil)
    (setq-local meep-surround-pairs '((bad . ("x" . nil))))
    (goto-char 2) ; On `x'.
    (simulate-input-for-meep
      "tt")
    (should (equal "axb" (buffer-string)))))

(ert-deftest surround-delete-no-pair-unchanged ()
  "Delete with no surrounding pair reports it and leaves the buffer unchanged."
  (let ((text-initial "foo bar"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "tt")
      (should (member "No surrounding pair found" (meep-test-messages)))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-delete-lines-no-pair-unchanged ()
  "Line-wise delete with no surrounding pair reports it, like the region path.
The per-line path tracks its own edits rather than routing through
`meep--surround-operate-apply', so it must emit the same no-match message."
  (let ((text-initial "foo bar"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (simulate-input-for-meep
        "TT")
      (should (member "No surrounding pair found" (meep-test-messages)))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-delete-single-undo ()
  "A delete is a single undo step (both delimiters restored at once)."
  (let ((text-initial "[{foo}]")
        (text-expected "{foo}"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 4)
      (undo-boundary)
      (simulate-input-for-meep
        [?\C-u ?2]
        "tt")
      (should (equal text-expected (buffer-string)))
      (primitive-undo 1 buffer-undo-list)
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-delete-lines-skips-unwrapped ()
  "Line-wise delete strips each wrapped line and skips the rest."
  (let ((text-initial "(a)\nbc\n(d)")
        (text-expected "a\nbc\nd"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        "TT")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-lines-contracts-indent ()
  "Line-wise delete leaves indentation outside the delimiters."
  (let ((text-initial "  (a)\n  (b)")
        (text-expected "  a\n  b"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        "TT")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-lines-peels-layers ()
  "Line-wise count peels that many outer layers."
  (let ((text-initial "[{x}]")
        (text-expected "x"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        [?\C-u ?2]
        "TT")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-lines-peels-spaced-layers ()
  "Line-wise count peels nested layers even when padded by spaces."
  (let ((text-initial "[ {x} ]")
        (text-expected " x "))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        [?\C-u ?2]
        "TT")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-lines-peels-fewer-than-count ()
  "Line-wise peel stops at the available layers when count exceeds the depth."
  (let ((text-initial "(foo)\n(bar)")
        (text-expected "foo\nbar"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        [?\C-u ?2]
        "TT")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-replace-lines ()
  "Line-wise replace swaps each wrapped line's outermost pair."
  (let ((text-initial "(a)\n(b)")
        (text-expected "[a]\n[b]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        "Tg[")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-lines-no-pair-keeps-region ()
  "A line-wise delete that edits nothing leaves the selection active."
  (let ((text-initial "foo\nbar"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        "TT")
      (should (region-active-p))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-delete-lines-applied-deactivates-region ()
  "A line-wise delete that edits at least one line drops the selection."
  (let ((text-initial "(a)\nbc")
        (text-expected "a\nbc"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle))
      (goto-char (point-max))
      (simulate-input-for-meep
        "TT")
      (should-not (region-active-p))
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-rectangle ()
  "Rectangle delete strips the pair in each row's column span."
  (let ((text-initial "(a)\n(b)\n(c)")
        (text-expected "a\nb\nc"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (set-mark (point-min))
      (rectangle-mark-mode 1)
      (goto-char (point-max))
      (simulate-input-for-meep
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-rectangle-mark-result-no-stray-mark ()
  "Rectangle delete ignores `meep-surround-mark-result' (many rows, no single span).
With the option on, the bug left point inside the first row's content; the
multi-target path now restores point instead of marking only the first row."
  (let ((text-initial "(a)\n(b)\n(c)")
        (text-expected "a\nb\nc"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (set-mark (point-min))
      (rectangle-mark-mode 1)
      (goto-char (point-max))
      (let ((meep-surround-mark-result t))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string)))
      ;; Point is restored to the last row, not jumped into the first row's content.
      (should (eq (line-number-at-pos (point)) 3)))))

(ert-deftest surround-delete-rectangle-single-row-mark-result-inert ()
  "A single-row rectangle delete ignores `meep-surround-mark-result'.
A one-row, one-pair rectangle collects a single target - the same shape the
region path marks - but the rectangle path leaves the mark to the region verbs.
The bug entered the mark branch here (no `save-mark-and-excursion' shields it),
jumping point to the content start and setting the mark; point is now restored."
  (let ((text-initial "(ab)")
        (text-expected "ab"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (set-mark (point-min))
      (rectangle-mark-mode 1)
      (goto-char (point-max))
      (let ((meep-surround-mark-result t))
        (simulate-input-for-meep
          "tt"))
      (should (equal text-expected (buffer-string)))
      ;; Point restored to the user's position, not jumped to the content start.
      (should (eq (point) 3))
      ;; No feature-set mark at the content end (the bug left the mark at 3).
      (should-not (eq (mark t) 3)))))

(ert-deftest surround-delete-rectangle-row-anchored-on-open ()
  "A row whose column span starts on the open delimiter is edited, not skipped."
  (let ((text-initial "(aa)\n(b)")
        (text-expected "aa\nb"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (set-mark (point-min))
      (rectangle-mark-mode 1)
      (goto-char 8) ; Line 2, on the close `)' (column 2).
      (simulate-input-for-meep
        "tt")
      (should (equal text-expected (buffer-string))))))

(ert-deftest surround-delete-rectangle-pair-outside-span-skipped ()
  "A rectangle delete skips a pair whose open lies outside the column span."
  (let ((text-initial "x(ab)y"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char 3) ; Column 2, on "a".
      (set-mark 3)
      (rectangle-mark-mode 1)
      (goto-char 5) ; Column 4, on ")"; the rectangle covers only "ab".
      (simulate-input-for-meep
        "tt")
      (should (member "No surrounding pair found" (meep-test-messages)))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-delete-rectangle-no-pair-keeps-region ()
  "A rectangle delete that edits nothing leaves the selection active."
  (let ((text-initial "foo\nbar"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (set-mark (point-min))
      (rectangle-mark-mode 1)
      (goto-char (point-max))
      (simulate-input-for-meep
        "tt")
      (should (member "No surrounding pair found" (meep-test-messages)))
      (should (region-active-p))
      (should (equal text-initial (buffer-string))))))

(ert-deftest surround-replace-rectangle ()
  "Rectangle replace swaps the pair in each row's column span."
  (let ((text-initial "(a)\n(b)")
        (text-expected "[a]\n[b]"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (goto-char (point-min))
      (set-mark (point-min))
      (rectangle-mark-mode 1)
      (goto-char (point-max))
      (simulate-input-for-meep
        "tg[")
      (should (equal text-expected (buffer-string))))))

(ert-deftest message-capture-basic ()
  "Verify message capture is working.

Messages should be captured and not displayed."
  (with-meep-test "hello"
    (text-mode)
    (bray-mode 1)
    ;; Verify no messages captured yet.
    (should (equal '() (meep-test-messages)))
    ;; Generate some messages.
    (message "Test message 1")
    (message "Test message 2: %s" "arg")
    ;; Verify messages were captured in order.
    (should (equal '("Test message 1" "Test message 2: arg") (meep-test-messages)))))

(ert-deftest should-error-with-message-basic ()
  "Verify should-error-with-message validates error messages."
  ;; Test with matching message.
  (should-error-with-message
      (error "Expected error message")
    'error
    "Expected error message")
  ;; Test with user-error type.
  (should-error-with-message
      (user-error "User error message")
    'user-error
    "User error message"))

(ert-deftest indent-rigidly-shift-left-with-existing-indent ()
  "Shift an indented line further left using `meep-indent-rigidly'.

Verifies: `meep-indent-rigidly' sets up the current line as the region
and `indent-rigidly' shifts it left when existing indentation is present."
  (let ((text-initial (concat "first\n" "        second"))
        ;; See `inhibit-redisplay' note in this file.
        (inhibit-redisplay t))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to line 2.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      ;; Cursor on "        second" (8 spaces).
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      ;; Shift left by 4 via TAB (meep-indent-rigidly).
      ;; C-a exits the transient keymap (not in `indent-rigidly-map'),
      ;; a NOP (globally unset in the test environment).
      (simulate-input
        [?\t left left left left ?\C-a])
      (should (equal "first\n    second" (buffer-string)))
      (should (equal '(2 . 0) (meep-test-point-line-column))))))

(ert-deftest indent-rigidly-shift-line ()
  "Shift a line right and left using `meep-indent-rigidly'.

Verifies: right and left keys shift indentation
via the transient keymap entered by `indent-rigidly'.
Note: S-right/S-left are not testable in batch mode."
  (let ((text-initial "hello")
        ;; See `inhibit-redisplay' note in this file.
        (inhibit-redisplay t))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Right by 3.
      ;; C-a exits the transient keymap (not in `indent-rigidly-map'),
      ;; a NOP (globally unset in the test environment).
      (simulate-input
        [?\t right right right ?\C-a])
      (should (equal "   hello" (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column)))
      ;; Right by 3 more (6 total).
      (simulate-input
        [?\t right right right ?\C-a])
      (should (equal "      hello" (buffer-string)))
      ;; Left by 3 (3 remaining).
      (simulate-input
        [?\t left left left ?\C-a])
      (should (equal "   hello" (buffer-string)))
      ;; Left by 3 (back to 0).
      (simulate-input
        [?\t left left left ?\C-a])
      (should (equal "hello" (buffer-string)))
      (should (equal '(1 . 0) (meep-test-point-line-column))))))

(ert-deftest indent-rigidly-shift-region ()
  "Shift a multi-line region right using `meep-indent-rigidly'.

Verifies: `meep-indent-rigidly' passes through to `indent-rigidly'
when a region is already active."
  (let ((text-initial "aaa\nbbb\nccc")
        ;; See `inhibit-redisplay' note in this file.
        (inhibit-redisplay t))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select lines 1-2 (move into line 2 so indent-rigidly includes it).
      (simulate-input-for-meep
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-move-char-next))
      (should (region-active-p))
      ;; Shift right by 3.
      ;; C-a exits the transient keymap (not in `indent-rigidly-map'),
      ;; a NOP (globally unset in the test environment).
      (simulate-input
        [?\t right right right ?\C-a])
      (should (equal "   aaa\n   bbb\nccc" (buffer-string))))))

(ert-deftest indent-rigidly-error-empty-line ()
  "Error when `meep-indent-rigidly' is called on an empty line."
  (let ((text-initial "hello\n\nworld"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to the empty line 2.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      (should (equal '(2 . 0) (meep-test-point-line-column)))
      (should-error-with-message
          (simulate-input
            [?\t])
        'user-error
        "The line is empty"))))

(ert-deftest indent-rigidly-shift-line-mid-column ()
  "Shift a line right when the cursor is mid-line.

Verifies: `meep-indent-rigidly' sets the marker to bol
when point is not at the beginning of the line."
  (let ((text-initial "hello")
        ;; See `inhibit-redisplay' note in this file.
        (inhibit-redisplay t))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move cursor to mid-line.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      (should (equal '(1 . 2) (meep-test-point-line-column)))
      ;; Shift right by 3.
      ;; C-a exits the transient keymap (not in `indent-rigidly-map'),
      ;; a NOP (globally unset in the test environment).
      (simulate-input
        [?\t right right right ?\C-a])
      (should (equal "   hello" (buffer-string)))
      ;; Column 2 + 3 spaces inserted before point = column 5.
      (should (equal '(1 . 5) (meep-test-point-line-column))))))

(ert-deftest digit-argument-repeat-basic ()
  "Repeat the last command using `meep-digit-argument-repeat'.

Move right once, then press 5 to repeat the motion 5 more times."
  (let ((text-initial "abcdefghij"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move right once (cursor on 'b'), then press 5 to repeat 5 times (cursor on 'g').
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        "5")
      (should (equal '(1 . 6) (meep-test-point-line-column))))))

(ert-deftest digit-argument-repeat-multiple ()
  "Repeat the last command multiple times using `meep-digit-argument-repeat'.

Move right once, then press 3 three times to repeat the motion 3+3+3 more times."
  (let ((text-initial "abcdefghijklmnop"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move right once (cursor on 'b'), then press 3 three times (cursor on 'k').
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        "3"
        "3"
        "3")
      (should (equal '(1 . 10) (meep-test-point-line-column))))))

(ert-deftest repeat-fu-upcase-region ()
  "Repeat upcase-region on a visual selection using repeat-fu.

Select 3 middle words, upcase them, then move to the next line
and repeat the action with repeat-fu-execute."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial
         ;; format-next-line: off
         (concat
          "alpha beta gamma delta epsilon\n"
          "alpha beta gamma delta epsilon\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "alpha BETA GAMMA DELTA epsilon\n"
          "alpha BETA GAMMA DELTA epsilon\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Select and upcase "beta gamma delta" on line 1,
      ;; then navigate to "beta" on line 2.
      ;; NOTE: all commands must be in a single `simulate-input-for-meep' call
      ;; because splitting across two calls inserts a spurious nil-command entry
      ;; into the repeat-fu command buffer at the `execute-kbd-macro' boundary,
      ;; which breaks macro extraction.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next)
        '(:state normal :command meep-region-toggle)
        '(:state visual :command meep-move-word-next)
        '(:state visual :command meep-move-word-next)
        '(:state visual :command meep-move-word-next)
        '(:state visual :command upcase-region)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-word-prev)
        '(:state normal :command meep-move-word-prev)
        '(:state normal :command meep-move-word-prev))
      ;; Repeat the upcase action with repeat-fu.
      ;; Called directly because repeat-fu internally uses `execute-kbd-macro'
      ;; which cannot be nested inside `simulate-input-for-meep'.
      (repeat-fu-execute 1)
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest repeat-fu-upcase-region-rectangle ()
  "Repeat a rectangle upcase-region using repeat-fu.

Select the middle 3 words as a rectangle on lines 1-2, upcase them,
then move to line 3 and repeat the action on lines 3-4.
Each line uses unique words so the test proves only the rectangle
columns are affected, not entire lines."
  (require 'repeat-fu-preset-meep)
  (let ((repeat-fu-backend (repeat-fu-preset-meep))
        (text-initial
         ;; format-next-line: off
         (concat
          "apple berry grape dusty ember\n"
          "acorn birch guava drift elbow\n"
          "arbor bunch gourd ditto eager\n"
          "adept bumpy grain dunce elope\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "apple BERRY GRAPE DUSTY ember\n"
          "acorn BIRCH GUAVA DRIFT elbow\n"
          "arbor BUNCH GOURD DITTO eager\n"
          "adept BUMPY GRAIN DUNCE elope\n")))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to "berry" on line 1, enable meep-region-toggle-rectangle,
      ;; extend to cover columns 6-23 on lines 1-2, then upcase.
      ;; Then navigate to the second word on line 3.
      ;; NOTE: all commands must be in a single `simulate-input-for-meep' call
      ;; because splitting across two calls inserts a spurious nil-command entry
      ;; into the repeat-fu command buffer at the `execute-kbd-macro' boundary,
      ;; which breaks macro extraction.
      (simulate-input-for-meep
        '(:state normal :command meep-move-word-next)
        '(:state normal :command meep-region-toggle-rectangle)
        '(:state visual :command meep-move-word-next)
        '(:state visual :command meep-move-word-next)
        '(:state visual :command meep-move-word-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command upcase-region)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command meep-move-word-prev)
        '(:state normal :command meep-move-word-prev)
        '(:state normal :command meep-move-word-prev))
      ;; Verify only lines 1-2 were affected and lines 3-4 are unchanged.
      (should (equal 'normal (bray-state)))
      (should
       (equal
        ;; format-next-line: off
        (concat
         "apple BERRY GRAPE DUSTY ember\n"
         "acorn BIRCH GUAVA DRIFT elbow\n"
         "arbor bunch gourd ditto eager\n"
         "adept bumpy grain dunce elope\n")
        (buffer-string)))
      ;; Repeat the rectangle upcase on lines 3-4.
      ;; Called directly because repeat-fu internally uses `execute-kbd-macro'
      ;; which cannot be nested inside `simulate-input-for-meep'.
      (repeat-fu-execute 1)
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

(ert-deftest selection-mark-bounds-of-char-outer-region-straddles-close ()
  "Outer-bracket selection must contain the full active region.

Regression: the containment check used (car bounds-init) for both the
open and close bracket tests, so a bracket pair whose close fell inside
the selection (but after the selection start) was returned as valid outer
bounds even though it did not contain the selection end.

Setup: select inner `foo` of `(foo) bar', swap so point is at the end,
extend two chars to include `) ' - the region now straddles the closing
paren.  Requesting outer `(' must fail (no enclosing pair exists) rather
than returning the mismatched inner pair."
  (let ((text-initial "(foo) bar"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move one step right so cursor is inside the parens on `f'.
      ;; Cursor: (foo) bar
      ;;          ^
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next))
      ;; Select inner content of `(' - region becomes `foo'.
      ;; point=col1, mark=col4 (is-forward nil, so point at start).
      (simulate-input-for-meep
        '(:state normal :command meep-region-mark-bounds-of-char-inner)
        "(")
      (should (equal "foo" (meep-test-region-as-string)))
      ;; Swap so point is at the far end, enabling forward extension.
      (simulate-input-for-meep
        '(:state visual :command meep-region-activate-and-reverse))
      ;; Extend two chars forward: past `)' and into the space.
      ;; Region is now `foo) ' - close-bracket is inside the selection.
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next))
      (should (equal "foo) " (meep-test-region-as-string)))
      ;; Attempt outer `(' - no enclosing pair exists.
      ;; With the bug: returns `(foo)' which drops the ` ' from the end.
      ;; With the fix: leaves the region unchanged and emits a message.
      (simulate-input-for-meep
        '(:state visual :command meep-region-mark-bounds-of-char-outer)
        "(")
      (should (equal "foo) " (meep-test-region-as-string))))))

(ert-deftest selection-expand-to-line-bounds-initial-no-trailing-newline ()
  "Select current line when on last line of buffer with no trailing newline.

Regression: goto-char was called with (1+ (pos-eol)) which equals
(1+ (point-max)) on the last line without a trailing newline, raising
an out-of-range error."
  (let ((text-initial "hello")) ; No trailing newline.
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      (simulate-input-for-meep
        '(:state normal :command meep-region-expand-to-line-bounds))
      (should (equal 'visual (bray-state)))
      (should (equal '(1 . 5) (meep-test-point-line-column)))
      (should (equal '(1 . 0) (meep-test-mark-line-column)))
      (should (equal "hello" (meep-test-region-as-string))))))

(provide 'meep_tests)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; meep_tests.el ends here
