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
         (orig-read-char-from-minibuffer (symbol-function 'read-char-from-minibuffer)))
     (unwind-protect
         (progn
           (fset 'message #'meep-test--message-capture)
           (fset 'read-char #'meep-test--read-char-no-prompt)
           (fset 'read-char-from-minibuffer #'meep-test--read-char-from-minibuffer-no-prompt)
           ,@body)
       (fset 'message orig-message)
       (fset 'read-char orig-read-char)
       (fset 'read-char-from-minibuffer orig-read-char-from-minibuffer))))

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

;; Key bindings organized by state.
;; Commands are looked up dynamically from bray's state keymaps.

(defvar meep-test-state-commands
  '((normal
     .
     (meep-char-insert
      meep-char-replace
      meep-char-surround-insert
      meep-char-surround-insert-lines
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
      rectangle-mark-mode))
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
      meep-char-surround-insert
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
      rectangle-mark-mode)))
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
    ;; Region commands.
    (meep-region-activate-and-reverse . (normal visual))
    (meep-region-disable . (visual)))
  "Commands needing test bindings, with list of states for each.
Each entry is (COMMAND . STATES) where STATES is a list of state symbols.
Commands get the same key in all specified states for consistency.")

(defun meep-test-generate-key (index)
  "Generate a unique key string for INDEX.
Uses function keys f5-f12 with modifier combinations.
Provides 72 unique keys (9 modifiers × 8 function keys)."
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
        '(:state normal :command rectangle-mark-mode)
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-char-replace)
        "#")
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
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-line-next)
        '(:state normal :command rectangle-mark-mode)
        '(:state visual :command meep-move-char-next)
        "3"
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-insert-change)
        "####"
        [return])
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string))))))

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
        '(:state normal :command rectangle-mark-mode)
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
        '(:state normal :command rectangle-mark-mode)
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
        '(:state normal :command rectangle-mark-mode)
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
        '(:state normal :command rectangle-mark-mode)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-yank)
        '(:state normal :command meep-delete-char-ring-yank)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-prev)
        '(:state normal :command meep-delete-char-ring-prev)
        '(:state normal :command meep-delete-char-ring-prev))
      ;; Cursor: [] ()
      ;;          ^
      ;; Move past ']', ' ', '(' to inside parens, yank all three.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-delete-char-ring-yank)
        '(:state normal :command meep-delete-char-ring-yank)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
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
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
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
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
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
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
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
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
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
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
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
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-move-line-next)
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
        '(:state normal :command rectangle-mark-mode))
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select [WX]/[YZ]/[UV] rectangle (3 lines).
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command rectangle-mark-mode))
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
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
        '(:state normal :command rectangle-mark-mode))
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-line-next)
        '(:state visual :command meep-region-to-secondary-selection))
      ;; Select BCD]/FGH] rectangle (columns 2-5), overlapping at columns 2-4.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-prev)
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command rectangle-mark-mode))
      (simulate-input-for-meep
        '(:state visual :command meep-move-char-next)
        '(:state visual :command meep-move-char-next)
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
        '(:state normal :command rectangle-mark-mode))
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
      ;; Position on 'C'.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next))
      ;; Cursor: ABC
      ;;           ^
      ;; Move backward one char and transpose in same block.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-prev)
        '(:state normal :command meep-transpose))
      ;; Result: ACB (B and C swapped)
      ;;          ^
      (should (equal text-expected (buffer-string)))
      (should (equal 'normal (bray-state)))
      (should (equal point-expected (meep-test-point-line-column))))))

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
      ;; Move to line 2.
      (simulate-input-for-meep
        '(:state normal :command meep-move-line-next))
      ;; Cursor line 2: BBB
      ;;                ^
      ;; Move up one line and transpose in same block.
      (simulate-input-for-meep
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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

(ert-deftest selection-mark-bounds-of-char-empty-content ()
  "Mark bounds with empty content inside delimiters.

Verifies: handles () with nothing inside."
  (let ((text-initial "a0 () b1"))
    (with-meep-test text-initial
      (text-mode)
      (bray-mode 1)
      ;; Move to between the parens.
      (simulate-input-for-meep
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
        '(:state normal :command meep-move-char-next)
        '(:state normal :command meep-move-char-next)
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
      ;; Surround with parens.
      (simulate-input-for-meep
        '(:state visual :command meep-char-surround-insert)
        "(")
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
      ;; Surround with double quotes.
      (simulate-input-for-meep
        '(:state visual :command meep-char-surround-insert)
        "\"")
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
      ;; Surround with brackets.
      (simulate-input-for-meep
        '(:state visual :command meep-char-surround-insert)
        "[")
      (should (equal 'normal (bray-state)))
      (should (equal "[hello ]world" (buffer-string))))))


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
;; Bounds: Comment

(ert-deftest bounds-of-comment-inner-basic ()
  "Select comment content without delimiters.

Verifies: inner selection excludes comment markers."
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

(ert-deftest char-surround-insert-lines-basic ()
  "Surround current line with character pair.

Verifies: char-surround-insert-lines wraps line with matching delimiters."
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
      ;; Surround current line with parens.
      (simulate-input-for-meep
        '(:state normal :command meep-char-surround-insert-lines)
        "(")
      ;; Line surrounded, cursor moves right due to inserted open paren.
      ;; Cursor: (hello)
      ;;          ^
      (should (equal 'normal (bray-state)))
      (should (equal text-expected (buffer-string)))
      (should (equal '(1 . 1) (meep-test-point-line-column)))
      (should (equal ?h (char-after))))))

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

(provide 'meep_tests)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; meep_tests.el ends here
