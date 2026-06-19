;;; meep.el --- Lightweight modal editing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; Keywords: convenience, modal-editing
;; URL: https://codeberg.org/ideasman42/emacs-meep
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides utility functions to bind to keys,
;; intended to be used with `bray' although
;; most of the functions can be used with vanilla Emacs.
;;
;; Starting out you may want to load Emacs with one of the
;; bundled `init.el' files, linked from this project's URL.

;;; Usage:

;; Bind any of the `autoload' functions in this file to a key-map.

;;; Code:

(defgroup meep nil
  "Lightweight modal editing."
  :group 'emulations)

(declare-function bray-state "bray")
(declare-function bray-state-get-hook-exit "bray")
(declare-function bray-state-set "bray")
(declare-function bray-state-stack-pop "bray")
(declare-function bray-state-stack-push "bray")

(declare-function apply-on-rectangle "rect")
(declare-function kmacro-ring-head "kmacro")

;; For `meep-repeat-fu-replay'.
(declare-function repeat-fu-listener-register "repeat-fu")
(declare-function repeat-fu-listener-unregister-and-collect "repeat-fu")

;; Must be forward declared for byte-compilation.
(defvar which-key-this-command-keys-function)

;; ---------------------------------------------------------------------------
;; Custom Variables

(defcustom meep-mark-set-on-motion t
  "When non-nil, motion commands set the mark."
  :type 'boolean)

(defcustom meep-state-insert nil
  "The name of the state used for insert-mode (must be set)."
  :type 'symbol)

(defcustom meep-state-insert-register ?^
  "The register set when leaving insert mode.

Used by `meep-insert-at-last' which will enter insert mode at this location."
  :type 'register)

(defcustom meep-repeat-fu-replay t
  "When non-nil, use real insert mode for rectangle editing.
Instead of `string-rectangle', enter insert mode on the first line
and record keystrokes.  On exit, replay the recorded keystrokes
on all other lines in the rectangle.

Requires `repeat-fu-mode' to be active; falls back to
`string-rectangle' when it is not."
  :type 'boolean)

(defvar-local meep-state-region-elem nil
  "Supported values are nil or `line-wise'.

Note that line-wise navigation is not enforced;
this is a hint that commands may use.")

;; ---------------------------------------------------------------------------
;; Public Variables

;; In some cases it's necessary to know the previous point in a motion.
;; In most cases "mark-on-motion" commands set the mark, however in the case
;; of commands that adjust the motion, they don't set the mark.
;; This is needed so it's possible to use `meep-move-char-prev' and `meep-move-char-next'
;; as motions which transpose characters.
(defvar-local meep-mark-adjust nil
  "The previous position for commands that don't set mark-on-motion.

This must be set by commands that pass the:
`meep-command-is-mark-set-on-motion-adjust' test.")


;; ---------------------------------------------------------------------------
;; Compatibility

;; NOTE: can use regular `incf' when 31.1 is released.
(defmacro meep--incf (place &optional delta)
  "Increment PLACE by DELTA or 1."
  (declare (debug (gv-place &optional form)))
  (gv-letplace (getter setter) place
    (funcall setter `(+ ,getter ,(or delta 1)))))
(defmacro meep--decf (place &optional delta)
  "Decrement PLACE by DELTA or 1."
  (declare (debug (gv-place &optional form)))
  (gv-letplace (getter setter) place
    (funcall setter `(- ,getter ,(or delta 1)))))


;; ---------------------------------------------------------------------------
;; Internal Functions: Generic Helpers

(defun meep--assert-unreachable ()
  "Raise an error."
  (error "Assertion, unreachable"))

(defmacro meep--assert (condition)
  "Assert CONDITION is non-nil."
  `(unless ,condition
     (error "Assertion failed: %S" ',condition)))

(defmacro meep--swap-vars (i j)
  "Swap I and J."
  `(setq ,i
         (prog1 ,j
           (setq ,j ,i))))

;; Include a version of this function that checks the input,
;; because if the value of `pos' is *ever* null, it's difficult
;; to troubleshoot as the errors only show up later on.
(defun meep--set-marker (pos)
  "Set the current marker to POS."
  (meep--assert (integerp pos))
  (set-marker (mark-marker) pos)
  nil)

(defun meep--set-marker-and-activate (pos)
  "Set the current marker to POS and activate the region."
  (meep--set-marker pos)
  (activate-mark t)
  (setq deactivate-mark nil)
  nil)

;; ---------------------------------------------------------------------------
;; Internal Functions: Algorithms

(defun meep--plist-remove (plist key)
  "Remove KEY and its value from PLIST destructively.
Return the modified PLIST, or the original if KEY is not found."
  (declare (important-return-value t))
  (cond
   ;; Empty PLIST.
   ((null plist)
    plist)
   ;; Key is at the beginning.
   ((eq (car plist) key)
    (cddr plist))
   ;; Key is elsewhere or not present.
   (t
    (let ((tail plist)
          (next nil)
          (next-next nil))
      ;; Walk the list two cells at a time.
      (while (progn
               (setq next (cdr tail))
               (setq next-next (cdr next))
               ;; Check the next-next doesn't equal the key.
               (and next-next (null (eq (car next-next) key))))
        (setq tail next-next))
      ;; If we found the key, splice it out.
      (when next-next
        (setcdr next (cdr (cdr next-next))))
      plist))))

(defun meep--ranges-overlap-p (list-a list-b)
  "Return t if any range in LIST-A overlaps any range in LIST-B.
Each list contains cons cells (BEG . END): half-open ranges [BEG END) with
BEG <= END, so END is the position past the range and touching ranges (one
END equal to the other BEG) do not overlap.
Stop at the first detected overlap."
  (declare (important-return-value t))
  (let ((found nil))
    (while (and list-a (null found))
      (let* ((range-a (car list-a))
             (a-beg (car range-a))
             (a-end (cdr range-a))
             (b-list list-b))
        (while (and b-list (null found))
          (let* ((range-b (car b-list))
                 (b-beg (car range-b))
                 (b-end (cdr range-b)))
            ;; Overlap if the half-open ranges intersect (touching is not
            ;; overlapping, so the comparisons are strict).
            (when (and (< a-beg b-end) (< b-beg a-end))
              (setq found t)))
          (setq b-list (cdr b-list))))
      (setq list-a (cdr list-a)))
    found))


;; ---------------------------------------------------------------------------
;; Internal Utilities: Emacs/Text
;;
;; Not specific to MEEP.

(defun meep--indent-calc-in-region-from-first-non-blank-or-non-empty (beg end)
  "Return a string representing the text indenting this region.
- The first non-blank line is used.
- If all lines are blank, use the first non-empty line.
- If all lines are empty - return the longest empty string.

It is expected that BEG and END have been extended to line end-points.
The behavior if they have not is undefined."
  (declare (important-return-value t))
  (let ((result-non-empty nil)
        (result-non-empty-column 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((bol (point))
              (eol (pos-eol)))
          (cond
           ((eq bol eol)
            ;; Skip blank lines.
            (goto-char (min (1+ eol) end)))
           (t
            (skip-chars-forward "[:blank:]" eol)
            (cond
             ((eq (point) eol) ; Blank line (with blank-space).
              (let ((column (current-column)))
                (when (< result-non-empty-column column)
                  (setq result-non-empty-column column)
                  (setq result-non-empty (buffer-substring-no-properties bol (point)))))
              ;; Next line (maybe it's non-blank).
              (goto-char (min (1+ eol) end)))
             (t
              (setq result-non-empty (buffer-substring-no-properties bol (point)))
              ;; Break.
              (goto-char end))))))))
    (or result-non-empty "")))

(defun meep--replace-in-region (str beg end)
  "Replace region from BEG to END with STR.
Return the region replaced."
  (declare (important-return-value nil))
  (let ((len (length str))
        (i-beg nil)
        (i-end nil)
        (i-end-ofs nil))

    ;; Check for skip end.
    (let ((i 0))
      (let ((len-test (min (- end beg) len)))
        (while (< i len-test)
          (let ((i-next (1+ i)))
            (cond
             ((eq (aref str (- len i-next)) (char-after (- end i-next)))
              (setq i i-next))
             (t ; Break.
              (setq len-test i))))))
      (unless (zerop i)
        (setq i-end (- len i))
        (meep--decf len i)
        (meep--decf end i)
        (setq i-end-ofs i)))

    ;; Check for skip start.
    (let ((i 0))
      (let ((len-test (min (- end beg) len)))
        (while (< i len-test)
          (cond
           ((eq (aref str i) (char-after (+ beg i)))
            (meep--incf i))
           (t ; Break.
            (setq len-test i)))))
      (unless (zerop i)
        (setq i-beg i)
        (meep--incf beg i)))

    (when (or i-beg i-end)
      ;; This may assign an empty string, which is OK.
      (setq str (substring-no-properties str (or i-beg 0) (or i-end len))))

    (goto-char beg)

    (unless (eq beg end)
      (delete-region beg end))
    (unless (string-empty-p str)
      (insert str))

    (when i-end-ofs
      ;; Leave the cursor where it would be if the end wasn't clipped.
      (goto-char (+ (point) i-end-ofs)))
    (cons beg (+ beg (length str)))))

(defun meep--syntax-skip-to-comment-start ()
  "Move point to the start of comment syntax.
When point is at the very beginning of a comment,
there may be no comment syntax information at point.
Move forward until `syntax-ppss' information is available
\(typically only 1-2 characters).
Return t when point was moved to the comment start."
  (declare (important-return-value t))
  (let ((pos-found nil)
        (pos-init (point)))
    (when (forward-comment 1)
      (let ((end (point)))
        (when (forward-comment -1)
          (let ((beg (point)))
            (when (and (<= beg pos-init) (<= pos-init end))
              (let ((pos-test (1+ pos-init)))
                ;; This seems inefficient but it's very likely only skipping 1-2 chars.
                (while (< pos-test end)
                  (let ((state-test (syntax-ppss pos-test)))
                    (cond
                     ((nth 4 state-test)
                      ;; Very likely, just check the initial position
                      ;; is in the bounds of the parsed syntax.
                      (when-let* ((beg-test (nth 8 state-test)))
                        (when (<= pos-init beg-test)
                          (setq pos-found pos-test)))
                      (setq pos-test end))
                     (t
                      (meep--incf pos-test)))))))))))
    (cond
     (pos-found
      (goto-char pos-found)
      t)
     (t
      nil))))

(defun meep--rectangle-row-span (col-beg col-end)
  "Return the `(BEG . END)' buffer span between COL-BEG and COL-END on this line.
A pure query: point is preserved.  BEG equals END (an empty span) when the line
is too short to reach the columns, or when they fall within a single TAB - in
both cases `move-to-column' lands at the same position, as there is no character
boundary between them."
  (declare (important-return-value t))
  (save-excursion
    (let ((beg
           (progn
             (move-to-column col-beg)
             (point)))
          (end
           (progn
             (move-to-column col-end)
             (point))))
      (cons beg end))))

(defun meep--rectangle-range-list-from-rectangle (beg end)
  "Return a list of ranges from a rectangle from BEG and END."
  (declare (important-return-value t))
  (let ((result (list)))
    (apply-on-rectangle
     (lambda (col-beg col-end) (push (meep--rectangle-row-span col-beg col-end) result)) beg end)
    (nreverse result)))

(defun meep--columns-from-point-range (beg end)
  "Calculate the column offset between points BEG and END.

Typically these will be on the same line but this isn't a requirement."
  (declare (important-return-value t))
  (save-excursion
    (let ((col-beg
           (progn
             (goto-char beg)
             (current-column)))
          (col-end
           (progn
             (goto-char end)
             (current-column))))
      (- col-end col-beg))))

(defmacro meep--with-substitute-last-command (&rest body)
  "Execute BODY, replacing `last-command' with its :substitute if defined."
  (declare (indent 0))
  `(let ((last-command
          (or (and (symbolp last-command) (meep-command-prop-get last-command :substitute))
              ;; Keep existing binding.
              last-command)))
     ,@body))

(defun meep--register-position-or-message (reg)
  "Return the position of REG or report a message."
  (declare (important-return-value t))
  (let ((reg-val (get-register reg)))
    (cond
     ((null reg-val)
      (message "No register found at: %S" reg)
      nil)
     ((null (markerp reg-val))
      (message "No marker register found at: %S" reg)
      nil)
     (t
      (marker-position reg-val)))))


;; ---------------------------------------------------------------------------
;; Public Variables/Constants

;; This value only exists to temporarily override it.
(defvar meep-mark-set-on-motion-override nil
  "Override variable for suppressing mark-on-motion.

When let-bound to t, motions can be repeated without setting the mark.
Must never be set directly.")


;; ---------------------------------------------------------------------------
;; Motion: Symbol/Word
;;
;; Command properties:
;; Commands may have a `meep' property which is expected to be a PLIST of properties.
;;
;; :mark-on-motion
;;    - t: Mark on motion.
;;    - 'adjust: Adjust the previous motion.
;;
;;      This is used so a motion can be adjusted,
;;      without breaking the chain of commands used to repeat an action.
;;      So it's possible to perform a motion and any number of adjustments before an edit-command.
;;
;;      When repeating the motion, adjustments and edit will all be repeated.
;;      Single character motion commands take advantage of this.
;;
;;    - nil: don't mark on motion (same as missing).
;;
;; :mark-on-motion-no-repeat
;;    - t: Motions that should not be repeated, such as search.
;;      (used by repeat-fu).
;; :digit-repeat
;;    - t: The command is a digit command.
;;
;;      This command can repeat other commands multiple times.

(defun meep--mark-on-motion-set (pos always)
  "Set the mark to POS if the region is not active.
When ALWAYS is non-nil, set mark-on-motion even if the cursor didn't move."
  (when (and meep-mark-set-on-motion
             ;; Has motion, or always.
             (or always (/= pos (point)))
             ;; Has no region.
             (null (region-active-p))
             ;; Allow numeric commands to adjust the motion,
             ;; without moving the mark.
             (null meep-mark-set-on-motion-override))
    (setq deactivate-mark t)
    (meep--set-marker pos)))

(defmacro meep--with-mark-on-motion-maybe-set (&rest body)
  "Execute BODY with mark-on-motion enabled."
  (declare (indent 0))
  (let ((pos-orig (make-symbol "pos-orig")))
    `(let ((,pos-orig (point)))
       (prog1 (progn
                ,@body)
         ;; Some extra checks could be added here,
         ;; reserve for the function call to prevent code-bloat.
         (meep--mark-on-motion-set ,pos-orig nil)))))

(defmacro meep--with-mark-on-motion-always-set (&rest body)
  "Execute BODY with mark-on-motion always enabled."
  (declare (indent 0))
  (let ((pos-orig (make-symbol "pos-orig")))
    `(let ((,pos-orig (point)))
       (prog1 (progn
                ,@body)
         ;; Some extra checks could be added here,
         ;; reserve for the function call to prevent code-bloat.
         (meep--mark-on-motion-set ,pos-orig t)))))

(defun meep--maintain-line-based-region (pos-orig mrk-orig)
  "Maintain line-based selection.
POS-ORIG and MRK-ORIG define the original region."
  (when (and
         ;; Check line-based selection is in use.
         (eq meep-state-region-elem 'line-wise)
         ;; Unlikely this de-activates, check for the sake of correctness.
         (region-active-p))
    (cond
     ((< pos-orig mrk-orig)
      (when (>= (point) (mark))
        (beginning-of-line)
        (forward-line 1)
        (save-excursion
          (goto-char mrk-orig)
          (beginning-of-line)
          (forward-line -1)
          (meep--set-marker (point)))))
     ((> pos-orig mrk-orig)
      (when (<= (point) (mark))
        (beginning-of-line)
        (forward-line -1)
        (save-excursion
          (goto-char mrk-orig)
          (beginning-of-line)
          (forward-line 1)
          (meep--set-marker (point)))))))
  nil)
(defmacro meep--with-maintain-line-based-region (&rest body)
  "Execute BODY, maintaining line-based region selection."
  (declare (indent 0))
  (let ((pos-orig (make-symbol "pos-orig"))
        (mrk-orig (make-symbol "mrk-orig"))
        (was-active (make-symbol "was-active")))
    `(let ((,pos-orig (point))
           (,mrk-orig (mark))
           (,was-active (region-active-p)))
       (prog1 (progn
                ,@body)
         (when ,was-active
           ;; Some extra checks could be added here,
           ;; reserve for the function call to prevent code-bloat.
           (meep--maintain-line-based-region ,pos-orig ,mrk-orig))))))

(defun meep--mark-on-motion-maybe-activate-as-bounds ()
  "Return the active or implied region as `(BEG . END)', or nil when neither.
The implied region is the span a mark-on-motion command would have activated.
Use this where a command must read those bounds without making the region visible."
  (let ((result nil))
    (cond
     ((region-active-p)
      (setq result (cons (region-beginning) (region-end))))
     (meep-mark-set-on-motion
      (let ((local-last-command (meep--last-command)))
        (when (and (symbolp local-last-command)
                   (meep-command-is-mark-set-on-motion-any local-last-command))
          (let ((a (mark))
                (b (point)))
            (setq result
                  (cond
                   ((< a b)
                    (cons a b))
                   (t
                    (cons b a)))))))))
    result))

(defun meep--mark-on-motion-bounds ()
  "Return the equivalent region beginning and end.
These are the values that *would* be set if the motion
were to be made into the active region."
  (declare (important-return-value t))
  (when (and meep-mark-set-on-motion (null (region-active-p)))
    (let ((local-last-command (meep--last-command)))
      (when (and (symbolp local-last-command)
                 (meep-command-is-mark-set-on-motion-any local-last-command))
        (let ((a (point))
              (b (mark)))
          (cond
           ((<= a b)
            (cons a b))
           (t
            (cons b a))))))))

(defun meep--region-or-mark-on-motion-bounds ()
  "Return the region beginning and end or motion bounds."
  (declare (important-return-value t))
  (cond
   ((region-active-p)
    (cons (region-beginning) (region-end)))
   (t
    (meep--mark-on-motion-bounds))))

(defun meep--region-or-mark-bounds ()
  "Return the active or implied region as `(BEG . END)', or nil when no mark.
The implied region is whatever span the mark bounds while inactive (it stays
readable via `mark-even-if-inactive').  Verbs that operate on the region as a
noun - cut, surround add - use this so they act on a selection even when it is
not active, like `meep-clipboard-killring-cut'.  Return nil when no mark is set,
so the caller falls back to point.

Surround delete and replace instead search for an enclosing pair, so they anchor
on point and do not use this, see `meep--surround-operate-region'."
  (declare (important-return-value t))
  ;; NOTE: `(mark t)' accepts any set mark - active or not, and regardless of the
  ;; command that set it - not only a mark a motion left behind.  The mark *is*
  ;; the selection in Emacs (`mark-even-if-inactive'), so gating on how it was set
  ;; would surprise the user and diverge from `meep-clipboard-killring-cut', which
  ;; wraps the same span.  A stale mark left far from point therefore widens the
  ;; add, which is consistent with treating the region as a noun, so accept it.
  ;;
  ;; Read the span from the mark directly, not via `region-beginning' /
  ;; `region-end': those call `(mark)' which signals `(mark-inactive)' for a set
  ;; but inactive mark when `mark-even-if-inactive' is nil (a common user setting).
  (let ((m (mark t)))
    (and m
         (let ((p (point)))
           (cons (min m p) (max m p))))))

;;;###autoload
(defun meep-move-symbol-prev (arg)
  "Move point to the beginning of the previous symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    ;; at-start = (> arg 0): preserves primitive landing for both directions.
    (meep-motion-at-point 'symbol nil (- arg) (> arg 0))))

;;;###autoload
(defun meep-move-symbol-prev-end (arg)
  "Move to the end of the previous symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep-motion-at-point 'symbol nil (- arg) nil))
     (t
      (meep-motion-at-point 'symbol nil (- arg) nil t)))))

;;;###autoload
(defun meep-move-symbol-next-end (arg)
  "Move to the end of the next symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    ;; at-start = (< arg 0): preserves primitive landing for both directions.
    (meep-motion-at-point 'symbol nil arg (< arg 0))))

;;;###autoload
(defun meep-move-symbol-next (arg)
  "Move point to the beginning of the next symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep-motion-at-point 'symbol nil arg t))
     (t
      (meep-motion-at-point 'symbol nil arg t t)))))

;;;###autoload
(defun meep-move-word-prev (arg)
  "Move point to the beginning of the previous word, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    ;; at-start = (> arg 0): preserves primitive landing for both directions.
    (meep-motion-at-point 'word nil (- arg) (> arg 0))))

;;;###autoload
(defun meep-move-word-next-end (arg)
  "Move to the end of the next word, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    ;; at-start = (< arg 0): preserves primitive landing for both directions.
    (meep-motion-at-point 'word nil arg (< arg 0))))

;;;###autoload
(defun meep-move-word-prev-end (arg)
  "Move to the end of the previous word, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep-motion-at-point 'word nil (- arg) nil))
     (t
      (meep-motion-at-point 'word nil (- arg) nil t)))))

;;;###autoload
(defun meep-move-word-next (arg)
  "Move point to the beginning of the next word, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep-motion-at-point 'word nil arg t))
     (t
      (meep-motion-at-point 'word nil arg t t)))))


;; ---------------------------------------------------------------------------
;; Motion: List Item
;;
;; Useful for navigating over function arguments,
;; but can be used for stepping over other kinds of list-items.
;;
;; Especially useful for transposing arguments as it properly handles
;; multi-line arguments, arguments mixed with comments and arguments that
;; themselves contain lists/function calls.

;;;###autoload
(defun meep-move-list-item-prev (arg)
  "Move point to the beginning of the (previous) list item, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    ;; at-start = (> arg 0): preserves primitive landing for both directions.
    (meep-motion-at-point 'list-item t (- arg) (> arg 0))))

;;;###autoload
(defun meep-move-list-item-next-end (arg)
  "Move to the end of the current list item, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    ;; at-start = (< arg 0): preserves primitive landing for both directions.
    (meep-motion-at-point 'list-item t arg (< arg 0))))

;;;###autoload
(defun meep-move-list-item-prev-end (arg)
  "Move to the end of the previous list item, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep-motion-at-point 'list-item t (- arg) nil))
     (t
      (meep-motion-at-point 'list-item t (- arg) nil t)))))

;;;###autoload
(defun meep-move-list-item-next (arg)
  "Move point to the beginning of the next list item, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep-motion-at-point 'list-item t arg t))
     (t
      (meep-motion-at-point 'list-item t arg t t)))))


;; ---------------------------------------------------------------------------
;; Motion: Same Syntax

(defun meep--move-same-syntax-impl (n skip-single skip-space or-thing)
  "Move forward over N syntax-spans, a negative argument skips backwards.
When SKIP-SINGLE isn't nil, initial single motion isn't counted as a step for N.
SKIP-SPACE is a cons cell for additional space skipping before and after the motion.
When OR-THING is non-nil, skip over the bounds of the `thing-at-point'."
  (let ((syn nil)
        (skip-space-beg (car skip-space))
        (skip-space-end (cdr skip-space))
        (pos-orig (point)))
    (cond
     ;; Backwards (matches "Forwards" logic closely).
     ((< n 0)
      (setq n (- n))

      (when skip-space-end
        (skip-chars-backward "[:blank:]" (point-min)))
      (while (and (null (bobp))
                  (null
                   (zerop
                    (prog1 n
                      (meep--decf n)))))
        (syntax-ppss)
        (setq syn (syntax-after (1- (point))))
        (while (and (null (bobp)) (equal syn (syntax-after (1- (point)))))
          (forward-char -1)
          ;; Optionally step over blocks.
          (when or-thing
            (let ((bounds (bounds-of-thing-at-point or-thing)))
              (when (and bounds (>= (cdr bounds) (1+ (point))) (> (point) (car bounds)))
                (goto-char (car bounds)))))

          (syntax-ppss))

        (when skip-single
          (setq skip-single nil)
          ;; Skip the first item if this would have only moved one.
          (when (eq 1 (- pos-orig (point)))
            (meep--incf n))))
      (when (and skip-space-beg (< 0 n))
        (unless (zerop (skip-chars-backward "[:blank:]" (point-min)))
          (meep--decf n skip-space-beg))))
     ;; Forwards (matches "Backwards" logic closely).
     (t
      (when (and skip-space-beg (< 0 n))
        (unless (zerop (skip-chars-forward "[:blank:]" (point-max)))
          (meep--decf n skip-space-beg)))
      (while (and (null (eobp))
                  (null
                   (zerop
                    (prog1 n
                      (meep--decf n)))))
        (syntax-ppss)
        (setq syn (syntax-after (point)))
        (while (and (null (eobp)) (equal syn (syntax-after (point))))
          (forward-char 1)
          ;; Optionally step over blocks.
          (when or-thing
            (let ((bounds (bounds-of-thing-at-point or-thing)))
              (when (and bounds (<= (car bounds) (1- (point))) (< (point) (cdr bounds)))
                (goto-char (cdr bounds)))))
          (syntax-ppss))

        (when skip-single
          (setq skip-single nil)
          ;; Skip the first item if this would have only moved one.
          (when (eq 1 (- (point) pos-orig))
            (meep--incf n))))

      (when skip-space-end
        (skip-chars-forward "[:blank:]" (point-max))))))
  nil)

;;;###autoload
(defun meep-move-same-syntax-prev (arg)
  "Move back over characters with the same syntax class, ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-next (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl (- arg) t (cons nil nil) nil)))))

;;;###autoload
(defun meep-move-same-syntax-next (arg)
  "Move forward over characters with the same syntax class, ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-prev (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl arg t (cons nil nil) nil)))))


;; ---------------------------------------------------------------------------
;; Motion: Same Syntax or Symbol
;;
;; Skip over the same syntax or entire symbols.

;;;###autoload
(defun meep-move-same-syntax-or-symbol-prev (arg)
  "Move back over characters with the same syntax class or symbols, ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-or-symbol-next (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl (- arg) t (cons nil nil) 'symbol)))))

;;;###autoload
(defun meep-move-same-syntax-or-symbol-next (arg)
  "Move forward over characters with the same syntax class or symbols, ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-or-symbol-prev (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl arg t (cons nil nil) 'symbol)))))


;; ---------------------------------------------------------------------------
;; Motion: Same Syntax and Space
;;
;; Skip over the same syntax with changes to behavior for surrounding space,
;; where space at the bounds of text is skipped over, matching
;; how this is handled for skipping words and symbols.

;;;###autoload
(defun meep-move-same-syntax-and-space-prev (arg)
  "Move back a syntax-and-space, ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-and-space-next (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl (- arg) nil (cons 0 nil) nil)))))

;;;###autoload
(defun meep-move-same-syntax-and-space-next (arg)
  "Move to the end of the next syntax-and-space, ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-and-space-prev (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl arg nil (cons 1 0) nil)))))

;;;###autoload
(defun meep-move-same-syntax-and-space-next-end (arg)
  "Move to the end of the next syntax-and-space, ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-and-space-prev (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl arg nil (cons 1 nil) nil)))))


;; ---------------------------------------------------------------------------
;; Motion: Line

(defun meep--move-line-beginning-end-impl (n)
  "Move to line beginning or end based on sign of N."
  (cond
   ((< n 0)
    (beginning-of-line))
   (t
    (end-of-line))))

;;;###autoload
(defun meep-move-line-beginning (arg)
  "Move to the beginning of the current line.
Move to the end when ARG is negative."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-beginning-end-impl (- arg))))

;;;###autoload
(defun meep-move-line-end (arg)
  "Move to the end of the current line.
Move to the beginning when ARG is negative."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-beginning-end-impl arg)))

(defun meep--move-line-non-space-beginning-end-impl (n)
  "Move to non-space line beginning or end based on sign of N."
  (cond
   ((< n 0)
    (beginning-of-line)
    (skip-chars-forward "[:blank:]" (pos-eol)))
   (t
    (end-of-line)
    (let ((eol (point))
          (bol (pos-bol)))
      (skip-chars-backward "[:blank:]" bol)
      ;; When the line is entirely blank both directions move the point to the line end.
      ;; Otherwise going to the "end" would move the point to a location before the beginning
      ;; which isn't logical.
      (when (eq bol (point))
        (goto-char eol)))))
  nil)

;;;###autoload
(defun meep-move-line-non-space-beginning (arg)
  "Move to the first non-blank character of the line.
A negative ARG moves to the end."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-non-space-beginning-end-impl (- arg))))

;;;###autoload
(defun meep-move-line-non-space-end (arg)
  "Move to the end of the line, ignoring trailing blank-space.
A negative ARG moves to the beginning."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-non-space-beginning-end-impl arg)))

(defun meep--move-line-wrapper (n &optional noerror)
  "Call `line-move' with N, setting `last-command' to respect the goal column.
Only needed from interactive line move.
NOERROR is forwarded to `line-move'."
  ;; With a line-wise region, enforce a zero goal.
  ;; Needed for line-wise regions when used with packages that use overlays such as
  ;; `hl-indent-scope' which cause the line beginning not to start at the 0-th column.
  ;; In this case it's necessary to force the column to be zero.
  (let ((goal-column
         (cond
          ((and (eq meep-state-region-elem 'line-wise)
                ;; Only makes sense to use line-wise with an active region.
                (region-active-p)
                ;; Otherwise this locks to the line beginning,
                ;; even after horizontal motion which feels too constrained.
                (bolp))
           0)
          (t
           goal-column))))
    ;; Needed for line motion.
    (meep--with-substitute-last-command
      (line-move n noerror))))

;;;###autoload
(defun meep-move-line-prev (arg)
  "Move to the previous line ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--with-maintain-line-based-region
      (meep--move-line-wrapper (- arg) t))))

;;;###autoload
(defun meep-move-line-next (arg)
  "Move to the next line ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--with-maintain-line-based-region
      (meep--move-line-wrapper arg t))))


;; ---------------------------------------------------------------------------
;; Motion: Character

;;;###autoload
(defun meep-move-char-prev (arg)
  "Move to the previous character ARG times."
  (interactive "^p")
  ;; Intentionally don't include in "mark-on-motion",
  ;; allow adjustments after motion.
  (setq meep-mark-adjust (point))
  (left-char arg))

;;;###autoload
(defun meep-move-char-next (arg)
  "Move to the next character ARG times."
  (interactive "^p")
  ;; Intentionally don't include in "mark-on-motion",
  ;; allow adjustments after motion.
  (setq meep-mark-adjust (point))
  (right-char arg))


;; ---------------------------------------------------------------------------
;; Motion: Paragraph

;;;###autoload
(defun meep-move-paragraph-prev (arg)
  "Move backward paragraphs ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--with-maintain-line-based-region
      (backward-paragraph arg))))

;;;###autoload
(defun meep-move-paragraph-next (arg)
  "Move forward paragraphs ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--with-maintain-line-based-region
      (forward-paragraph arg))))


;; ---------------------------------------------------------------------------
;; Motion: Sentence

;;;###autoload
(defun meep-move-sentence-prev (arg)
  "Move backward sentences, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (forward-sentence (- arg))))

;;;###autoload
(defun meep-move-sentence-next (arg)
  "Move forward sentences, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (forward-sentence arg)))


;; ---------------------------------------------------------------------------
;; Motion: S-expressions

(defcustom meep-move-comment-skip-space t
  "When navigating comment bounds, skip leading/trailing space."
  :type 'boolean)

;; Useful for `/** comment */` or `/// comment.`
(defcustom meep-move-comment-skip-repeated t
  "When navigating comment bounds, skip repeated characters."
  :type 'boolean)

(defun meep--goto-comment-or-string-bounds (dir)
  "Move point to the beginning/end of the comment or string.

When DIR is -1, move to the beginning; when 1, move to the end.
Return t when stepping out of string or comment bounds."
  (declare (important-return-value t))
  ;; When in a comment or string, skip out of it.
  (let ((state (syntax-ppss))
        (changed nil))
    (cond
     ((nth 3 state) ; String.
      (when-let* ((beg (nth 8 state)))
        (cond
         ((eq dir -1)
          (goto-char beg)
          (setq changed t))
         (t
          (let ((pos-init (point))
                (pos-next nil))
            (save-excursion
              (goto-char beg)
              (forward-sexp)
              (unless (eq pos-init (point))
                (setq pos-next (point))))
            (when pos-next
              (goto-char pos-next)
              (setq changed t)))))))
     ((nth 4 state) ; Comment.
      (when-let* ((beg (nth 8 state)))
        (cond
         ((eq dir -1)
          (goto-char beg)
          (setq changed t))
         (t
          (let ((pos-next nil))
            (save-excursion
              (goto-char beg)
              (when (forward-comment 1)
                (setq pos-next (point))))
            (when pos-next
              (goto-char pos-next)
              (setq changed t))))))))
    changed))

(defun meep--jump-brackets-from-mode ()
  "Return a cons cell of brackets to use for navigation based on the major mode."
  (declare (important-return-value t))
  (cond
   ;; Modes that use {} brackets.
   ;; For now, assume that if "{}" are brackets,
   ;; then these are the "main" brackets to use for navigation,
   ;; true for C/C++/Java, etc.
   ((and (eq (char-syntax ?\{) ?\() ; Check both match.
         (eq (char-syntax ?\}) ?\)))
    (list ?{ ?}))
   (t
    nil)))

(defun meep--jump-next-sexp-step-over-impl ()
  "Step over the next SEXP or return nil."
  (declare (important-return-value t))
  (let ((p nil))
    ;; Never true when at the beginning.
    (unless (bobp)
      (save-excursion
        (forward-char -1)
        (when (looking-at-p "\\s(")
          (forward-sexp 1 nil)
          (setq p (point)))))
    (cond
     (p
      (goto-char p)
      t)
     (t
      nil))))

(defun meep--jump-prev-sexp-step-over-impl ()
  "Step over the previous SEXP or return nil."
  (declare (important-return-value t))
  (let ((p nil))
    ;; Never true when at the end.
    (unless (eobp)
      (save-excursion
        (when (looking-at-p "\\s)")
          (forward-char 1)
          (forward-sexp -1 nil)
          (setq p (point)))))
    (cond
     (p
      (goto-char p)
      t)
     (t
      nil))))

(defun meep--jump-next-sexp-impl (n step-over)
  "Jump to the next SEXP N times.
When STEP-OVER is non-nil, don't step into expressions."
  (declare (important-return-value t))
  (let ((pos nil)
        (changed nil))
    (cond
     ((and step-over (meep--jump-next-sexp-step-over-impl))
      (setq changed t))
     (t
      (save-excursion
        ;; When in a comment or string, skip out of it.
        (ignore (meep--goto-comment-or-string-bounds 1))
        (condition-case _
            (progn
              (down-list n)
              (setq pos (point)))
          (error ; Fallback.
           ;; This may error too, do nothing in this case.
           (when (ignore-errors
                   (up-list n)
                   t)
             (setq pos (point))))))
      (when pos
        (setq changed t)
        (goto-char pos))))

    changed))

(defun meep--jump-prev-sexp-impl (n step-over)
  "Jump to the previous SEXP N times.
When STEP-OVER is non-nil, don't step into expressions."
  (declare (important-return-value t))
  (setq n (- n))
  (let ((pos nil)
        (changed nil))
    (cond
     ((and step-over (meep--jump-prev-sexp-step-over-impl))
      (setq changed t))
     (t
      (save-excursion
        ;; When in a comment or string, skip out of it.
        (ignore (meep--goto-comment-or-string-bounds -1))
        (condition-case _
            (progn
              (down-list n)
              (setq pos (point)))
          (error ; Fallback.
           ;; This may error too, do nothing in this case.
           (when (ignore-errors
                   (up-list n)
                   t)
             (setq pos (point))))))
      (when pos
        (setq changed t)
        (goto-char pos))))

    changed))

(defsubst meep--is-bracket-at-p-impl (pos syntax-class bracket-chars)
  "Return t if POS has SYNTAX-CLASS and char at POS is in BRACKET-CHARS.
SYNTAX-CLASS is 4 for open parenthesis, 5 for close parenthesis."
  (declare (important-return-value t))
  (let ((syn (syntax-after pos)))
    (cond
     ((and syn
           (eq (syntax-class syn) syntax-class)
           (or (null bracket-chars) (memq (char-after pos) bracket-chars)))
      t)
     (t
      nil))))

(defun meep--is-point-after-bracket-open (bracket-chars)
  "Return t if point is after an opening bracket in BRACKET-CHARS."
  (declare (important-return-value t))
  (meep--is-bracket-at-p-impl (1- (point)) 4 bracket-chars)) ; Open parenthesis.

(defun meep--is-point-after-bracket-close (bracket-chars)
  "Return t if point is after a closing bracket in BRACKET-CHARS."
  (declare (important-return-value t))
  (meep--is-bracket-at-p-impl (1- (point)) 5 bracket-chars)) ; Close parenthesis.

(defun meep--is-point-before-bracket-open (bracket-chars)
  "Return t if point is before an opening bracket in BRACKET-CHARS."
  (declare (important-return-value t))
  (meep--is-bracket-at-p-impl (point) 4 bracket-chars)) ; Open parenthesis.

(defun meep--is-point-before-bracket-close (bracket-chars)
  "Return t if point is before a closing bracket in BRACKET-CHARS."
  (declare (important-return-value t))
  (meep--is-bracket-at-p-impl (point) 5 bracket-chars)) ; Close parenthesis.

(defun meep--move-by-sexp-any-impl (n step-over)
  "Jump to the next/previous SEXP by N.
When STEP-OVER is non-nil, don't step into nested blocks."
  (declare (important-return-value t))
  (let ((pos-init (point))
        (pos-found nil)
        ;; Store valid steps, as it's possible to step into S-expressions
        ;; which don't match `bracket-chars'. Typically these are stepped over
        ;; however there may be brackets at the beginning or end of the buffer.
        ;; In that case the point should not be left on a bracket not intended to
        ;; be used for navigation.
        (pos-step nil)
        (bracket-chars (meep--jump-brackets-from-mode))
        (times 1))
    (cond
     ;; Previous.
     ((< n 0)
      (setq n (- n))
      (save-excursion
        ;; Match forward/backward motion.
        (when (meep--is-point-after-bracket-open bracket-chars)
          (forward-char -1))
        ;; Ensure only intended brackets count as steps.
        (when (and (< 1 n) bracket-chars)
          (setq times n)
          (setq n 1))

        (dotimes (_ times)
          (while (and (meep--jump-prev-sexp-impl n step-over)
                      (let ((keep-searching
                             (cond
                              (bracket-chars
                               (null (memq (char-after (point)) bracket-chars)))
                              (t
                               nil))))
                        (unless keep-searching
                          (setq pos-step (point)))
                        keep-searching))))

        (when pos-step
          (goto-char pos-step)
          (when (meep--is-point-before-bracket-open bracket-chars)
            (forward-char 1))
          (unless (eq pos-init (point))
            (setq pos-found (point))))))
     ;; Next.
     (t
      (save-excursion
        ;; Match forward/backward motion.
        (when (meep--is-point-before-bracket-close bracket-chars)
          (forward-char 1))
        ;; Ensure only intended brackets count as steps.
        (when (and (< 1 n) bracket-chars)
          (setq times n)
          (setq n 1))

        (dotimes (_ times)
          (while (and (meep--jump-next-sexp-impl n step-over)
                      (let ((keep-searching
                             (cond
                              (bracket-chars
                               (null (memq (char-before (point)) bracket-chars)))
                              (t
                               nil))))
                        (unless keep-searching
                          (setq pos-step (point)))
                        keep-searching))))

        (when pos-step
          (goto-char pos-step)
          (when (meep--is-point-after-bracket-close bracket-chars)
            (forward-char -1))
          (unless (eq pos-init (point))
            (setq pos-found (point)))))))
    pos-found))

;;;###autoload
(defun meep-move-by-sexp-any-next (arg)
  "Jump to the next S-expression, ARG times."
  (interactive "^p")
  (when-let* ((pos-found (meep--move-by-sexp-any-impl arg nil)))
    (meep--with-mark-on-motion-maybe-set
      (goto-char pos-found))))

;;;###autoload
(defun meep-move-by-sexp-any-prev (arg)
  "Jump to the previous S-expression, ARG times."
  (interactive "^p")
  (when-let* ((pos-found (meep--move-by-sexp-any-impl (- arg) nil)))
    (meep--with-mark-on-motion-maybe-set
      (goto-char pos-found))))


(defvar-local meep-move-by-sexp-over-depth nil
  "The target depth when moving over S-expressions.
Used to maintain the depth even when the motion causes
navigation to move to an outer scope.

Only used between successive
`meep-move-by-sexp-over-next' and `meep-move-by-sexp-over-prev' calls.")

;; Internal, avoids multiple similar S-expression lookups,
;; the cache is valid as long as `meep--sexp-depth-calc' calls
;; are done without any buffer edits.
;;
;; - Format: (tick pos1 pos2 ...) where tick is `buffer-chars-modified-tick'.
;; - Positions are ordered largest to smallest (innermost bracket first).
;; - The place represents the depth (1 based).
;;   Where the first item has depth 1, the second depth 2 and so on.
(defvar-local meep--sexp-depth-calc-cache nil)

(defun meep--sexp-depth-calc ()
  "Return the S-expression depth."
  (declare (important-return-value t))
  (let* ((bracket-chars (meep--jump-brackets-from-mode))
         (tick (buffer-chars-modified-tick))
         (cache-prev
          (cond
           ((and meep--sexp-depth-calc-cache (eq tick (car meep--sexp-depth-calc-cache)))
            (cdr meep--sexp-depth-calc-cache))
           (t
            nil)))
         (cache-tail (cons nil nil))
         (cache-cell cache-tail)
         (keep-searching t)
         (depth 0))
    (save-excursion
      (cond
       ((meep--is-point-after-bracket-open bracket-chars)
        (forward-char -1))
       ((meep--is-point-before-bracket-close bracket-chars)
        (forward-char 1)))
      (while (and keep-searching
                  (condition-case _
                      (progn
                        (backward-up-list 1 nil t)
                        t)
                    (user-error
                     nil)))
        (let ((pos (point)))
          (when (or (null bracket-chars) (memq (char-after pos) bracket-chars))
            ;; Try to use cache for an early exit.
            (when cache-prev
              ;; Remove any cached positions after this position,
              ;; happens when the point moves to a position before previously cached points.
              (while (and cache-prev (< pos (car cache-prev)))
                (pop cache-prev))
              ;; If this point is cached, count the outer levels and exit this loop.
              (when (and cache-prev (eq pos (car cache-prev)))
                (setq keep-searching nil)
                (setcdr cache-tail cache-prev)
                (meep--incf depth (length cache-prev))))

            (when keep-searching
              (meep--incf depth)

              ;; Build the next cache.
              (let ((cache-next (cons pos nil)))
                (setcdr cache-tail cache-next)
                (setq cache-tail cache-next)))))))

    (setq meep--sexp-depth-calc-cache (cons tick (cdr cache-cell)))
    depth))

(defun meep--move-by-sexp-over-last-command-check ()
  "Return non-nil when the `last-command' moved over an S-expression."
  (declare (important-return-value t))
  (memq last-command '(meep-move-by-sexp-over-next meep-move-by-sexp-over-prev)))

;;;###autoload
(defun meep-move-by-sexp-over-next (arg)
  "Move to the next S-expression at the same depth, ARG times."
  (interactive "^p")
  (let ((step-over t)
        (this-depth (meep--sexp-depth-calc)))
    (unless (meep--move-by-sexp-over-last-command-check)
      (setq meep-move-by-sexp-over-depth nil))
    (when (and meep-move-by-sexp-over-depth (< this-depth meep-move-by-sexp-over-depth))
      (setq step-over nil))

    (when-let* ((pos-found (meep--move-by-sexp-any-impl arg step-over)))
      (meep--with-mark-on-motion-maybe-set
        (goto-char pos-found)))

    (unless meep-move-by-sexp-over-depth
      (setq meep-move-by-sexp-over-depth this-depth))))

;;;###autoload
(defun meep-move-by-sexp-over-prev (arg)
  "Move to the previous S-expression at the same depth, ARG times."
  (interactive "^p")
  (let ((step-over t)
        (this-depth (meep--sexp-depth-calc)))
    (unless (meep--move-by-sexp-over-last-command-check)
      (setq meep-move-by-sexp-over-depth nil))
    (when (and meep-move-by-sexp-over-depth (< this-depth meep-move-by-sexp-over-depth))
      (setq step-over nil))

    (when-let* ((pos-found (meep--move-by-sexp-any-impl (- arg) step-over)))
      (meep--with-mark-on-motion-maybe-set
        (goto-char pos-found)))

    (unless meep-move-by-sexp-over-depth
      (setq meep-move-by-sexp-over-depth this-depth))))

(defun meep--move-by-sexp-out-impl (arg)
  "Move up and out of the expression.
Move forward when ARG is positive, otherwise backwards."
  (let ((pos-init (point))
        (pos-found (point))
        (bracket-chars (meep--jump-brackets-from-mode))
        (found nil))
    (save-excursion
      (cond
       ((meep--is-point-after-bracket-open bracket-chars)
        (forward-char -1))
       ((meep--is-point-before-bracket-close bracket-chars)
        (forward-char 1)))

      ;; TODO: only get opening brackets.
      (let ((keep-looking t))
        (while keep-looking
          (let ((pos-prev (point)))
            (backward-up-list (abs arg) nil t)
            (cond
             ;; Stop if we didn't move (reached top of buffer/structure).
             ((eq (point) pos-prev)
              (setq keep-looking nil))
             ;; Stop if we found a valid bracket (or aren't filtering by brackets).
             ((or (null bracket-chars) (memq (char-after (point)) bracket-chars))
              (setq keep-looking nil)
              (setq found t))))))

      (when found
        ;; Jump to the matching bracket (otherwise this is like prev-out).
        (when (< 0 arg)
          (forward-sexp 1 t))

        (unless (eq pos-init (point))
          (forward-char
           (cond
            ((< 0 arg)
             -1)
            (t
             1)))
          (setq pos-found (point)))))

    (unless (eq pos-init pos-found)
      (meep--with-mark-on-motion-maybe-set
        (goto-char pos-found)))))

;;;###autoload
(defun meep-move-by-sexp-out-prev (&optional arg)
  "Jump out of the current S-expression to the opening bracket, ARG times."
  (interactive "^p")
  (meep--move-by-sexp-out-impl (- (or arg 1))))

;;;###autoload
(defun meep-move-by-sexp-out-next (&optional arg)
  "Jump out of the current S-expression to the closing bracket, ARG times."
  (interactive "^p")
  (meep--move-by-sexp-out-impl (or arg 1)))

;;;###autoload
(defun meep-move-matching-bracket-outer ()
  "Jump to the matching outer bracket.
When not at the bounds, jump to the start (when enclosed in brackets).

Return non-nil when point was moved."
  (interactive "^")
  (meep--with-mark-on-motion-maybe-set
    (let ((result nil)
          (is-interactive t)
          ;; It only makes sense to use an argument of 1
          ;; when jumping to matching items.
          (arg 1)
          (pos-orig (point))
          ;; Jump to the start of the string to prevent failure to jump out of it.
          (pos-outer (nth 8 (syntax-ppss))))
      (cond
       ((meep--is-point-before-bracket-open nil)
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (forward-sexp arg is-interactive)
                 (point))))
          (setq result t)
          (goto-char pos)))
       ((meep--is-point-after-bracket-close nil)
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (forward-sexp (- arg) is-interactive)
                 (point))))
          (setq result t)
          (goto-char pos))))
      ;; As a handy fallback, jump up to the enclosing parenthesis.
      (when (eq pos-orig (point))
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (backward-up-list arg nil t)
                 (point))))
          (setq result t)
          (goto-char pos)))
      result)))


;; ---------------------------------------------------------------------------
;; Motion: Matching Characters
;;
;; Jump to the opposite character - bracket, quote or comment bounds
;; depending on the command.
;;
;; When no matching character is found,
;; jump to the start of the surrounding characters (if found).

;;;###autoload
(defun meep-move-matching-bracket-inner ()
  "Jump to the matching inner bracket.
When not at the bounds, jump to the start (when enclosed in brackets).

Return non-nil when point was moved."
  (interactive "^")
  (meep--with-mark-on-motion-maybe-set
    (let ((result nil)
          (is-interactive t)
          ;; It only makes sense to use an argument of 1
          ;; when jumping to matching items.
          (arg 1)
          (pos-orig (point))
          ;; Jump to the start of the string to prevent failure to jump out of it.
          (pos-outer (nth 8 (syntax-ppss))))
      (cond
       ((meep--is-point-after-bracket-open nil)
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (forward-char -1)
                 (forward-sexp arg is-interactive)
                 (forward-char -1)
                 (point))))
          (setq result t)
          (goto-char pos)))
       ((meep--is-point-before-bracket-close nil)
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (forward-char 1)
                 (forward-sexp (- arg) is-interactive)
                 (forward-char 1)
                 (point))))
          (setq result t)
          (goto-char pos))))
      ;; As a handy fallback, jump up to the opening parenthesis.
      (when (eq pos-orig (point))
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (backward-up-list arg nil t)
                 (forward-char 1)
                 (point))))
          (setq result t)
          (goto-char pos)))
      result)))

(defun meep--bounds-at-point-for-comment-outer ()
  "Return the outer bounds for the comment at point or nil when not found."
  (declare (important-return-value t))
  (let ((state (syntax-ppss)))
    (unless (nth 4 state)
      ;; Don't step back onto the previous line as it causes
      ;; syntax with single line comments to consider the blank
      ;; line after a comment to be a comment.
      (unless (or (bobp) (bolp))
        (save-excursion
          ;; We could use a nicer method of checking this.
          ;; Be smarter.
          (forward-char -1)
          (let ((state-test (syntax-ppss)))
            (when (nth 4 state-test)
              (setq state state-test))))))

    ;; We may be at the comment start, which isn't considered a comment.
    (unless (nth 4 state)
      ;; Skip into the comment and get the state.
      (save-excursion
        (when (meep--syntax-skip-to-comment-start)
          (setq state (syntax-ppss)))))

    (when (nth 4 state)
      (let ((pos-orig (point))
            (beg (nth 8 state)))
        (when (and beg (<= beg pos-orig))
          (save-excursion
            (goto-char beg)
            (forward-comment 1)
            (skip-syntax-backward ">" pos-orig)
            ;; It's possible a comment range is *before* the point.
            ;; Typically when the point is on the next line.
            ;; Ensure the original point is in the range.
            ;; Checking the `beg' is more of an extra precaution.
            (when (<= pos-orig (point))
              (cons beg (point)))))))))

(defun meep--syntax-state-is-string (state)
  "Return non-nil if STATE is a string."
  (declare (important-return-value t))
  (and (null (nth 4 state)) (nth 8 state)))

(defun meep--bounds-at-point-for-string-outer ()
  "Return the outer bounds for the string at point or nil when not found."
  (declare (important-return-value t))
  (let ((pos-orig (point)))
    (when-let* ((start
                 (or (meep--syntax-state-is-string (syntax-ppss))
                     (and (null (bobp))
                          (meep--syntax-state-is-string
                           (save-excursion (syntax-ppss (1- pos-orig)))))
                     (and (null (eobp))
                          (meep--syntax-state-is-string
                           (save-excursion (syntax-ppss (1+ pos-orig))))))))
      (when (<= start pos-orig)
        (save-excursion
          (goto-char start)
          (forward-sexp)
          (when (<= pos-orig (point))
            (cons start (point))))))))

(defun meep--bounds-match-at-end-points (bounds beg-re end-re)
  "Return t if BOUNDS begins and ends with BEG-RE and END-RE."
  (declare (important-return-value t))
  (save-excursion
    (cond
     ((and (or (null beg-re)
               (progn
                 (goto-char (car bounds))
                 (looking-at-p beg-re)))
           (or (null end-re)
               (progn
                 (goto-char (cdr bounds))
                 (save-match-data (looking-back end-re (car bounds))))))
      t)
     (t
      nil))))

(defun meep--bounds-equal-at-end-points (bounds beg-str end-str)
  "Return t if BOUNDS begins and ends with BEG-STR and END-STR."
  (declare (important-return-value t))
  (let* ((beg (car bounds))
         (end (cdr bounds))
         (bounds-len (- end beg))
         (beg-str-len (length beg-str))
         (end-str-len (length end-str)))
    (cond
     ((and (>= bounds-len (+ beg-str-len end-str-len))
           (or (zerop beg-str-len)
               (string-equal beg-str (buffer-substring-no-properties beg (+ beg beg-str-len))))
           (or (zerop end-str-len)
               (string-equal end-str (buffer-substring-no-properties (- end end-str-len) end))))
      t)
     (t
      nil))))

;;;###autoload
(defun meep-bounds-inner-from-delimiters (pairs bounds)
  "Contract BOUNDS by the first matching delimiter in PAIRS.

PAIRS is a list of `(BEG-STR . END-STR)' cons cells.  For the
first pair where BOUNDS begins with BEG-STR and ends with
END-STR, return BOUNDS contracted by the lengths of those
delimiters.  Return nil if no pair matches.

Intended for use as the FUNC in `meep-bounds-for-inner-comment',
where ARGS is the PAIRS list.  Useful when writing a preset for
a mode whose comment delimiters are fixed strings - most of the
bundled `meep-preset-MODE.el' files (`c-mode', `lua-mode',
`html-mode', ...) configure their comment spec this way."
  (declare (important-return-value t))
  (let ((result nil))
    (while (and pairs (null result))
      (let* ((pair (pop pairs))
             (beg-str (car pair))
             (end-str (cdr pair)))
        (when (meep--bounds-equal-at-end-points bounds beg-str end-str)
          (setq result
                (cons (+ (car bounds) (length beg-str)) (- (cdr bounds) (length end-str)))))))
    result))

(defun meep--bounds-at-point-for-comment-inner-guess (bounds)
  "Contract BOUNDS based on `meep-bounds-for-inner-comment'.

The spec is read via `meep-preset-ensure-variable', so the
preset for the current `major-mode' is consulted on demand -
users do not have to call `meep-preset-ensure' from a mode hook
just to get inner-comment bounds.  An explicit user setting -
buffer-local or global - still takes precedence over the preset."
  (declare (important-return-value t))
  (let ((spec (meep-preset-ensure-variable 'meep-bounds-for-inner-comment)))
    (when spec
      (funcall (car spec) (cadr spec) bounds))))

(defun meep--bounds-at-point-for-comment-inner ()
  "Return the inner bounds for the comment at point or nil when not found."
  (declare (important-return-value t))
  (let ((bounds-outer (meep--bounds-at-point-for-comment-outer))
        (result nil))
    ;; Contract, NOTE: this doesn't work with C/C++
    ;; as they don't consider the /* ... */ to be start/end. sigh!
    (when bounds-outer
      (let ((beg (car bounds-outer))
            (end (cdr bounds-outer)))

        (save-excursion
          (goto-char beg)
          (skip-syntax-forward "<")
          (setq beg (point))

          (goto-char end)
          (skip-syntax-backward ">")
          (setq end (point)))

        ;; When unchanged "guess" the inner bounds.
        (when (and (eq beg (car bounds-outer)) (eq end (cdr bounds-outer)))
          ;; If this fails, just don't return the inner bounds
          ;; not great - but better than returning nil.
          (when-let* ((bounds-inner
                       (meep--bounds-at-point-for-comment-inner-guess (cons beg end))))
            (setq beg (car bounds-inner))
            (setq end (cdr bounds-inner))))

        ;; Skip repeated chars: "/***** "
        ;; TODO: check if the characters are space (seems very unlikely).
        ;; but could mess with `meep-move-comment-skip-space'.
        (when meep-move-comment-skip-repeated
          (unless (eq beg (car bounds-outer))
            (save-excursion
              (goto-char beg)
              (when-let* ((ch-code (char-before beg)))
                (let ((ch (char-to-string ch-code)))
                  (skip-chars-forward ch end)
                  (unless (eq (point) end)
                    (setq beg (point)))))))
          (unless (eq end (cdr bounds-outer))
            (save-excursion
              (goto-char end)
              (when-let* ((ch-code (char-after end)))
                (let ((ch (char-to-string ch-code)))
                  (skip-chars-backward ch beg)
                  (unless (eq (point) beg)
                    (setq end (point))))))))

        (unless (and (eq beg (car bounds-outer)) (eq end (cdr bounds-outer)))
          (setq result (cons beg end))))

      (when meep-move-comment-skip-space
        (when result
          (let ((skip "[:blank:]"))
            (setq result (meep--bounds-contract-by-chars-non-empty result skip skip))))))

    (or result bounds-outer)))

(defun meep--bounds-at-point-for-string-inner ()
  "Return the inner bounds for the string at point or nil when not found."
  (declare (important-return-value t))
  (let ((bounds (meep--bounds-at-point-for-string-outer)))
    ;; TODO: make this more advanced, for now, simply contract successive matching characters
    ;; as this is quite a good heuristic for strings in most languages.
    (when bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (beg-chr (char-after beg))
             (end-chr (char-before end)))

        ;; Disallow the beginning from being greater than the end.
        (while (and (<= 2 (- end beg))
                    (eq beg-chr (char-after beg))
                    (eq end-chr (char-before end)))
          (meep--incf beg)
          (meep--decf end))
        (setq bounds (cons beg end))))
    bounds))

;;;###autoload
(defun meep-move-matching-syntax-outer ()
  "Move to the outer matching string/comment syntax.
When not at the bounds, jump to the start (when in a string/comment).

Return nil if no matching syntax was found."
  (interactive "^")
  (meep--with-mark-on-motion-maybe-set
    (let ((result nil)
          (pos-orig (point))
          (bounds
           (or (meep--bounds-at-point-for-comment-outer)
               (meep--bounds-at-point-for-string-outer))))
      (when bounds
        (setq result t)
        (cond
         ((eq (point) (car bounds))
          (goto-char (cdr bounds)))
         ((eq (point) (cdr bounds))
          (goto-char (car bounds))))
        ;; As a handy fallback, jump to the beginning of bounds.
        (when (eq pos-orig (point))
          (goto-char (car bounds))))
      result)))

;;;###autoload
(defun meep-move-matching-syntax-inner ()
  "Move to the inner matching string/comment syntax.
When not at the bounds, jump to the start (when in a string/comment).

Return nil if no matching syntax was found."
  (interactive "^")
  (meep--with-mark-on-motion-maybe-set
    (let ((result nil)
          (pos-orig (point))
          (bounds
           (or (meep--bounds-at-point-for-comment-inner)
               (meep--bounds-at-point-for-string-inner))))
      (when bounds
        (setq result t)
        (cond
         ((eq (point) (car bounds))
          (goto-char (cdr bounds)))
         ((eq (point) (cdr bounds))
          (goto-char (car bounds))))
        ;; As a handy fallback, jump to the beginning of bounds.
        (when (eq pos-orig (point))
          (goto-char (car bounds))))
      result)))

;;;###autoload
(defun meep-move-matching-contextual-outer ()
  "Move to the matching character.
When not at the bounds, jump to the start."
  (interactive "^")
  (or (meep-move-matching-syntax-outer) (meep-move-matching-bracket-outer)))

;;;###autoload
(defun meep-move-matching-contextual-inner ()
  "Move to the matching character.
When not at the bounds, jump to the start."
  (interactive "^")
  (or (meep-move-matching-syntax-inner) (meep-move-matching-bracket-inner)))


;; ---------------------------------------------------------------------------
;; Motion: Find and Till

(defvar meep--move-find-last-char nil
  "The last character used to find.")

(defun meep--move-find-impl (n ch is-till)
  "Implementation for find and till motions.
N is the number of times to find.
CH is the character to find.
IS-TILL when non-nil, stop just before the character."
  (let* ((case-fold-search nil)
         (ch-str
          (cond
           ((eq ch 13)
            "\n")
           (t
            (char-to-string ch))))

         ;; Note that limiting could be optional.
         (end-limit
          (cond
           ((> n 0)
            (pos-eol))
           (t
            (pos-bol))))

         (end
          (save-mark-and-excursion
            (when is-till
              (cond
               ((> n 0)
                (forward-char 1))
               (t
                (forward-char -1))))

            (search-forward ch-str end-limit t n))))

    (meep--with-mark-on-motion-maybe-set
      (cond
       ((null end)
        (message "char %s not found" ch-str))

       (is-till
        (goto-char
         (+ end
            (cond
             ((< n 0)
              1)
             (t
              -1))))
        (setq meep--move-find-last-char ch))
       (t
        (goto-char end)
        (setq meep--move-find-last-char ch))))))

;; Avoid repeating the message all over the place.
(defun meep--move-find-last-char-or-message ()
  "Return the last character or display a message and return nil."
  (declare (important-return-value t))
  (cond
   (meep--move-find-last-char
    meep--move-find-last-char)
   (t
    (message "No last character is set")
    nil)))

;;;###autoload
(defun meep-move-find-char-on-line-at-next (arg ch)
  "Find the next ARG char CH, read from minibuffer."
  (interactive "^p\ncFind Next:")
  (meep--move-find-impl arg ch nil))

;;;###autoload
(defun meep-move-find-char-on-line-at-prev (arg ch)
  "Find the previous ARG char CH, read from minibuffer."
  (interactive "^p\ncFind Prev:")
  (meep--move-find-impl (- arg) ch nil))

;;;###autoload
(defun meep-move-find-char-on-line-till-next (arg ch)
  "Find till the next char CH, ARG times."
  (interactive "^p\ncTill Next:")
  (meep--move-find-impl arg ch t))

;;;###autoload
(defun meep-move-find-char-on-line-till-prev (arg ch)
  "Find till the previous char CH, ARG times."
  (interactive "^p\ncTill Prev:")
  (meep--move-find-impl (- arg) ch t))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-at-next (arg)
  "Repeat find ARG times forwards."
  (interactive "^p")
  (when-let* ((ch (meep--move-find-last-char-or-message)))
    (meep--move-find-impl arg ch nil)))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-at-prev (arg)
  "Repeat find ARG times backwards."
  (interactive "^p")
  (when-let* ((ch (meep--move-find-last-char-or-message)))
    (meep--move-find-impl (- arg) ch nil)))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-till-next (arg)
  "Repeat find-till ARG times forwards."
  (interactive "^p")
  (when-let* ((ch (meep--move-find-last-char-or-message)))
    (meep--move-find-impl arg ch t)))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-till-prev (arg)
  "Repeat find-till ARG times backwards."
  (interactive "^p")
  (when-let* ((ch (meep--move-find-last-char-or-message)))
    (meep--move-find-impl (- arg) ch t)))


;; ---------------------------------------------------------------------------
;; Motion: Bounds (Implementation)

(defun meep--bounds-contract-by-chars (bounds skip-beg skip-end)
  "Contract BOUNDS by SKIP-BEG and SKIP-END."
  (declare (important-return-value t))
  (let ((beg (car bounds))
        (end (cdr bounds)))
    (when (< beg end)
      (save-excursion
        (goto-char end)
        (unless (zerop (skip-chars-backward skip-end beg))
          (setq end (point)))

        (goto-char beg)
        (unless (zerop (skip-chars-forward skip-beg end))
          (setq beg (point)))))
    (cons beg end)))

(defun meep--bounds-contract-by-chars-non-empty (bounds skip-beg skip-end)
  "Contract BOUNDS by SKIP-BEG and SKIP-END."
  (declare (important-return-value t))
  (let ((beg (car bounds))
        (end (cdr bounds))
        (non-empty t))
    (when (< beg end)
      (save-excursion
        (goto-char end)
        (unless (zerop (skip-chars-backward skip-end beg))
          (cond
           ((eq beg (point))
            (setq non-empty nil))
           (t
            (setq end (point)))))
        (when non-empty
          (goto-char beg)
          (unless (zerop (skip-chars-forward skip-beg end))
            (setq beg (point))))))
    (cons beg end)))

(defun meep--bounds-of-visual-line (inner)
  "Return bounds of the visual line at point.
When INNER is non-nil, contract by surrounding blank/newline characters."
  (declare (important-return-value t))
  (let ((bounds
         (cons
          (save-excursion
            (beginning-of-visual-line)
            (point))
          (save-excursion
            (end-of-visual-line)
            (point)))))
    (when inner
      (let ((skip "[:blank:]\r\n"))
        (setq bounds (meep--bounds-contract-by-chars bounds skip skip))))
    bounds))

(defun meep--bounds-of-sentence (inner)
  "Return bounds of the sentence at point, or nil.
When INNER is non-nil, contract to inner bounds."
  (declare (important-return-value t))
  (let ((bounds (bounds-of-thing-at-point 'sentence)))
    (when (and bounds inner)
      ;; Since it's unlikely the beginning is blank,
      ;; it's likely only the end of the bounds changes when `inner' is used.
      ;; Note that skipping back punctuation is a generalization,
      ;; using `sentence-end' could work too, but is quite involved and
      ;; difficult to get working reliably.
      (save-excursion
        (goto-char (cdr bounds))
        (skip-chars-backward "[:blank:]\r\n" (car bounds))
        ;; Trim punctuation.
        (skip-syntax-backward "." (car bounds))
        (setcdr bounds (point))))
    bounds))

(defun meep--bounds-of-paragraph (inner)
  "Return bounds of the paragraph at point, or nil.
When INNER is non-nil, contract to inner bounds."
  (declare (important-return-value t))
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (when (and bounds inner)
      (let ((skip "[:blank:]\r\n"))
        (setq bounds (meep--bounds-contract-by-chars bounds skip skip))))
    bounds))

(defun meep--move-to-bounds-endpoint (bounds n)
  "Move to the start/end of BOUNDS (start when N is negative)."
  ;; Always set the mark so `meep-region-activate-and-reverse-motion' works
  ;; even when the point doesn't move, see: #8.
  (meep--with-mark-on-motion-always-set
    (cond
     ((< n 0)
      (goto-char (car bounds)))
     (t
      (goto-char (cdr bounds))))))

(defun meep--move-to-bounds-of-thing (thing n)
  "Move to bounds of THING (start when N is negative)."
  (when-let* ((bounds (bounds-of-thing-at-point thing)))
    (meep--move-to-bounds-endpoint bounds n)))

;; ---------------------------------------------------------------------------
;; Mark: Bounds
;;
;; Note that MEEP's design is typically to mark after a motion,
;; however there are some downsides to this:
;; - A motion only sets the point, not the opposite mark.
;; - Activating the region is an extra step.
;;
;; A common operation such as [change, in, quotes] is 3 keys,
;; without a way to mark the region in bounds this would take
;; an additional key-stroke to activate.
;;
;; While it's arguably acceptable, it's a useful enough functionality
;; to support for convenience.

(defcustom meep-symmetrical-chars
  (list (cons "(" ")") (cons "[" "]") (cons "{" "}") (cons "<" ">"))
  "List of character matches.

Used for `meep-region-mark-bounds-of-char-inner' and
`meep-region-mark-bounds-of-char-outer'."
  :type '(repeat (cons string string)))

;; NOTE: this would benefit from per-major-mode presets,
;; Since ORG mode and similar should use `*' and `='.
;;
;; NOTE: order from least to most likely.
(defcustom meep-match-bounds-of-char-contextual
  '(("\"" . "\"")
    ("'" . "'") ("`" . "`") ("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">")

    ;; Non ASCII characters.
    ("“" . "”") ("‘" . "’"))
  "List of boundary string matches used for automatically marking bounds.

While this is typically used for brackets and quotes,
multi-character pairs are also supported.

Used for `meep-region-mark-bounds-of-char-inner-contextual' and
`meep-region-mark-bounds-of-char-outer-contextual'."
  :type '(repeat (cons string string)))

(defcustom meep-bounds-for-inner-comment nil
  "Spec for contracting outer comment bounds to inner.

When non-nil, the value is a 2-element list `(FUNC ARGS)'.  Given
outer comment bounds BOUNDS, the inner bounds are computed as

  (funcall FUNC ARGS BOUNDS)

which should return a cons `(BEG . END)' or nil if BOUNDS cannot
be contracted.

Defaults to nil; the spec is then read from the preset for the
current `major-mode' (see the bundled `meep-preset-MODE.el'
files).  An explicit user setting - buffer-local or global -
takes precedence over the preset."
  :type '(choice (const nil) (list function sexp)))

(defun meep--list-item-bounds-default ()
  "Return a fallback `meep-list-item-bounds' spec derived from the buffer.
Each single-character pair in `meep-symmetrical-chars' with paren syntax in the
current buffer becomes two fallback entries: braces `{}' split on `;' (else `,'),
other brackets on `,' (else `;').  See `meep-list-item-bounds' for the format."
  (declare (important-return-value t))
  (let ((result nil))
    (dolist (pair meep-symmetrical-chars)
      (let ((open (car pair))
            (close (cdr pair)))
        (when (and (stringp open) (length= open 1) (stringp close) (length= close 1))
          (let ((open-ch (string-to-char open))
                (close-ch (string-to-char close)))
            (when (and (eq (char-syntax open-ch) ?\() (eq (char-syntax close-ch) ?\)))
              (let ((bracket (cons open-ch close-ch))
                    (seps
                     (cond
                      ((eq open-ch ?\{)
                       '(";" ","))
                      (t
                       '("," ";")))))
                (dolist (sep seps)
                  (push (list bracket (list sep)) result))))))))
    (nreverse result)))

(defcustom meep-list-item-bounds nil
  "Spec for the `list-item' text object: bracket lists and their separators.

The value is a list of entries, each of the form

  ((OPEN . CLOSE) SEPARATORS)

OPEN and CLOSE are the bracket characters delimiting a list, and SEPARATORS is
the list of that list's separator strings, or t to split on runs of whitespace
\(e.g. Lisp, where items are space-separated):

  (((?\\( . ?\\)) (\",\")) ((?\\=\\{ . ?\\}) (\";\")))
  (((?\\( . ?\\)) t))   ; whitespace-separated, e.g. Lisp

Each bracket type defines its own separators, so different brackets may separate
differently - in C/C++ `()' splits on `,' while `{}' splits on `;'.  A bracket
may appear in more than one entry to give a fallback chain: entries are tried in
order and the first whose separators occur in the list wins, e.g. C/C++ braces
holding either statements (`;') or an initializer (`,').

Nesting is read from the buffer's syntax tree, so a separator inside any deeper
bracket, string or comment never splits a list item.  Each OPEN and CLOSE must
therefore be a single character with paren syntax in the current mode (possibly
via a syntax-table text property, as CC Mode does for C++ template `<' / `>').

When nil, falls back to the preset for the current `major-mode', then to a spec
generated from `meep-symmetrical-chars' (see `meep--list-item-bounds-default')."
  :type
  '(repeat
    (list
     (cons (character :tag "Open") (character :tag "Close"))
     (choice (repeat (string :tag "Separator")) (const :tag "Whitespace" t)))))

(defun meep--symmetrical-char-entry (ch-str)
  "Return the `meep-symmetrical-chars' `(OPEN . CLOSE)' entry CH-STR names, or nil.
CH-STR may match either side of the entry.  The shared scan behind
`meep--symmetrical-char-other-any' and `meep--symmetrical-char-pair-canonical',
which differ only in what they read from the matched entry."
  (declare (important-return-value t))
  (let ((chars meep-symmetrical-chars)
        (result nil))
    (while chars
      (let ((item (pop chars)))
        (when (or (string-equal ch-str (car item)) (string-equal ch-str (cdr item)))
          (setq result item)
          (setq chars nil))))
    result))

(defun meep--symmetrical-char-other-any (ch-str)
  "Return the symmetrical character of CH-STR or nil.
Leads with the side opposite CH-STR (the one the bounds-of-char motion searches
for); see `meep--symmetrical-char-pair-canonical' for the canonically-ordered pair."
  (declare (important-return-value t))
  (let ((entry (meep--symmetrical-char-entry ch-str)))
    (and entry
         (cond
          ((string-equal ch-str (car entry))
           (cdr entry))
          (t
           (car entry))))))

(defun meep--symmetrical-char-pair-canonical (ch-str)
  "Return the `(OPEN . CLOSE)' pair CH-STR belongs to, open first, or nil.
CH-STR may name either side of a `meep-symmetrical-chars' entry, so a closing
delimiter resolves to the same pair as its opener.  Unlike
`meep--symmetrical-char-other-any', which leads with the typed character (for the
bounds-of-char motion that searches for it), this orders the pair canonically -
what surround needs to wrap a region the same way whichever side is typed.
Conses a fresh pair rather than returning the matched entry, so a caller may
mutate it without touching `meep-symmetrical-chars'."
  (declare (important-return-value t))
  (let ((entry (meep--symmetrical-char-entry ch-str)))
    (and entry (cons (car entry) (cdr entry)))))

(defcustom meep-syntax-backend nil
  "Backend for locating the bracket pair enclosing point.

- nil: choose automatically per buffer - `syntax' in `prog-mode' derivatives
  (whose syntax tables are reliable), `text' elsewhere (so prose and markup
  delimiters keep working).
- `text': scan the buffer text.  Matches any configured delimiter, including
  multi-character markup, but counts brackets inside strings and comments.
- `syntax': read the syntax tree (`syntax-ppss').  Ignores brackets inside
  strings and comments and nests correctly, but applies only to single-character
  paren-syntax brackets - every other delimiter always scans text regardless of
  this setting (same-delimiter quotes and markup never consult it; multi-character
  and non-paren pairs fall back to `text').  Auto-detected quote and markup pairs
  are dropped from surround recognition, so surround-delete inside a top-level
  string with no enclosing bracket is a no-op (use `text', or configure the quote
  in `meep-surround-pairs', to delete a string's own quotes).

Affects the surround delete / replace verbs and the mark-bounds-of-char motions."
  :type
  '(choice
    (const :tag "Automatic (syntax in prog-mode, text elsewhere)" nil)
    (const :tag "Scan the buffer text" text)
    (const :tag "Read the syntax tree" syntax)))

(defun meep--syntax-backend-resolve ()
  "Return the effective `meep-syntax-backend' for the current buffer.
A nil (auto) value resolves to `syntax' in `prog-mode' derivatives - whose
syntax tables are reliable - and `text' elsewhere."
  (declare (important-return-value t))
  (or meep-syntax-backend
      ;; NOTE: `derived-mode-p' is a proxy for "is the syntax table reliable for
      ;; brackets".  It is imperfect both ways - it forces `text' on `markdown',
      ;; `org' & `latex' buffers that do have real bracket syntax, and `syntax' on
      ;; a thin or generated `prog-mode' table.  A finer axis (inspecting the
      ;; syntax table) could be used, however `meep-syntax-backend' is overridable
      ;; per buffer, so it isn't worth the added complexity.  Leave as-is.
      (cond
       ((derived-mode-p 'prog-mode)
        'syntax)
       (t
        'text))))

;; ---------------------------------------------------------------------------
;; Syntax Query Layer: Enclosing Bracket Pair
;;
;; "What bracket encloses point?" answered by two interchangeable backends
;; (`meep-syntax-backend' selects), so a caller can pick correctness-by-syntax or
;; reach beyond the syntax table:
;; - `-from-text' scans the buffer text.  Matches any configured pair, including
;;   multi-character markup, but counts brackets inside strings and comments.
;; - `-from-syntax' reads the syntax tree.  Ignores brackets inside strings and
;;   comments and nests correctly, but only sees single-character parens.
;; `meep--syntax-enclosing-pair' dispatches between them.

(defun meep--bracket-pair-regex (open close)
  "Return a regex matching OPEN as group 1 or CLOSE as group 2.
Both delimiters are quoted; shared by the depth-counting bracket scans."
  (declare (important-return-value t))
  (concat "\\(" (regexp-quote open) "\\)\\|\\(" (regexp-quote close) "\\)"))

(defun meep--syntax-enclosing-pair-from-text
    (bounds-init bounds-limit ch-str-pair &optional match-at-start)
  "Find the pair of matching brackets around BOUNDS-INIT.
BOUNDS-LIMIT constrains the search bounds.
CH-STR-PAIR provides the bracket strings (supports multi-character brackets).
When MATCH-AT-START is non-nil, an opening bracket sitting exactly at the start of
BOUNDS-INIT counts as the match (cursor-on-open); when nil the search begins
strictly outside it, so a caller peeling outward from a pair reaches the next one.
Return the bounds or nil if no matching brackets are found."
  (declare (important-return-value t))
  ;; Match case-sensitively, as the same-delimiter branch of
  ;; `meep--region-mark-bounds-of-char-calc' does, so a letter-bearing multi-char
  ;; delimiter (e.g. `<b>') does not pair with a different-case token (`<B>') under
  ;; an ambient `case-fold-search' (the default, and `t' in `html-mode').
  (let* ((case-fold-search nil)
         (has-region (not (eq (car bounds-init) (cdr bounds-init))))
         (open-str (car ch-str-pair))
         (close-str (cdr ch-str-pair))
         (open-str-quote (regexp-quote open-str))
         (limit-min (car bounds-limit))
         (limit-max (cdr bounds-limit))
         (bracket-regex (meep--bracket-pair-regex open-str close-str)))
    (let ((find-open-bracket-backward-fn
           (lambda ()
             (save-match-data
               (save-excursion
                 (let ((depth 0)
                       (found nil))
                   ;; Point may sit exactly on an opening bracket (cursor on the
                   ;; open delimiter).  `re-search-backward' only matches strictly
                   ;; before point, so it would step past this open to an outer
                   ;; pair (or miss entirely) - the close-side search has no such
                   ;; blind spot, so cursor-on-open and cursor-on-close disagreed.
                   ;; Treat a looking-at open as the depth-zero match so the two
                   ;; behave alike.  MATCH-AT-START gates this: a caller peeling
                   ;; outward passes it nil so the open it is anchored on is skipped
                   ;; and the enclosing pair is found instead.
                   (cond
                    ((and match-at-start (looking-at-p open-str-quote))
                     (setq found (point)))
                    (t
                     (while (and (null found)
                                 (> (point) limit-min)
                                 (re-search-backward bracket-regex limit-min t))
                       (cond
                        ;; Found an opening bracket: finish or decrease depth and continue.
                        ((match-beginning 1)
                         (cond
                          ((zerop depth)
                           (setq found (point)))
                          (t
                           (meep--decf depth))))
                        ((match-beginning 2)
                         ;; Found a closing bracket: increase depth.
                         (meep--incf depth))))))
                   found)))))
          (find-close-bracket-forward-fn
           (lambda ()
             (save-match-data
               (save-excursion
                 (when (looking-at-p open-str-quote)
                   (forward-char (length open-str)))
                 (let ((depth 0)
                       (found nil))
                   (while (and (null found)
                               (< (point) limit-max)
                               (re-search-forward bracket-regex limit-max t))
                     (cond
                      ((match-beginning 2)
                       ;; Found a closing bracket: finish or decrease depth and continue.
                       (cond
                        ((zerop depth)
                         (setq found (point)))
                        (t
                         (meep--decf depth))))
                      ((match-beginning 1)
                       ;; Found an opening bracket: increase depth.
                       (meep--incf depth))))
                   found))))))

      (save-excursion
        ;; Search from the beginning of the region.
        (goto-char (car bounds-init))
        (let* ((open-pos (funcall find-open-bracket-backward-fn))
               (result nil))
          (when open-pos
            ;; Now find the matching closing bracket.
            (goto-char open-pos)
            (let ((close-pos (funcall find-close-bracket-forward-fn)))
              (when (and close-pos
                         (cond
                          (has-region
                           ;; When region is active, ensure brackets contain the region.
                           (and (<= open-pos (car bounds-init)) (>= close-pos (cdr bounds-init))))
                          (t
                           ;; When no region, ensure point is between brackets.
                           (>= close-pos (car bounds-init)))))
                (setq result (cons open-pos close-pos)))))
          result)))))

(defun meep--syntax-stack-opens (ppss pos &optional at-pos)
  "Return PPSS's open-paren positions enclosing POS, innermost-first.
PPSS is a `syntax-ppss' result for POS.  `nth 9' lists the open positions
outermost-first; reversing runs them innermost-first, so the first encloser found
is the nearest.  When AT-POS is non-nil and POS is outside a string or comment
\(`nth 8'), an open sitting at POS - not yet on the stack - is prepended as the
innermost candidate (cursor-on-open).  Callers match the open character and
resolve the matching close themselves (e.g. with `scan-sexps')."
  (declare (important-return-value t))
  (let ((opens (reverse (nth 9 ppss))))
    (cond
     ((and at-pos (not (nth 8 ppss)))
      (cons pos opens))
     (t
      opens))))

(defun meep--syntax-enclosing-pair-from-syntax
    (bounds-init bounds-limit ch-str-pair &optional match-at-start)
  "Syntax-tree backend of `meep--syntax-enclosing-pair'.
The enclosing pair is read from the `syntax-ppss' open-paren stack, so a bracket
inside a string or comment is never matched.  OPEN of CH-STR-PAIR must be a
single character with paren syntax; the arguments and return value otherwise
match `meep--syntax-enclosing-pair-from-text'.  Return nil when no such pair
encloses BOUNDS-INIT within BOUNDS-LIMIT."
  (declare (important-return-value t))
  (let ((open-char (string-to-char (car ch-str-pair)))
        (beg (car bounds-init))
        (end (cdr bounds-init))
        (clamp-min (car bounds-limit))
        (clamp-max (cdr bounds-limit)))
    (let ((has-region (not (eq beg end)))
          (ppss (syntax-ppss beg))
          (result nil))
      ;; Open-paren positions enclosing BEG, innermost-first; MATCH-AT-START adds an
      ;; open sitting at BEG (cursor-on-open), which the per-candidate char test
      ;; below rejects when its char is not OPEN-CHAR.
      (let ((opens (meep--syntax-stack-opens ppss beg match-at-start)))
        (while (and opens (null result))
          (let* ((open-beg (pop opens))
                 (close-end
                  (and (eq (char-after open-beg) open-char)
                       (ignore-errors
                         (scan-sexps open-beg 1)))))
            ;; A pair qualifies when it lies within the clamp and contains
            ;; BOUNDS-INIT; candidates run innermost-first, so the first to qualify
            ;; is the nearest enclosing pair.  An outer pair is never nearer on the
            ;; clamp - its open is smaller and its close larger - so rejecting and
            ;; continuing cannot skip a closer in-clamp match.
            (when (and close-end
                       (>= open-beg clamp-min) (<= close-end clamp-max)
                       (cond
                        (has-region
                         (and (<= open-beg beg) (>= close-end end)))
                        (t
                         (>= close-end beg))))
              (setq result (cons open-beg close-end))))))
      result)))

(defun meep--syntax-pair-is-paren-p (ch-str-pair)
  "Return non-nil when CH-STR-PAIR's open is a single paren-syntax character.
Only such a pair can be matched from the syntax tree; a multi-character or
non-paren delimiter (markdown markup, `<' `>' lacking paren syntax) cannot, so
the syntax backend is skipped for it."
  (declare (important-return-value t))
  (let ((open (car ch-str-pair)))
    (and (length= open 1) (eq (char-syntax (string-to-char open)) ?\())))

(defun meep--syntax-enclosing-pair (bounds-init bounds-limit ch-str-pair &optional match-at-start)
  "Return the bracket pair around BOUNDS-INIT as `(OPEN-POS . CLOSE-POS)', or nil.
Routes to a backend per `meep-syntax-backend': the syntax tree when it is
`syntax' and CH-STR-PAIR is a paren-syntax bracket, otherwise the text scan.  The
arguments and return value match `meep--syntax-enclosing-pair-from-text'."
  (declare (important-return-value t))
  (cond
   ((and (eq (meep--syntax-backend-resolve) 'syntax) (meep--syntax-pair-is-paren-p ch-str-pair))
    (meep--syntax-enclosing-pair-from-syntax bounds-init bounds-limit ch-str-pair match-at-start))
   (t
    ;; NOTE: the text fall-back is not only for the `text' backend - under
    ;; `syntax' it is what locates an explicitly configured non-paren pair (a
    ;; quote or markup token a mode surrounds with).
    ;; `meep--surround-recognition-pairs' drops such pairs from *auto*-detection
    ;; under `syntax' yet still trusts the explicit config, relying on this
    ;; fall-back to find them.  Keep it.
    (meep--syntax-enclosing-pair-from-text bounds-init bounds-limit ch-str-pair match-at-start))))

(defun meep--region-mark-same-delim-opener-p (delim pos clamp-min paragraph-parity)
  "Return non-nil when same-delimiter DELIM sitting at POS opens a pair, by parity.
Toggle for each DELIM from the parity-scope start up to POS; an even count (t)
means POS opens a pair, odd (nil) means it closes one.

When PARAGRAPH-PARITY is non-nil the scope starts at POS's paragraph (the first
blank line above POS, bounded below by CLAMP-MIN), else at POS's line start.  A
same-delimiter span never crosses a blank line, so paragraph scope is correct for
matching a multi-line span; counting from CLAMP-MIN (`point-min' on the region
path) instead would let a stray token in unrelated prose - an apostrophe in a
contraction, say - flip the count and reject a real pair.  Per-line scope, the
bounds-of-char motion default, ignores a stray on an earlier line of the same
paragraph (so a single-line pair after such a stray is still found) but cannot
validate a span whose open is on an earlier line."
  (declare (important-return-value t))
  ;; NOTE: a stray DELIM earlier on POS's *own* line flips the count, so a real
  ;; opener reads as a closer and the pair is rejected (e.g. a literal `*' from
  ;; `2 * 3' before `*bold*').  Both scopes count POS's line, so neither per-line
  ;; nor the paragraph retry can exclude it.  This is inherent - a lone
  ;; same-delimiter token has no opener / closer identity, so a literal token is
  ;; indistinguishable from a delimiter; proximity matching cannot help, as it
  ;; would accept an adjacent close + open (`*a* *b*') as a pair just the same.
  ;; Rare in practice, so accept the limitation.
  (save-excursion
    (save-match-data
      (let ((case-fold-search nil)
            (parity-start
             (cond
              (paragraph-parity
               (goto-char pos)
               (cond
                ((re-search-backward "\n[[:blank:]]*\n" clamp-min t)
                 (match-end 0))
                (t
                 clamp-min)))
              (t
               (goto-char pos)
               (pos-bol))))
            (is-open t))
        (goto-char parity-start)
        (while (search-forward delim pos t)
          (setq is-open (not is-open)))
        is-open))))

(defun meep--peel-outward (seed count step-fn)
  "Return the COUNT-th layer outward from SEED, clamping to the outermost.
SEED is layer 1; STEP-FN is called on the current layer to get the next one
enclosing it, COUNT-1 times.  STEP-FN returns the enclosing layer or nil; a nil
result (fewer than COUNT layers exist) stops the peel and keeps the outermost
layer found so far.  Return nil when SEED is nil."
  (declare (important-return-value t))
  (let ((found seed)
        (i 1))
    (while (and found (< i count))
      (let ((next (funcall step-fn found)))
        (cond
         (next
          (setq found next)
          (meep--incf i))
         (t
          (setq i count)))))
    found))

(defun meep--region-mark-bounds-of-char-calc
    (bounds-init bounds-limit n ch-str-pair &optional match-at-start paragraph-fallback)
  "Calculate the bounds around CH-STR-PAIR from BOUNDS-INIT N times.
BOUNDS-LIMIT constrains the search bounds.
MATCH-AT-START, when non-nil, treats a delimiter sitting exactly at BOUNDS-INIT's
start as the open (cursor-on-open) - for the distinct-bracket case via
`meep--syntax-enclosing-pair', and for the same-delimiter case via the
parity-validated branch below.
PARAGRAPH-FALLBACK, when non-nil, also accepts a same-delimiter opener validated
by paragraph-scope parity when the per-line scope (the bounds-of-char motion
default, always tried first) rejects it, recovering a multi-line span whose open
sits on an earlier line; both scopes reuse one forward+backward scan.  See
`meep--region-mark-same-delim-opener-p'."
  (declare (important-return-value t))
  (cond
   ((string-equal (car ch-str-pair) (cdr ch-str-pair))
    (let* ((delim (car ch-str-pair))
           (delim-len (length delim))
           (clamp-min (car bounds-limit))
           (beg nil)
           (end nil)
           (bounds nil)
           (case-fold-search nil)
           ;; Cursor-on-open: point sits exactly on a delimiter that opens a pair.
           ;; `search-backward' would skip it (it matches strictly before point) and
           ;; `search-forward' would take it as the close, pairing prior tokens; so
           ;; anchor BEG here instead.  The opener test is paragraph-scoped
           ;; unconditionally - a closer earlier on the same line must not make the
           ;; opener at point read as a close, which is exactly the wrong match the
           ;; per-line search can otherwise return.  Honoured only when
           ;; MATCH-AT-START is set (the outward peel passes it nil so it steps past
           ;; its own open).
           (cursor-on-open
            (and match-at-start
                 (save-excursion
                   (goto-char (car bounds-init))
                   (looking-at-p (regexp-quote delim)))
                 (meep--region-mark-same-delim-opener-p delim (car bounds-init) clamp-min t))))
      (save-match-data
        (save-excursion
          (cond
           (cursor-on-open
            (setq beg (car bounds-init))
            ;; Search the close past the open's own end (and past the region end),
            ;; so the open at point is not re-matched as the close.  Thread N so the
            ;; branch peels outward like the ordinary and distinct-bracket paths:
            ;; with BEG anchored at the open, the tokens forward alternate
            ;; close, open, close ... so the Nth pair's close is the (2N-1)th token.
            ;; Counting raw tokens (N) instead would land the Nth-forward token on an
            ;; *opening* delimiter for N > 1, returning an unbalanced span that ends
            ;; mid-pair; (2N-1) keeps the span ending on a close.  A count past the
            ;; available delimiters returns nil (not found), matching the ordinary
            ;; same-delimiter branch rather than clamping.
            (goto-char (max (+ beg delim-len) (cdr bounds-init)))
            (when (search-forward delim (cdr bounds-limit) t (1- (* 2 n)))
              (setq end (point))
              (setq bounds (cons beg end))))
           (t
            (goto-char (cdr bounds-init))
            (when (search-forward delim (cdr bounds-limit) t n)
              (setq end (point))
              (goto-char (car bounds-init))
              (when (and (search-backward delim clamp-min t n)
                         ;; Per-line scope first; accept paragraph scope as a
                         ;; fall-back (the surround search) to recover a multi-line
                         ;; span - both reuse this one forward+backward scan.
                         (or (meep--region-mark-same-delim-opener-p delim (point) clamp-min nil)
                             (and paragraph-fallback
                                  (meep--region-mark-same-delim-opener-p
                                   delim (point) clamp-min t))))
                (setq beg (point))
                (setq bounds (cons beg end))))))))
      bounds))
   (t
    ;; Distinct brackets: peel outward to the Nth enclosing pair (a depth count),
    ;; matching the same-delimiter branch which honours N.  The first find may match
    ;; a bracket at point (MATCH-AT-START); each further layer searches strictly
    ;; outside the pair found so far (nil match-at-start steps past its own open), so
    ;; a count past the available depth clamps to the outermost.
    (meep--peel-outward
     (meep--syntax-enclosing-pair bounds-init bounds-limit ch-str-pair match-at-start) n
     (lambda (layer) (meep--syntax-enclosing-pair layer bounds-limit ch-str-pair nil))))))

(defun meep--bounds-within-pair-delimiter-p (inner-bounds outer-bounds outer-pair)
  "Return non-nil when INNER-BOUNDS falls inside OUTER-BOUNDS' own delimiter.
OUTER-PAIR gives the delimiter lengths.  A shorter same-character delimiter can
mis-pair the two halves of a longer token - cursor-on-open `*' matching the two
`*' of a `**' - and that match lands entirely within the longer pair's opening or
closing delimiter rather than nesting inside its content.  A caller comparing
candidate pairs rejects such a match in favour of the longer pair, honouring the
multi-character-first order of the recognized pairs."
  (declare (important-return-value t))
  (let ((content-start (+ (car outer-bounds) (length (car outer-pair))))
        (content-end (- (cdr outer-bounds) (length (cdr outer-pair)))))
    (or (<= (cdr inner-bounds) content-start) (>= (car inner-bounds) content-end))))

(defun meep--bounds-supersedes-p (bounds-test best best-pair)
  "Return non-nil when BOUNDS-TEST should replace BEST as the innermost pair.
BOUNDS-TEST is a fresh candidate; BEST is the kept pair so far (nil when none)
with delimiter BEST-PAIR.  A non-nil candidate wins unless it only re-pairs
BEST's own delimiter - a shorter same-character delimiter inside a longer token -
so the more-specific markup is kept; see `meep--bounds-within-pair-delimiter-p'."
  (declare (important-return-value t))
  (and bounds-test
       (not (and best (meep--bounds-within-pair-delimiter-p bounds-test best best-pair)))))

(defun meep--region-mark-bounds-init ()
  "Return the initial bounds."
  (declare (important-return-value t))
  (cond
   ((region-active-p)
    (cons (region-beginning) (region-end)))
   (t
    (cons (point) (point)))))

(defun meep--region-mark-bounds-to-region (bounds is-forward)
  "Mark BOUNDS as a region.
When IS-FORWARD is non-nil, point is after the mark."
  (unless (region-active-p)
    (meep-region-enable))
  (cond
   (is-forward
    (meep--set-marker (car bounds))
    (goto-char (cdr bounds)))
   (t
    (meep--set-marker (cdr bounds))
    (goto-char (car bounds)))))

(defun meep--region-mark-ch-pair-from-char (ch)
  "Return a cons cell of CH converted to a string."
  (declare (important-return-value t))
  (let ((ch-str (make-string 1 ch)))
    (cons ch-str (or (meep--symmetrical-char-other-any ch-str) ch-str))))

(defun meep--region-mark-bounds-of-char-impl (ch n inner)
  "Mark region bounds of CH, N times.

When INNER is non-nil, mark the inner bounds."
  (let ((bounds-limit (cons (point-min) (point-max)))
        (bounds-init (meep--region-mark-bounds-init))
        (ch-str-pair (meep--region-mark-ch-pair-from-char ch))
        (is-negative
         (cond
          ((< n 0)
           (setq n (- n))
           t)
          (t
           nil))))
    ;; A count of 0 leaves BOUNDS nil for the no-op clause below; running the
    ;; search with a zero count matches at point in both directions and returns the
    ;; degenerate span there rather than a pair.
    (let ((bounds
           (and (not (zerop n))
                (meep--region-mark-bounds-of-char-calc bounds-init bounds-limit n ch-str-pair))))
      (cond
       ;; A count of 0 is a no-op: mark nothing, leaving point and any region as-is.
       ((zerop n)
        nil)
       ((null bounds)
        (message "Unable to find bounds!")
        nil)
       (t
        (let ((is-forward (/= (point) (car bounds-init))))
          (when is-negative
            (setq is-forward (null is-forward)))
          (when inner
            (setcar bounds (+ (car bounds) (length (car ch-str-pair))))
            (setcdr bounds (- (cdr bounds) (length (cdr ch-str-pair)))))
          (meep--region-mark-bounds-to-region bounds is-forward))
        t)))))

(defun meep-region-mark-bounds-of-char-contextual-impl (n inner)
  "Mark region bounds contextually, N times.

When INNER is non-nil, mark the inner bounds."
  (let ((bounds-limit (cons (point-min) (point-max)))
        (bounds-init (meep--region-mark-bounds-init))
        (ch-str-pair nil)
        (is-negative
         (cond
          ((< n 0)
           (setq n (- n))
           t)
          (t
           nil))))
    (let ((bounds nil))
      ;; A count of 0 is a no-op (see `meep--region-mark-bounds-of-char-impl'); skip
      ;; the search so BOUNDS stays nil for the clause below.
      (unless (zerop n)
        (dolist (ch-str-pair-test meep-match-bounds-of-char-contextual)
          (let ((bounds-test
                 (meep--region-mark-bounds-of-char-calc
                  bounds-init bounds-limit n ch-str-pair-test)))
            ;; Keep the innermost across pair types (`meep--surround-bounds-search'
            ;; runs the same reduction).  Only update the clamp beginning, since the
            ;; end may increase and that's OK.
            (when (meep--bounds-supersedes-p bounds-test bounds ch-str-pair)
              (setq bounds bounds-test)
              (setcar bounds-limit (car bounds-test))
              (setq ch-str-pair ch-str-pair-test)))))
      (cond
       ((zerop n)
        nil)
       ((null bounds)
        (message "Unable to find bounds!")
        nil)
       (t
        (let ((is-forward (/= (point) (car bounds-init))))
          (when is-negative
            (setq is-forward (null is-forward)))
          (when inner
            (setcar bounds (+ (car bounds) (length (car ch-str-pair))))
            (setcdr bounds (- (cdr bounds) (length (cdr ch-str-pair)))))
          (meep--region-mark-bounds-to-region bounds is-forward))
        t)))))


;; ---------------------------------------------------------------------------
;; Region Mark: Bounds in Character
;;
;; Support convenient marking of a region in character bounds.
;; This works by prompting for a character which is then scanned in both directions,
;; marking the region in the bounds when it is found.
;;
;; Notes:
;;
;; - Both inner/outer commands are available,
;;   in case you wish to manipulate the region including/excluding the characters.
;; - Entering bracket characters uses matching brackets.
;;   This is customizable with the ``meep-symmetrical-chars`` variable.
;; - Entering an opening ``(`` bracket marks the region inside: ``( ... )``.
;; - Entering a closing ``)`` bracket marks the region inside: ``) ... (``.
;; - A "contextual" version of this function has been implemented which marks the nearest region.
;;   This is customizable with the ``meep-match-bounds-of-char-contextual`` variable.

;;;###autoload
(defun meep-region-mark-bounds-of-char-inner (ch arg)
  "Mark the inner bounds of CH, ARG times.
A negative ARG positions point at the end of the region.

Note that pressing Return instead of a character performs a contextual mark,
finding the closest pair, see: `meep-match-bounds-of-char-contextual'."
  (interactive "cMark inner char:\np")
  ;; NOTE: we could add useful features with other characters.
  (cond
   ((eq ch 13) ; RET.
    (meep-region-mark-bounds-of-char-contextual-inner arg))
   (t
    (meep--char-is-ok-or-error "Mark inner" ch)
    (meep--region-mark-bounds-of-char-impl ch arg t))))

;;;###autoload
(defun meep-region-mark-bounds-of-char-outer (ch arg)
  "Mark the outer bounds of CH, ARG times.
A negative ARG positions point at the end of the region.

Note that pressing Return instead of a character performs a contextual mark,
finding the closest pair, see: `meep-match-bounds-of-char-contextual'."
  (interactive "cMark outer char:\np")
  ;; NOTE: we could add useful features with other characters.
  (cond
   ((eq ch 13) ; RET.
    (meep-region-mark-bounds-of-char-contextual-outer arg))
   (t
    (meep--char-is-ok-or-error "Mark outer" ch)
    (meep--region-mark-bounds-of-char-impl ch arg nil))))

;;;###autoload
(defun meep-region-mark-bounds-of-char-contextual-inner (arg)
  "Mark the inner bounds of the nearest character pairs, ARG times.
A negative ARG positions point at the end of the region.

Character pairs are detected using: `meep-match-bounds-of-char-contextual'."
  (interactive "p")
  (meep-region-mark-bounds-of-char-contextual-impl arg t))

;;;###autoload
(defun meep-region-mark-bounds-of-char-contextual-outer (arg)
  "Mark the outer bounds of the nearest boundary pairs, ARG times.
A negative ARG positions point at the end of the region.

Bounds are detected using: `meep-match-bounds-of-char-contextual'."
  (interactive "p")
  (meep-region-mark-bounds-of-char-contextual-impl arg nil))

;; ---------------------------------------------------------------------------
;; Implementation: Bounds and Motion
;;
;; Each entry in `meep-text-object-alist' maps a text-object KIND to a plist of
;; operations.  See the alist's docstring for the full slot contract.

(defun meep--bounds-of-word (_inner)
  "Return bounds of the word at point, or nil.
INNER is ignored (no inner/outer distinction for words)."
  (declare (important-return-value t))
  (bounds-of-thing-at-point 'word))

(defun meep--bounds-of-symbol (_inner)
  "Return bounds of the symbol at point, or nil.
INNER is ignored (no inner/outer distinction for symbols)."
  (declare (important-return-value t))
  (bounds-of-thing-at-point 'symbol))

(defun meep--bounds-of-defun (inner)
  "Return bounds of the defun at point, or nil.
When INNER is non-nil, contract by surrounding blanks."
  (declare (important-return-value t))
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when (and bounds inner)
      (let ((skip "[:blank:]\r\n"))
        (setq bounds (meep--bounds-contract-by-chars bounds skip skip))))
    bounds))

(defun meep--bounds-of-comment (inner)
  "Return bounds of the comment at point, or nil.
When INNER is non-nil, return the inner bounds (strip delimiters)."
  (declare (important-return-value t))
  (cond
   (inner
    (meep--bounds-at-point-for-comment-inner))
   (t
    (meep--bounds-at-point-for-comment-outer))))

(defun meep--bounds-of-comment-block (inner)
  "Return bounds of the whole comment block at point, or nil.
Extends across adjacent comments separated only by blank/newline blank-space
When INNER is non-nil, strip the opening delimiter of the first comment and
the closing delimiter of the last comment (the `comment' inner behavior
applied to the first/last lines of the block); otherwise include the
trailing newline (outer is line-wise by default)."
  (declare (important-return-value t))
  (when-let* ((first-outer (meep--bounds-at-point-for-comment-outer)))
    (let ((first-beg (car first-outer))
          (last-beg (car first-outer))
          (beg (car first-outer))
          (end (cdr first-outer)))
      ;; Extend end forward across adjacent comments separated only by blanks.
      ;; Remember the last comment's start so we can query its inner bounds.
      (save-excursion
        (let (next)
          (while (progn
                   (goto-char end)
                   (skip-chars-forward " \t\n\r")
                   (setq next (meep--bounds-at-point-for-comment-outer)))
            (setq last-beg (car next))
            (setq end (cdr next)))))
      ;; Extend beg backward across adjacent comments.
      ;; Terminate at BOBP: otherwise the helper would rediscover the
      ;; current comment and the loop would not progress.
      (save-excursion
        (let (prev)
          (while (progn
                   (goto-char beg)
                   (skip-chars-backward " \t\n\r")
                   (and (not (bobp))
                        (progn
                          (forward-char -1)
                          (setq prev (meep--bounds-at-point-for-comment-outer)))))
            (setq first-beg (car prev))
            (setq beg (car prev)))))
      (cond
       (inner
        ;; Apply `comment' inner logic on the first and last comments.
        (save-excursion
          (goto-char first-beg)
          (when-let* ((b (meep--bounds-of-comment t)))
            (setq beg (car b))))
        (save-excursion
          (goto-char last-beg)
          (when-let* ((b (meep--bounds-of-comment t)))
            (setq end (cdr b)))))
       (t
        ;; Outer (default): include trailing newline (line-wise).
        (when (>= (point-max) (1+ end))
          (meep--incf end))))
      (cons beg end))))

(defun meep--bounds-of-string (inner)
  "Return bounds of the string at point, or nil.
When INNER is non-nil, return the inner bounds (inside the quotes)."
  (declare (important-return-value t))
  (cond
   (inner
    (meep--bounds-at-point-for-string-inner))
   (t
    (meep--bounds-at-point-for-string-outer))))

(defun meep--bounds-of-line (inner)
  "Return bounds of the line at point.
When INNER is non-nil, contract by surrounding blanks; otherwise include
the trailing newline when present."
  (declare (important-return-value t))
  (cond
   (inner
    (let ((skip "[:blank:]"))
      (meep--bounds-contract-by-chars (cons (pos-bol) (pos-eol)) skip skip)))
   (t
    (let ((end (pos-eol)))
      (when (>= (point-max) (1+ end))
        (meep--incf end))
      (cons (pos-bol) end)))))

;; ---------------------------------------------------------------------------
;; Implementation: List Item Text Object
;;
;; A "list item" is one top-level element of a bracketed, separated list
;; (e.g. a function call's parameters).  Separators nested inside brackets,
;; strings or comments do not split list items, so given:
;;
;;   foo_fn(a, b, [c, (0, 1)], {e, f, "_"}, "g h")
;;
;; the list items are: a / b / [c, (0, 1)] / {e, f, "_"} / "g h".
;;
;; The recognized brackets and separators are configurable per major mode via
;; `meep-list-item-bounds' (see its docstring and the bundled presets).

(defun meep--list-item-bounds-config ()
  "Return the resolved spec for the `list-item' text object.
See `meep-list-item-bounds' for the spec format and resolution order."
  (declare (important-return-value t))
  (or (meep-preset-ensure-variable 'meep-list-item-bounds) (meep--list-item-bounds-default)))

(defun meep--list-item-comment-skip (limit dir)
  "Step point past a comment abutting it in direction DIR; return non-nil if so.
Point must already sit just past any blank-space.  Clamps to LIMIT; returns
nil with point unmoved when no comment abuts.  Backward, when blank-skipping has
carried point into a line comment's body, point is dropped to the comment start."
  (declare (important-return-value t))
  (let* ((pos (point))
         ;; Backward only: if blank-skip landed us inside a comment's body, this
         ;; is that comment's start position (`nth 8'), else nil.
         (comment-beg
          (and (< dir 0)
               (let ((state (syntax-ppss pos)))
                 (and (nth 4 state) (not (nth 3 state)) (nth 8 state))))))
    (cond
     (comment-beg
      (goto-char (max comment-beg limit)))
     (t
      (forward-comment dir)
      (goto-char
       (cond
        ((> dir 0)
         (min (point) limit))
        (t
         (max (point) limit))))))
    (/= (point) pos)))

(defun meep--list-item-trim-edge (from limit dir)
  "Scan from FROM toward LIMIT, returning the first real-token position.
Skips the blank-space and whole comments padding a list item, never crossing
LIMIT.  DIR is 1 to scan forward (LIMIT after FROM) or -1 backward."
  (declare (important-return-value t))
  ;; TOWARD: still short of LIMIT?  Blank-space is skipped via
  ;; `meep--list-item-skip-blank' (same DIR/LIMIT clamping).
  (let ((toward
         (cond
          ((> dir 0)
           #'<)
          (t
           #'>))))
    (save-excursion
      (save-match-data
        (goto-char (meep--list-item-skip-blank from limit dir))
        ;; Step over each comment that still abuts point, then its blank-space.
        (while (and (funcall toward (point) limit) (meep--list-item-comment-skip limit dir))
          (goto-char (meep--list-item-skip-blank (point) limit dir)))
        (point)))))

(defun meep--list-item-trim (beg end)
  "Return `(BEG . END)' contracted past surrounding blank-space and comments.
A comment adjacent to the list item is trivia and skipped; one *between* tokens
of the same list item is interior and kept.  See `meep--list-item-trim-edge'."
  (declare (important-return-value t))
  (let ((new-beg (meep--list-item-trim-edge beg end 1)))
    (cons new-beg (meep--list-item-trim-edge end new-beg -1))))

(defun meep--list-item-enclosing-bounds (config)
  "Return `(INNER-BEG INNER-END . GROUPS)' of the enclosing list, or nil.
CONFIG is the resolved `meep-list-item-bounds' spec.  Finds the innermost list
whose open bracket is one of CONFIG's pairs by walking the syntax tree's
open-paren stack (`syntax-ppss').  INNER-BEG is just after the open bracket,
INNER-END just before its matching close, and GROUPS the separator groups for
that bracket (one per matching CONFIG entry, tried as fallbacks)."
  (declare (important-return-value t))
  (let ((opens (mapcar #'caar config)))
    (save-excursion
      (let* ((ppss (syntax-ppss))
             ;; `nth 9' lists open-paren positions outermost-first; reversing it
             ;; makes the search run innermost-first, so the first configured
             ;; open it finds is the innermost enclosing list.  Point resting
             ;; *on* an open is not yet on the stack, so fall back to the char at
             ;; point (unless point is in a string or comment).
             (open-beg
              (or (seq-find (lambda (pos) (memq (char-after pos) opens)) (reverse (nth 9 ppss)))
                  (and (not (nth 8 ppss)) (memq (char-after) opens) (point)))))
        (when open-beg
          ;; `scan-sexps' jumps to just past the matching close (nil/error when
          ;; unbalanced).  No containment check is needed: the stack case has
          ;; point inside the bracket, and the at-point case has point *on* the
          ;; open (resolving to the first list item downstream).
          (when-let* ((close-end
                       (ignore-errors
                         (scan-sexps open-beg 1))))
            (let ((open-char (char-after open-beg)))
              (cons
               (1+ open-beg)
               (cons
                (1- close-end)
                (mapcar
                 #'cadr (seq-filter (lambda (entry) (eql (caar entry) open-char)) config)))))))))))

(defun meep--list-item-separators-in (inner-bounds separators)
  "Return an ordered list of `(SBEG . SEND)' for the top-level separators.
SEPARATORS is a list of separator strings, or t to split on runs of whitespace
\(consecutive whitespace counts once, and a run at either edge of the list is
trivia, so no empty items arise).  A separator counts only when its immediately
enclosing bracket is the one enclosing INNER-BOUNDS; one nested in a deeper
bracket, string or comment is skipped."
  (declare (important-return-value t))
  (let* ((by-whitespace (eq separators t))
         (re
          (cond
           (by-whitespace
            "[[:blank:]\r\n]+")
           (t
            (regexp-opt separators))))
         (beg (car inner-bounds))
         (end (cdr inner-bounds))
         ;; INNER-BEG is always just inside the enclosing open bracket, so that
         ;; bracket's position is `(1- beg)' - no need to read it back from the
         ;; syntax tree.  Separators whose enclosing open differs are nested.
         (list-open (1- beg))
         (result nil))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward re end t)
          (let* ((mbeg (match-beginning 0))
                 (mend (match-end 0))
                 (state (save-excursion (syntax-ppss mbeg)))
                 (open (nth 1 state)))
            (cond
             ;; Nested inside a deeper bracket: jump past the whole bracketed
             ;; sub-expression so its interior separators are never considered.
             ((and open (not (eql open list-open)))
              (when-let* ((close
                           (ignore-errors
                             (scan-sexps open 1))))
                (goto-char (min close end))))
             ;; A real separator: at the list level (not in a string/comment),
             ;; and - for whitespace - not the leading or trailing run, which is
             ;; edge trivia rather than an item boundary.
             ((and (not (nth 8 state)) (not (and by-whitespace (or (= mbeg beg) (= mend end)))))
              (push (cons mbeg mend) result)))))))
    (nreverse result)))

(defun meep--list-item-separators-for (inner-bounds groups)
  "Return the `(SBEG . SEND)' separators within INNER-BOUNDS, trying GROUPS.
GROUPS is a list of separator groups, each tried in order; the first that occurs
in INNER-BOUNDS wins (the separator fallback chain - see `meep-list-item-bounds').
Returns nil when no group matches, making the list a single list item."
  (declare (important-return-value t))
  (seq-some (lambda (group) (meep--list-item-separators-in inner-bounds group)) groups))

(defun meep--list-item-cells (inner-bounds seps)
  "Return the ordered list-item cells within INNER-BOUNDS.
SEPS is the precomputed `(SBEG . SEND)' separator list (see
`meep--list-item-separators-for').  Each cell is `(BEG END PREV NEXT)': BEG and
END are the list item's trimmed bounds (see `meep--list-item-trim'); PREV and
NEXT are the bounding separators, or nil at the list's first/last edge.  An empty
SEPS yields a single trimmed cell spanning the whole inner range."
  (declare (important-return-value t))
  (let ((end (cdr inner-bounds))
        (lo (car inner-bounds))
        (prev nil)
        (cells nil))
    (dolist (s seps)
      (let ((bounds (meep--list-item-trim lo (car s))))
        (push (list (car bounds) (cdr bounds) prev s) cells))
      (setq prev s)
      (setq lo (cdr s)))
    (let ((final-bounds (meep--list-item-trim lo end)))
      ;; Drop the final cell when a trailing separator leaves only blank-space
      ;; before the close (a phantom empty list item).  A separator-less list
      ;; still yields its single (possibly empty) cell.
      (unless (and seps (= (car final-bounds) (cdr final-bounds)))
        (push (list (car final-bounds) (cdr final-bounds) prev nil) cells)))
    (nreverse cells)))

(defun meep--list-item-cell-at (cells origin)
  "Return the cell in CELLS that owns position ORIGIN, or nil when CELLS is nil.
CELLS is the descriptor list from `meep--list-item-cells'.  A cell owns every
position from just after its PREV separator onward (the first cell, PREV nil,
owns from the inner start), so point on a separator or in an item's leading
blank-space/comment resolves to the adjacent real list item."
  (declare (important-return-value t))
  ;; The first cell (PREV nil) owns from the inner start, so RESULT is never left
  ;; nil for a non-empty CELLS.  Ownership (PREV separator at/before ORIGIN) is
  ;; monotonic over the ordered cells, so stop at the first cell it fails for.
  (let ((result (car cells))
        (rest (cdr cells)))
    (while (and rest
                (let ((prev (nth 2 (car rest))))
                  (or (null prev) (<= (cdr prev) origin))))
      (setq result (pop rest)))
    result))

(defun meep--list-item-skip-blank (from limit dir)
  "Return the position reached skipping blank-space from FROM toward LIMIT.
DIR is 1 to scan forward or -1 backward; the scan never crosses LIMIT.
Blank-space is spaces, tabs and newlines."
  (declare (important-return-value t))
  (save-excursion
    (goto-char from)
    (cond
     ((> dir 0)
      (skip-chars-forward "[:blank:]\r\n" limit))
     (t
      (skip-chars-backward "[:blank:]\r\n" limit)))
    (point)))

(defun meep--list-item-cells-at-point (config)
  "Return `(LIST-INNER . CELLS)' for the list enclosing point, or nil.
CONFIG is the resolved `meep-list-item-bounds' spec.  LIST-INNER is the
`(BEG . END)' inner span (see `meep--list-item-enclosing-bounds'); CELLS its
list-item descriptors (see `meep--list-item-cells')."
  (declare (important-return-value t))
  (when-let* ((enc (meep--list-item-enclosing-bounds config)))
    (let* ((list-inner (cons (car enc) (cadr enc)))
           (seps (meep--list-item-separators-for list-inner (cddr enc))))
      (cons list-inner (meep--list-item-cells list-inner seps)))))

(defun meep--list-item-outer-bounds-for-cell (cell list-beg list-end)
  "Return the outer `(BEG . END)' bounds of CELL within LIST-BEG..LIST-END.
CELL is a descriptor from `meep--list-item-cells'.  A non-final list item
absorbs its trailing separator and the blank-space after it; the final
list item absorbs its leading separator and preceding blank-space; a sole
list item absorbs nothing, so removing the bounds leaves a well-formed list."
  (declare (important-return-value t))
  (let ((cell-beg (nth 0 cell))
        (cell-end (nth 1 cell))
        (prev (nth 2 cell))
        (next (nth 3 cell)))
    (cond
     (next
      (cons cell-beg (meep--list-item-skip-blank (cdr next) list-end 1)))
     (prev
      (cons (meep--list-item-skip-blank (car prev) list-beg -1) cell-end))
     (t
      (cons cell-beg cell-end)))))

(defun meep--list-item-bounds-from-cells (list-cells inner)
  "Return the list-item bounds at point from precomputed LIST-CELLS, or nil.
LIST-CELLS is `(LIST-INNER . CELLS)' from `meep--list-item-cells-at-point'.  When
INNER is non-nil return just the list item, otherwise its outer bounds (see
`meep--list-item-outer-bounds-for-cell')."
  (declare (important-return-value t))
  (let* ((list-inner (car list-cells))
         (cell (meep--list-item-cell-at (cdr list-cells) (point))))
    (cond
     (inner
      (cons (nth 0 cell) (nth 1 cell)))
     (t
      (meep--list-item-outer-bounds-for-cell cell (car list-inner) (cdr list-inner))))))

(defun meep--bounds-of-list-item (inner)
  "Return bounds of the list item at point, or nil.
When INNER is non-nil return just the list item, otherwise its outer bounds (see
`meep--list-item-outer-bounds-for-cell').
Recognized brackets and separators come from `meep-list-item-bounds'.

Returns nil only when point is not inside a recognized list.  An empty list or a
leading empty slot (e.g. `(,a)') yields empty bounds (BEG = END), not nil."
  (declare (important-return-value t))
  (when-let* ((list-cells (meep--list-item-cells-at-point (meep--list-item-bounds-config))))
    (meep--list-item-bounds-from-cells list-cells inner)))

(defun meep--list-item-outer-ending-at (list-cells pos)
  "Return outer list-item bounds from LIST-CELLS whose end is exactly POS, or nil.
LIST-CELLS is `(LIST-INNER . CELLS)' from `meep--list-item-cells-at-point'.  A
backward outer move uses this to step to the list item *ending* at point rather
than the one starting there, mirroring gap-separated text objects so that
`meep-region-activate-and-reverse-motion' reconstructs the whole outer span."
  (declare (important-return-value t))
  (let ((list-inner (car list-cells)))
    (seq-some
     (lambda (cell)
       (let ((bounds
              (meep--list-item-outer-bounds-for-cell cell (car list-inner) (cdr list-inner))))
         (and (eql (cdr bounds) pos) bounds)))
     (cdr list-cells))))

(defun meep--char-space-p (ch)
  "Return non-nil if CH is blank-space for `char' text-object purposes.
Includes space, tab, newline, carriage return; nil (buffer edge) counts
as blank-space"
  (declare (important-return-value t))
  (or (null ch) (memq ch '(?\s ?\t ?\n ?\r))))


;; ---------------------------------------------------------------------------
;; Implementation: Text Object Motions

(defun meep--search-syntax-forward (step ppss-index)
  "Move to the start of the STEP'th next region matching `syntax-ppss'.
STEP is the signed count of regions to step over (positive forward,
negative backward).  PPSS-INDEX is 3 (string) or 4 (comment).  Returns
the signed count of STEP that could not be advanced; mirrors `forward-line'.
On partial success point is left at the last region found.
Uses Emacs's `comment-search-{forward,backward}' for comments and a
string-syntax regexp (`\\\\s\"') for strings, with `syntax-ppss' validation."
  (declare (important-return-value t))
  (let* ((sign
          (cond
           ((> step 0)
            1)
           ((< step 0)
            -1)
           (t
            0)))
         (remaining (abs step))
         (more t)
         ;; PRIMITIVE returns the next candidate region's start (or nil when
         ;; no more matches in direction).  Comments use the mode-aware
         ;; built-in search; strings use a syntax-class regex with `ppss'
         ;; validation (skipping closing quotes, escapes, etc.).
         (primitive
          (cond
           ((eq ppss-index 4)
            (cond
             ((> sign 0)
              (lambda () (comment-search-forward (point-max) t)))
             (t
              (lambda () (comment-search-backward (point-min) t)))))
           (t
            (cond
             ((> sign 0)
              (lambda ()
                (let ((beg nil))
                  (while (and (null beg) (re-search-forward "\\s\"" nil t))
                    (let ((s (syntax-ppss (point))))
                      (when (and (nth ppss-index s) (eql (nth 8 s) (1- (point))))
                        (setq beg (1- (point))))))
                  beg)))
             (t
              (lambda ()
                (let ((beg nil))
                  (while (and (null beg) (re-search-backward "\\s\"" nil t))
                    (let ((s (syntax-ppss (1+ (point)))))
                      (when (and (nth ppss-index s) (eql (nth 8 s) (point)))
                        (setq beg (point)))))
                  beg))))))))
    (while (and more (not (zerop remaining)))
      (let ((p (point))
            ;; CUR-BEG is the start of the region we're already in (or about
            ;; to enter at point).  Look one char ahead for forward search
            ;; so "at the start of a region" (where ppss hasn't yet crossed
            ;; the opener) is treated as inside, avoiding re-discovery.
            (cur-beg
             (let ((state (syntax-ppss)))
               (or (and (nth ppss-index state) (nth 8 state))
                   (and (> sign 0)
                        (not (eobp))
                        (let ((s (syntax-ppss (1+ (point)))))
                          (and (nth ppss-index s) (nth 8 s)))))))
            (beg nil)
            (cont t))
        (while (and (null beg) cont)
          (let ((found (funcall primitive)))
            (cond
             ((null found)
              (setq cont nil))
             ((eql found cur-beg)
              nil)
             (t
              (setq beg found)))))
        (cond
         (beg
          (goto-char beg)
          (meep--decf remaining))
         (t
          ;; No more regions in this direction; abort, restoring this
          ;; iteration's start so partial-advance lands at the last hit.
          (goto-char p)
          (setq more nil)))))
    (* sign remaining)))

(defun meep--bounds-endpoint (bounds at-start)
  "Return (car BOUNDS) when AT-START is non-nil, else (cdr BOUNDS)."
  (declare (important-return-value t))
  (cond
   (at-start
    (car bounds))
   (t
    (cdr bounds))))

(defun meep--forward-multi (step forward-1-fn)
  "Iterate FORWARD-1-FN one step at a time, STEP times, returning the unadvanced remainder.
FORWARD-1-FN takes a signed step (+1 or -1) and advances point to the
next object (a no-op if no further object exists).  Used to wrap
primitives whose own return value isn't a remainder count
\(e.g. `forward-thing', which returns t/nil)."
  (declare (important-return-value t))
  (cond
   ((zerop step)
    0)
   (t
    (let ((sign
           (cond
            ((> step 0)
             1)
            (t
             -1)))
          (remaining (abs step))
          (more t))
      (while (and more (not (zerop remaining)))
        (let ((p (point)))
          (funcall forward-1-fn sign)
          (cond
           ((eq p (point))
            (setq more nil))
           (t
            (meep--decf remaining)))))
      (* sign remaining)))))

(defun meep--scan-walk (step forward-fn)
  "Scan STEP objects via FORWARD-FN, returning `(LANDING-POS . REMAINING)'.
Does not move point (uses `save-excursion' internally).
FORWARD-FN takes a signed STEP and advances point that many objects,
returning the signed count of steps that could not be advanced (mirrors
`forward-line').  Wrap primitives that don't return a remainder via
`meep--forward-multi'."
  (declare (important-return-value t))
  (cond
   ((zerop step)
    (cons (point) 0))
   (t
    (save-excursion
      (let ((unstepped (funcall forward-fn step)))
        (cons (point) unstepped))))))

(defun meep--scan-walk-bounds (inner step at-start forward-fn bounds-fn)
  "Scan STEP objects then normalize the landing to the AT-START side of bounds.
FORWARD-FN scans (see `meep--scan-walk').  After landing, BOUNDS-FN is
called with INNER and the AT-START side of those bounds is the final
landing.  Returns `(LANDING-POS . REMAINING)'.  If BOUNDS-FN returns nil
at the landing (no object there), the primitive landing is returned."
  (declare (important-return-value t))
  (let* ((result (meep--scan-walk step forward-fn))
         (pos (car result)))
    (when pos
      (when-let* ((b
                   (save-excursion
                     (goto-char pos)
                     (funcall bounds-fn inner))))
        (setq pos (meep--bounds-endpoint b at-start))))
    (cons pos (cdr result))))

(defun meep--scan-walk-thing (inner step at-start forward-fn bounds-fn)
  "Scan via FORWARD-FN; skip bounds normalization when not needed.
Optimized for `forward-thing'-style primitives whose natural landing is
the trailing edge in the direction of motion (forward -> end, backward ->
start).  Skips the BOUNDS-FN call (and so ignores INNER) when STEP is
non-zero and AT-START matches the primitive's natural landing for that
direction."
  (declare (important-return-value t))
  (cond
   ((and (not (zerop step)) (eq at-start (< step 0)))
    (meep--scan-walk step forward-fn))
   (t
    (meep--scan-walk-bounds inner step at-start forward-fn bounds-fn))))

(defun meep--scan-walk-leading (inner step at-start forward-fn bounds-fn)
  "Scan via FORWARD-FN; skip bounds normalization when not needed.
Optimized for primitives whose natural landing is always the start of an
object (`forward-line', `vertical-motion', `forward-char',
`meep--search-syntax-forward').  Skips the BOUNDS-FN call (and so ignores
INNER) when STEP is non-zero and AT-START is non-nil."
  (declare (important-return-value t))
  (cond
   ((and (not (zerop step)) at-start)
    (meep--scan-walk step forward-fn))
   (t
    (meep--scan-walk-bounds inner step at-start forward-fn bounds-fn))))

(defun meep--bounds-step-word (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `word' text object; scan STEP words."
  (declare (important-return-value t))
  (meep--scan-walk-thing
   inner
   step
   at-start
   (lambda (s) (meep--forward-multi s (lambda (n) (forward-thing 'word n))))
   #'meep--bounds-of-word))

(defun meep--bounds-step-symbol (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `symbol' text object; scan STEP symbols."
  (declare (important-return-value t))
  (meep--scan-walk-thing
   inner step at-start
   (lambda (s)
     (meep--forward-multi s (lambda (n) (forward-thing 'symbol n))))
   #'meep--bounds-of-symbol))

(defun meep--bounds-step-defun (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `defun' text object; scan STEP defuns."
  (declare (important-return-value t))
  (meep--scan-walk-thing
   inner
   step
   at-start
   (lambda (s) (meep--forward-multi s (lambda (n) (forward-thing 'defun n))))
   #'meep--bounds-of-defun))

(defun meep--bounds-step-sentence (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `sentence' text object; scan STEP sentences."
  (declare (important-return-value t))
  (meep--scan-walk-thing
   inner step at-start
   (lambda (s)
     (meep--forward-multi s (lambda (n) (forward-thing 'sentence n))))
   #'meep--bounds-of-sentence))

(defun meep--bounds-step-paragraph (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `paragraph' text object; scan STEP paragraphs."
  (declare (important-return-value t))
  (meep--scan-walk-thing
   inner step at-start
   (lambda (s)
     (meep--forward-multi s (lambda (n) (forward-thing 'paragraph n))))
   #'meep--bounds-of-paragraph))

(defun meep--bounds-step-comment (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `comment' text object; scan STEP comments."
  (declare (important-return-value t))
  (meep--scan-walk-leading
   inner step at-start (lambda (s) (meep--search-syntax-forward s 4)) #'meep--bounds-of-comment))

(defun meep--bounds-step-comment-block (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `comment-block' text object; scan STEP blocks."
  (declare (important-return-value t))
  (meep--scan-walk-leading
   inner
   step
   at-start
   (lambda (s) (meep--search-syntax-forward s 4))
   #'meep--bounds-of-comment-block))

(defun meep--bounds-step-string (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `string' text object; scan STEP strings."
  (declare (important-return-value t))
  (meep--scan-walk-leading
   inner step at-start (lambda (s) (meep--search-syntax-forward s 3)) #'meep--bounds-of-string))

(defun meep--bounds-step-line (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `line' text object; scan STEP lines."
  (declare (important-return-value t))
  (meep--scan-walk-leading inner step at-start #'forward-line #'meep--bounds-of-line))

(defun meep--bounds-step-visual-line (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `visual-line' text object; scan STEP visual lines."
  (declare (important-return-value t))
  (meep--scan-walk-leading
   inner step at-start (lambda (s) (- s (vertical-motion s))) #'meep--bounds-of-visual-line))

(defun meep--forward-list-item (step)
  "Move point like `forward-thing' over list items, STEP times (signed).
Forward motion lands on a list item's end, backward on its start, counting the
current list item as the first step.  Bounded to the enclosing list; stepping
past the last/first item is a no-op, left in the returned remainder."
  (declare (important-return-value t))
  ;; The enclosing list - and hence its cells - is invariant across the whole
  ;; motion (every landing stays inside it), so resolve it once rather than
  ;; rebuilding it on each step.
  (let ((list-cells (meep--list-item-cells-at-point (meep--list-item-bounds-config))))
    (cond
     (list-cells
      ;; CELLS-REV is only consumed by backward steps; reverse once here rather
      ;; than on every step inside `meep--forward-multi's loop.
      (let* ((cells (cdr list-cells))
             (cells-rev (reverse cells)))
        (meep--forward-multi
         step
         (lambda (dir)
           (let* ((origin (point))
                  ;; Forward: end of the first list item ending past point.
                  ;; Backward: start of the last starting before point - the same
                  ;; "first match" over CELLS reversed.
                  (cell
                   (cond
                    ((> dir 0)
                     (seq-find (lambda (c) (> (nth 1 c) origin)) cells))
                    (t
                     (seq-find (lambda (c) (< (nth 0 c) origin)) cells-rev)))))
             (when cell
               (goto-char
                (nth
                 (cond
                  ((> dir 0)
                   1)
                  (t
                   0))
                 cell))))))))
     ;; No enclosing list: nothing stepped, so the full STEP is the remainder
     ;; (matching `meep--forward-multi's contract for a no-op).
     (t
      step))))

(defun meep--bounds-step-list-item (inner step at-start)
  "`:bounds-step-fn' (INNER STEP AT-START) for the `list-item' text object; scan STEP list items."
  (declare (important-return-value t))
  (meep--scan-walk-thing
   inner step at-start #'meep--forward-list-item #'meep--bounds-of-list-item))

;; ---------------------------------------------------------------------------
;; Implementation: Text Object Registry & Dispatch

(defvar meep-text-object-alist
  (list
   (cons
    'word
    (list :bounds-fn #'meep--bounds-of-word :bounds-step-fn #'meep--bounds-step-word :no-inner t))
   (cons
    'symbol
    (list
     :bounds-fn #'meep--bounds-of-symbol
     :bounds-step-fn #'meep--bounds-step-symbol
     :no-inner t))
   (cons
    'sentence
    (list :bounds-fn #'meep--bounds-of-sentence :bounds-step-fn #'meep--bounds-step-sentence))
   (cons
    'paragraph
    (list :bounds-fn #'meep--bounds-of-paragraph :bounds-step-fn #'meep--bounds-step-paragraph))
   (cons
    'comment
    (list :bounds-fn #'meep--bounds-of-comment :bounds-step-fn #'meep--bounds-step-comment))
   (cons
    'comment-block
    (list
     :bounds-fn #'meep--bounds-of-comment-block
     :bounds-step-fn #'meep--bounds-step-comment-block))
   (cons
    'string (list :bounds-fn #'meep--bounds-of-string :bounds-step-fn #'meep--bounds-step-string))
   (cons
    'defun (list :bounds-fn #'meep--bounds-of-defun :bounds-step-fn #'meep--bounds-step-defun))
   (cons 'line (list :bounds-fn #'meep--bounds-of-line :bounds-step-fn #'meep--bounds-step-line))
   (cons
    'visual-line
    (list
     :bounds-fn #'meep--bounds-of-visual-line
     :bounds-step-fn #'meep--bounds-step-visual-line))
   (cons
    'list-item
    (list :bounds-fn #'meep--bounds-of-list-item :bounds-step-fn #'meep--bounds-step-list-item)))
  "Alist mapping a text-object KIND to a plist of operations.
Plist keys:
  :bounds-fn (INNER) -> BOUNDS
    Pure query: return `(BEG . END)' of the object at point, or nil.
  :bounds-step-fn (INNER STEP AT-START) -> (LANDING-POS . REMAINING)
    Pure query (does not move point): scan STEP objects forward (backward
    if negative) and return the precise landing plus the signed count of
    steps that could not be advanced.
    LANDING-POS is the AT-START side (start when non-nil, end otherwise)
    of the target object's INNER bounds;
    the motion dispatcher uses it directly without further lookup.
    Implementations may skip the bounds-normalization step when the
    underlying primitive's natural landing already matches AT-START.
  :no-inner (optional)
    When non-nil, the kind has no meaningful inner variant (its `:bounds-fn'
    ignores INNER, e.g. word, symbol).  Consumers (e.g. the mark commands)
    may generate only an `-outer' variant and skip the `-inner' one.
Extension packages may extend this alist to register new kinds.")

;;;###autoload
(defun meep-calc-bounds-at-point (kind inner)
  "Return `(BEG . END)' for the text-object KIND at point, or nil.
When INNER is non-nil return the inner bounds, otherwise the outer bounds.
Dispatches via `meep-text-object-alist's :bounds-fn slot.
Returns nil when KIND is not registered."
  (declare (important-return-value t))
  (when-let* ((plist (alist-get kind meep-text-object-alist))
              (fn (plist-get plist :bounds-fn)))
    (funcall fn inner)))

;;;###autoload
(defun meep-motion-at-point (kind inner steps at-start &optional skip-current)
  "Move by STEPS objects of text-object KIND.
STEPS=0 lands at the current object's start/end without walking.
AT-START non-nil lands at the start, nil at the end.
When SKIP-CURRENT is non-nil, exit the current object before walking so
STEPS counts distinct objects forward/backward (strict-next).  When nil,
the underlying primitive counts the current object as step 1 (vim-style
word motion).  Skip-current is meaningful only for kinds whose primitive
lands within the current object (word, symbol, etc.); using it with
self-stepping primitives (line, char, visual-line) will skip an object.
INNER's meaning is kind-specific (e.g. for `string', inside vs outside
the quotes).  Returns the signed count of steps that could not be advanced,
or nil when KIND is not registered.
Dispatches via `meep-text-object-alist's :bounds-step-fn slot (plus
:bounds-fn for optional step-out)."
  (declare (important-return-value t))
  (when-let* ((plist (alist-get kind meep-text-object-alist))
              (step-fn (plist-get plist :bounds-step-fn))
              (bounds-fn (plist-get plist :bounds-fn)))
    (let* ((do-step-out (and skip-current (not (zerop steps))))
           ;; Outer save-excursion is load-bearing: it unwinds the step-out
           ;; motion when the scan can't advance, so we don't leave point at
           ;; the stepped-out position.  The inner scan does its own
           ;; save-excursion.
           (result
            (save-excursion
              ;; Step-out uses the *outer* bounds (INNER=nil) so we truly
              ;; exit the current object; step-fn below applies the caller's
              ;; INNER and AT-START for the final landing.  Step out to the
              ;; start when moving backward, to the end when moving forward.
              (when do-step-out
                (when-let* ((b (funcall bounds-fn nil)))
                  (goto-char (meep--bounds-endpoint b (< steps 0)))))
              (funcall step-fn inner steps at-start)))
           (target-pos (car result))
           (remaining (cdr result))
           (steps-taken (- (abs steps) (abs remaining))))
      ;; Advance when we asked for nothing, or when we took some steps.
      (when (and target-pos (or (zerop steps) (> steps-taken 0)))
        (goto-char target-pos))
      remaining)))

;; ---------------------------------------------------------------------------
;; Motion: Bounds

;;;###autoload
(defun meep-move-to-bounds-of-sentence (arg &optional inner)
  "Move to the sentence start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound."
  (interactive "^p")
  (let ((bounds (meep--bounds-of-sentence inner)))
    (cond
     (bounds
      (meep--move-to-bounds-endpoint bounds arg))
     (t
      (message "Not found: bounds of sentence")
      nil))))
;;;###autoload
(defun meep-move-to-bounds-of-sentence-inner (arg)
  "Move to the inner sentence start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-sentence arg t))

;;;###autoload
(defun meep-move-to-bounds-of-paragraph (arg &optional inner)
  "Move to the paragraph start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound."
  (interactive "^p")
  (let ((bounds (meep--bounds-of-paragraph inner)))
    (cond
     (bounds
      (meep--move-to-bounds-endpoint bounds arg))
     (t
      (message "Not found: bounds of paragraph")
      nil))))
;;;###autoload
(defun meep-move-to-bounds-of-paragraph-inner (arg)
  "Move to the inner paragraph start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-paragraph arg t))

;;;###autoload
(defun meep-move-to-bounds-of-comment (arg &optional inner)
  "Move to the comment start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound."
  (interactive "^p")
  (let ((bounds
         (cond
          (inner
           (meep--bounds-at-point-for-comment-inner))
          (t
           (meep--bounds-at-point-for-comment-outer)))))
    (cond
     (bounds
      (meep--move-to-bounds-endpoint bounds arg))
     (t
      (message "Not found: bounds of comment")
      nil))))
;;;###autoload
(defun meep-move-to-bounds-of-comment-inner (arg)
  "Move to the comment inner start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-comment arg t))

;;;###autoload
(defun meep-move-to-bounds-of-string (arg &optional inner)
  "Move to the string start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound."
  (interactive "^p")
  (let ((bounds
         (cond
          (inner
           (meep--bounds-at-point-for-string-inner))
          (t
           (meep--bounds-at-point-for-string-outer)))))
    (cond
     (bounds
      (meep--move-to-bounds-endpoint bounds arg))
     (t
      (message "Not found: bounds of string")
      nil))))
;;;###autoload
(defun meep-move-to-bounds-of-string-inner (arg)
  "Move to the string inner start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-string arg t))

;;;###autoload
(defun meep-move-to-bounds-of-defun (arg &optional inner)
  "Move to the function start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound."
  (interactive "^p")
  (ignore inner) ;; TODO: support inner.
  (meep--move-to-bounds-of-thing 'defun arg))
;;;###autoload
(defun meep-move-to-bounds-of-defun-inner (arg)
  "Move to the inner function start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-defun arg t))

;;;###autoload
(defun meep-move-to-bounds-of-line (arg &optional inner)
  "Move to the line start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound."
  (interactive "^p")
  (let ((bounds (cons (pos-bol) (pos-eol))))
    (when inner
      (let ((skip "[:blank:]"))
        (setq bounds (meep--bounds-contract-by-chars bounds skip skip))))
    (meep--move-to-bounds-endpoint bounds arg)))

;;;###autoload
(defun meep-move-to-bounds-of-line-inner (arg)
  "Move to the inner line start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-line arg t))

;;;###autoload
(defun meep-move-to-bounds-of-visual-line (arg &optional inner)
  "Move to the visual-line start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound."
  (interactive "^p")
  (let ((bounds (meep--bounds-of-visual-line inner)))
    ;; No need to check for nil.
    (meep--move-to-bounds-endpoint bounds arg)))
;;;###autoload
(defun meep-move-to-bounds-of-visual-line-inner (arg)
  "Move to the inner visual-line start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-visual-line arg t))

;;;###autoload
(defun meep-move-to-bounds-of-list-item (arg &optional inner)
  "Move to the list item start/end (start when ARG is negative).
When INNER is non-nil, move to the inner bound.
Recognized brackets and separators come from `meep-list-item-bounds'."
  (interactive "^p")
  ;; Resolve the enclosing list (and its cells) once: the same LIST-CELLS serves
  ;; both the backward-outer "ending at point" lookup and the plain
  ;; bounds-at-point, so the list is `scan-sexps'-ed only once.
  (let ((list-cells (meep--list-item-cells-at-point (meep--list-item-bounds-config))))
    (cond
     (list-cells
      (let ((bounds
             (or
              ;; A backward outer move from a list item's trailing edge targets
              ;; the list item ending there, not the adjacent one starting there.
              ;; This makes the motion step to the previous list item at a
              ;; boundary (as symbol/word motions do) so move + reverse-motion
              ;; reconstructs the whole outer span; see
              ;; `meep--list-item-outer-ending-at'.
              (and (< arg 0) (not inner) (meep--list-item-outer-ending-at list-cells (point)))
              (meep--list-item-bounds-from-cells list-cells inner))))
        (meep--move-to-bounds-endpoint bounds arg)))
     (t
      (message "Not found: bounds of list item")
      nil))))
;;;###autoload
(defun meep-move-to-bounds-of-list-item-inner (arg)
  "Move to the inner list item start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-list-item arg t))

(defcustom meep-bounds-commands
  '((?p meep-move-to-bounds-of-paragraph-inner "paragraph inner")
    (?P meep-move-to-bounds-of-paragraph "paragraphs")
    (?c meep-move-to-bounds-of-comment-inner "comment inner")
    (?C meep-move-to-bounds-of-comment "comment")
    (?s meep-move-to-bounds-of-string-inner "string inner")
    (?S meep-move-to-bounds-of-string "string")
    (?l meep-move-to-bounds-of-line-inner "line inner")
    (?L meep-move-to-bounds-of-line "line")
    (?V meep-move-to-bounds-of-visual-line-inner "visual line inner")
    (?v meep-move-to-bounds-of-visual-line "visual line")
    (?d meep-move-to-bounds-of-defun-inner "defun inner")
    (?D meep-move-to-bounds-of-defun "defun")
    (?i meep-move-to-bounds-of-list-item-inner "list item inner")
    (?I meep-move-to-bounds-of-list-item "list item")
    (?\. meep-move-to-bounds-of-sentence-inner "sentence inner")
    (?> meep-move-to-bounds-of-sentence "sentence"))
  "List of commands for bounds movement.
Each element is (key function description)."
  :type
  '(repeat
    (list
     :tag
     "Command"
     (character :tag "Key")
     (function :tag "Function")
     (string :tag "Description"))))

(defun meep--set-transient-map-echo (km live escaped)
  "Activate KM as a transient map echoing LIVE then the `%'-escaped ESCAPED.
`set-transient-map' reads its message through `format-spec', so a literal `%' in
user-interpolated text (a bound key or a symbol name) would be an invalid spec and
error; ESCAPED is that text, with each `%' doubled.  LIVE is fixed message text
where a `%k' key-list hint stays live for `format-spec' to expand."
  (set-transient-map km nil nil (concat live (string-replace "%" "%%" escaped))))

(defun meep--move-bounds-of-thing-impl (n)
  "Initiate a bounds motion, forward when N is positive."
  (let ((km nil)
        (info-text
         (mapcar
          (lambda (cmd) (format "%s: %s" (string (nth 0 cmd)) (nth 2 cmd))) meep-bounds-commands)))
    (setq km (make-sparse-keymap))
    (when (< n 0)
      (setq prefix-arg -1))
    (dolist (cmd (reverse meep-bounds-commands))
      (keymap-set km (string (nth 0 cmd)) (nth 1 cmd)))
    (meep--set-transient-map-echo
     km
     (concat
      (format "Jump to the %s of"
              (or (and (< n 0) "beginning") "end"))
      ": %k or any other to exit\n")
     (mapconcat #'identity info-text ", "))))

;;;###autoload
(defun meep-move-to-bounds-of-thing-beginning (arg)
  "Move to inner bounds of thing (beginning).
Move to the end with a negative ARG."
  (interactive "^p")
  (meep--move-bounds-of-thing-impl (- arg)))

;;;###autoload
(defun meep-move-to-bounds-of-thing-end (arg)
  "Move to inner bounds of thing (end).
Move to the beginning with a negative ARG."
  (interactive "^p")
  (meep--move-bounds-of-thing-impl arg))


;; ---------------------------------------------------------------------------
;; Selection/Region: Primitive

;;;###autoload
(defun meep-region-enable ()
  "Enable the active region.

The mark is moved to point to begin a new selection.
If you wish to activate the region between the existing point and mark see:
`meep-region-activate-and-reverse' and `meep-region-activate-or-reverse'."
  (interactive)
  (unless (region-active-p)
    ;; This may have been set, clear it if it was.
    (setq meep-state-region-elem nil)
    ;; Begin selecting (set the mark to the point's location).
    ;; Use `meep-region-activate-and-reverse' to activate the region with the old mark.
    (set-mark (point))
    (activate-mark t)))

;;;###autoload
(defun meep-region-enable-rectangle ()
  "Enable rectangle mark mode."
  (interactive)
  (rectangle-mark-mode 1))

;;;###autoload
(defun meep-region-toggle-rectangle ()
  "Toggle rectangle mark mode."
  (interactive)
  (rectangle-mark-mode))

;;;###autoload
(defun meep-region-activate-or-reverse ()
  "Activate the region without moving the mark.

Otherwise exchange point and mark when the region is already active.
See: `meep-region-activate-and-reverse'."
  (interactive)
  (cond
   ((region-active-p)
    (meep-region-activate-and-reverse))
   ((mark)
    (activate-mark t))
   (t
    (meep-region-enable))))

;;;###autoload
(defun meep-region-disable ()
  "Disable the active region.

The mark is not moved, the region can be restored
via `meep-region-activate-or-reverse' or `meep-region-activate-and-reverse'."
  (interactive)
  (when (region-active-p)
    (cond
     ((eq (car-safe transient-mark-mode) 'only)
      ;; Move from shift-select to "permanent" select.
      (setq transient-mark-mode t))
     (t
      (deactivate-mark t)))))

;;;###autoload
(defun meep-region-toggle ()
  "Toggle the active region.

When the region is transient (where motion would clear it),
this operation makes it stay active, running again clears it."
  (interactive)
  (cond
   ((region-active-p)
    (meep-region-disable))
   (t
    (meep-region-enable))))

;;;###autoload
(defun meep-region-activate-and-reverse ()
  "Exchange point and mark, activating the region.

To first activate the region without exchanging point and mark:
See: `meep-region-activate-or-reverse'.

Note that this wraps Emacs built-in: `exchange-point-and-mark'."
  (interactive)
  ;; This will activate the selection if it's not already selected,
  ;; it allows re-selecting pasted text for example.
  (setq meep-mark-adjust (point))
  (exchange-point-and-mark))

(defun meep--last-motion-calc-whole-mark-pos (use-adjust-mark)
  "Return the mark position needed to select the whole object after a partial motion.

When USE-ADJUST-MARK is non-nil, use the previous point of adjust commands."
  (declare (important-return-value t))
  (let ((local-last-command (meep--last-command))
        (local-last-prefix-arg (meep--last-prefix-arg))
        (local-mrk (mark))
        (new-mrk nil)
        (prefix "region-activate-and-reverse-motion"))

    (when (and use-adjust-mark
               meep-mark-adjust
               (symbolp local-last-command)
               (meep-command-is-mark-set-on-motion-adjust local-last-command))
      (setq local-mrk meep-mark-adjust))

    (cond
     ((null local-mrk)
      (message "%s: failed, no mark found, the mark is expected to be set by: %S"
               prefix
               local-last-command)
      nil)
     ((null local-last-command)
      (message "%s: failed, no last-command found" prefix)
      nil)
     ((null (symbolp local-last-command))
      (message "%s: failed, the last-command must be a symbol, not %S"
               prefix
               (type-of local-last-command))
      nil)
     ((eq local-last-command 'meep-region-activate-and-reverse-motion)
      ;; This could be made to do something different/useful.
      (message "%s: failed, cannot reverse ourselves" prefix)
      nil)

     (t
      (let ((current-prefix-arg
             (cond
              ((and (integerp local-last-prefix-arg) (< local-last-prefix-arg 0))
               1)
              (t
               -1))))
        (save-excursion
          (goto-char local-mrk)
          ;; Not totally elegant, this is needed so the mark is correctly
          ;; placed when on a word boundary.
          ;; Otherwise this would step over the boundary which isn't desirable.
          (let ((current-prefix-arg (- current-prefix-arg)))
            (call-interactively local-last-command))
          ;; Calling twice is intentional as the previous call reverses the motion.
          (call-interactively local-last-command)

          (setq new-mrk (point))))

      ;; Success.
      (cons new-mrk (cons local-last-command local-last-prefix-arg))))))

;;;###autoload
(defun meep-region-activate-and-reverse-motion ()
  "Exchange point and mark, activating the region."
  (interactive)
  (let ((last-motion-info (meep--last-motion-calc-whole-mark-pos nil)))
    (cond
     (last-motion-info
      (setq meep-mark-adjust (point))
      (setq deactivate-mark nil)
      (meep--set-marker (car last-motion-info))
      (exchange-point-and-mark)
      t)
     (t
      nil))))

;; ---------------------------------------------------------------------------
;; Selection/Region: Secondary Selection

(defcustom meep-region-swap-imply-region t
  "Imply the region from the length of the secondary region.

- When the region is on a single line:
  The text after point implies the selection.
- When a line-wise region is used:
  The same number of lines after point is used (ignoring line length).
- When a rectangle-wise region is used:
  The text after and lines below are used to create the implied selection."
  :type 'boolean)

(defun meep--range-list-as-marker-list (ranges)
  "Create a list of markers from RANGES, a list of integer ranges."
  (declare (important-return-value t))
  (mapcar
   (lambda (item)
     (let ((mark-beg (set-marker (make-marker) (car item)))
           (mark-end (set-marker (make-marker) (cdr item))))
       (set-marker-insertion-type mark-beg nil)
       (set-marker-insertion-type mark-end t)
       (cons mark-beg mark-end)))
   ranges))


(defun meep--region-swap-contiguous-impl (is-line-wise)
  "Swap the region with the secondary.

When IS-LINE-WISE is non-nil, the secondary selection represents whole lines.
This impacts `meep-region-swap-imply-region', causing the implied region to extend
to line boundaries."
  (let ((range-a nil)
        (range-b nil)
        (range-region nil)
        (is-forward nil))
    (setq range-a
          (cons (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay)))

    (setq range-b
          (cond
           ((region-active-p)
            (when (eq (point) (region-end))
              (setq is-forward t))
            (cons (region-beginning) (region-end)))
           ;; Allow swapping with the point, making this a way to move text.
           (meep-region-swap-imply-region
            (let* ((beg-a (car range-a))
                   (end-a (cdr range-a))
                   (beg-a-eol
                    (save-excursion
                      (goto-char beg-a)
                      (pos-eol)))
                   (beg-b (point))
                   ;; Either this value must be set or an error raised.
                   (end-b nil))
              (cond
               ;; A single line, simple!
               ((<= end-a beg-a-eol)
                ;; Swapping past the EOL could be considered an error.
                ;; However clamping and swapping could be useful, so do that.
                ;; Besides, it's unlikely the user intended anything else.
                (let ((col-count-a (meep--columns-from-point-range beg-a end-a)))
                  (save-excursion
                    (move-to-column (+ (current-column) col-count-a))
                    (setq end-b (point)))))
               (t
                ;; More complex multi-line swapping.
                (save-excursion
                  (goto-char end-a)
                  (cond
                   ;; Simple case, the region is N *whole* lines.
                   ;; Ending on the beginning of a new line.
                   ;; In this the implied selection is N whole lines
                   ((bolp)

                    ;; Trim the trailing line from `range-a',
                    ;; this means if the last N lines to swap with are at the end of the buffer.
                    ;; We are not relying on those lines to have a blank line afterwards for
                    ;; swapping to work usefully.
                    (setcdr range-a (meep--decf end-a))
                    ;; Should never be reversed as this begins as multi-line.
                    (meep--assert (<= beg-a end-a))

                    (goto-char beg-b)
                    (beginning-of-line)
                    ;; When inferring from whole lines, step back to include the whole line.
                    (when is-line-wise
                      (setq beg-b (point)))

                    (let ((range-a-lines (1- (count-lines beg-a end-a))))
                      (unless (zerop (forward-line range-a-lines))
                        (user-error "Region swap failed, expected %d line(s) after the point"
                                    range-a-lines)))
                    (setq end-b (pos-eol)))

                   ;; The trailing line is *not* on the new-line boundary.
                   ;; In this case apply the final lines column to this location.
                   (t
                    (goto-char end-a)
                    (let ((col-end-a (current-column)))
                      (goto-char beg-b)
                      (beginning-of-line)
                      (let ((range-a-lines (1- (count-lines beg-a end-a))))
                        (unless (zerop (forward-line range-a-lines))
                          (user-error "Region swap failed, expected %d line(s) after the point"
                                      range-a-lines)))
                      ;; Match the column on the ending line of range-a.
                      (move-to-column col-end-a)
                      (setq end-b (point))))))))
              (meep--assert end-b)
              (cons beg-b end-b)))
           (t
            (cons (point) (point)))))

    ;; Restore this range.
    (setq range-region range-b)

    (when (> (car range-a) (car range-b))
      (meep--swap-vars range-a range-b))

    (when (> (cdr range-a) (car range-b))
      (user-error "Region swap unsupported for overlapping regions"))

    (let ((str-a (buffer-substring-no-properties (car range-a) (cdr range-a)))
          (str-b (buffer-substring-no-properties (car range-b) (cdr range-b))))

      (delete-overlay mouse-secondary-overlay)

      (meep--replace-in-region str-a (car range-b) (cdr range-b))
      (let ((offset (- (length str-a) (- (cdr range-b) (car range-b)))))
        (setcdr range-b (+ (cdr range-b) offset)))

      (meep--replace-in-region str-b (car range-a) (cdr range-a))
      (let ((offset (- (length str-b) (- (cdr range-a) (car range-a)))))
        (setcar range-b (+ (car range-b) offset))
        (setcdr range-b (+ (cdr range-b) offset))
        (setcdr range-a (+ (cdr range-a) offset)))

      ;; Restore the region (without activating it).
      (let ((beg (car range-region))
            (end (cdr range-region)))
        (cond
         (is-forward
          (meep--set-marker beg)
          (goto-char end))
         (t
          (meep--set-marker end)
          (goto-char beg)))))))

(defun meep--region-swap-rectangle-impl ()
  "Swap the region with the secondary for rectangular regions."
  (let* ((region-beg nil)
         (region-end nil)
         (region-end-next nil)

         ;; Caller will have checked: `secondary-selection-exist-p'.
         (secondary-beg (overlay-start mouse-secondary-overlay))
         (secondary-end (overlay-end mouse-secondary-overlay))
         (secondary-end-next nil)

         ;; Set later.
         (line-ranges-a nil)
         (len-a nil)
         (line-ranges-b (meep--rectangle-range-list-from-rectangle secondary-beg secondary-end))
         (len-b (length line-ranges-b))

         (is-forward nil)
         (is-swap nil))

    (cond
     ((region-active-p)
      (setq region-beg (region-beginning))
      (setq region-end (region-end))

      (when (eq (point) (region-end))
        (setq is-forward t)))

     (t
      ;; Calculate a region based on the secondary region.
      ;; Implement `meep-region-swap-imply-region'.
      ;; Assume cursor is the top, left of the region.
      (save-excursion
        (let ((col-beg nil)
              (col-end nil)
              (pos-init (point))
              (col-init nil))

          (goto-char secondary-beg)
          (setq col-beg (current-column))
          (goto-char secondary-end)
          (setq col-end (current-column))

          (when (> col-beg col-end)
            (meep--swap-vars col-beg col-end))

          ;; Apply the offsets to calculate a rectangle region at `point'.
          (let ((col-len (- col-end col-beg)))
            (goto-char pos-init)
            (setq col-init (current-column))
            (goto-char (pos-bol))

            (when (> len-b 1)
              (let ((len-a-left (forward-line (1- len-b))))
                (unless (zerop len-a-left)
                  (user-error "Rectangle line count mismatch for implied region (%d and %d)"
                              (- len-b len-a-left)
                              len-b))))

            (move-to-column (+ col-init col-len))
            (when (< (current-column) col-init)
              (user-error
               "Rectangle can't compute implied region (last line doesn't meet current column)"))

            (setq region-beg pos-init)
            (setq region-end (point)))))))

    (setq line-ranges-a (meep--rectangle-range-list-from-rectangle region-beg region-end))
    (setq len-a (length line-ranges-a))

    (unless (eq len-a len-b)
      (user-error "Rectangle line count mismatch (%d and %d)" len-a len-b))

    ;; We _could_ subtract one region from another - to prevent overlap,
    ;; or handle overlap as part of the swapping logic.
    ;; Raise an error as this seems like enough of a corner case.
    ;; It could always be supported if an important use-case is shown.
    (when (meep--ranges-overlap-p line-ranges-a line-ranges-b)
      (user-error "Region swap unsupported for overlapping (rectangle) regions"))

    ;; Order so range-a is first.
    (when (> (car (car line-ranges-a)) (car (car line-ranges-b)))
      (meep--swap-vars line-ranges-a line-ranges-b)
      (setq is-swap t))

    (setq line-ranges-a (meep--range-list-as-marker-list line-ranges-a))
    (setq line-ranges-b (meep--range-list-as-marker-list line-ranges-b))

    ;; Now swap multiple ordered regions.
    (save-excursion
      (while line-ranges-a
        (let* ((range-a (pop line-ranges-a))
               (range-b (pop line-ranges-b))
               (text-a (buffer-substring-no-properties (car range-a) (cdr range-a)))
               (text-b (buffer-substring-no-properties (car range-b) (cdr range-b))))

          ;; Unlikely, but may as well skip redundant swaps.
          (unless (string-equal text-a text-b)
            (meep--replace-in-region text-b (car range-a) (cdr range-a))
            (meep--replace-in-region text-a (car range-b) (cdr range-b)))

          ;; Reached the end, ensure the mark and secondary selection are updated.
          (unless line-ranges-a

            (cond
             (is-swap
              (setq secondary-end-next (marker-position (cdr range-a)))
              (setq region-end-next (marker-position (cdr range-b))))
             (t
              (setq secondary-end-next (marker-position (cdr range-b)))
              (setq region-end-next (marker-position (cdr range-a))))))

          ;; Queue markers to be cleared.
          (set-marker (car range-a) nil)
          (set-marker (cdr range-a) nil)
          (set-marker (car range-b) nil)
          (set-marker (cdr range-b) nil))))

    ;; Only move the end for both region and secondary selection.
    (cond
     (is-forward
      (goto-char region-end-next))
     (t
      (meep--set-marker region-end-next)))

    (move-overlay
     mouse-secondary-overlay (overlay-start mouse-secondary-overlay) secondary-end-next)))

;;;###autoload
(defun meep-region-swap ()
  "Swap the contents of the primary and secondary region.

When `meep-region-swap-imply-region' is non-nil,
only the secondary region needs to be set."
  (interactive "*")
  (unless (secondary-selection-exist-p)
    (user-error "No secondary selection!"))

  (let ((is-rect-wise nil)
        (is-line-wise nil))
    (cond
     ((region-active-p)
      (when (bound-and-true-p rectangle-mark-mode)
        (setq is-rect-wise t)))
     (meep-region-swap-imply-region
      ;; When there is no active region.
      (let* ((plist (overlay-get mouse-secondary-overlay 'meep))
             (type (plist-get plist :region-type)))
        (cond
         ((eq type 'rect-wise)
          ;; This will need to be inferred.
          (setq is-rect-wise t))
         ((eq type 'line-wise)
          (setq is-line-wise t))))))

    (cond
     (is-rect-wise
      (meep--region-swap-rectangle-impl))
     (t
      (meep--region-swap-contiguous-impl is-line-wise)))))

;;;###autoload
(defun meep-region-to-secondary-selection ()
  "Create a secondary selection from the current region."
  (interactive)
  (cond
   ((region-active-p)

    ;; Inline: `secondary-selection-from-region'
    (delete-overlay mouse-secondary-overlay)
    (move-overlay mouse-secondary-overlay (region-beginning) (region-end))

    ;; If other properties are ever used, this would need to append the property.
    (overlay-put
     mouse-secondary-overlay 'meep
     (cond
      ((bound-and-true-p rectangle-mark-mode)
       (list :region-type 'rect-wise))
      ((meep--state-region-line-wise-check)
       (list :region-type 'line-wise))
      (t
       nil)))

    ;; Drop the selection as it's annoying to keep both.
    (deactivate-mark t))
   (t
    ;; Perhaps we could have a handy alternative?
    ;; (user-error "No active region")
    (delete-overlay mouse-secondary-overlay))))


;; ---------------------------------------------------------------------------
;; Selection/Region: Line Selection

(defun meep--state-region-line-wise-check ()
  "Return non-nil if the line-based selection is in use.

If the region no longer meets line bounds, return nil."
  (cond
   ((eq meep-state-region-elem 'line-wise)
    ;; Ensure the region is line-based (even if not active).
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (and (progn
               (goto-char beg)
               (bolp))
             (progn
               (goto-char end)
               (bolp))))))
   (t
    nil)))

(defun meep--state-region-type ()
  "Return the region-type or nil."
  (let ((region-type nil))
    (when (region-active-p)
      (cond
       ((bound-and-true-p rectangle-mark-mode)
        (setq region-type 'rect-wise))
       ((meep--state-region-line-wise-check)
        (setq region-type 'line-wise))))
    region-type))

;;;###autoload
(defun meep-region-expand-to-line-bounds ()
  "Expand the region to the line bounds.

`meep-state-region-elem' is set to \\='line-wise which commands may
use to maintain line-based selection."
  (interactive)
  (cond
   ((region-active-p)

    (when (eq (car-safe transient-mark-mode) 'only)
      ;; Move from shift-select to "permanent" select.
      ;; Needed when expanding to line bounds after a "search" for example,
      ;; so the next motion doesn't drop the selection.
      (setq transient-mark-mode t)
      ;; Behave as if setting for the first time.
      ;; Since transient selection may be set
      ;; when the user isn't explicitly requesting selection.
      (setq meep-state-region-elem 'line-wise))

    (let* ((is-forward (eq (point) (region-end)))
           (beg (region-beginning))
           (end (region-end))

           ;; Local functions.
           (is-line-empty-fn
            (lambda ()
              (save-excursion
                (beginning-of-line)
                (looking-at-p "[[:blank:]]*$"))))

           ;; Scan by STEP lines (+1/-1) over the run of non-empty lines from
           ;; point, returning the start of the last non-empty line reached.
           ;; `forward-line' yields a non-zero remainder at the buffer bound
           ;; without signaling, so the scan always terminates: an unterminated
           ;; final line (forward) or `point-min' (backward) cannot spin it.
           (scan-non-empty-fn
            (lambda (step)
              (let ((x (point))
                    (cont t))
                (while (and cont (null (funcall is-line-empty-fn)))
                  (setq x (point))
                  (setq cont (zerop (forward-line step))))
                x))))

      (let ((beg-next nil)
            (end-next nil))
        (save-excursion
          (goto-char beg)
          (setq beg-next (pos-bol))

          ;; +1 to copy the whole line.
          (goto-char (max beg (1- end)))
          (setq end-next (1+ (pos-eol))))

        ;; Extend if the selection already meets line bounds.
        (cond
         ((and (eq beg beg-next) (eq end end-next))
          (cond
           (is-forward
            ;; No need for an initial next-line as this is already on the next line.

            ;; Step past a leading blank line so the scan below extends the
            ;; following run of non-empty lines.
            (when (funcall is-line-empty-fn)
              (forward-line 1))

            (goto-char (funcall scan-non-empty-fn 1))
            (goto-char (pos-eol))

            ;; Ensure we always step over the last newline.
            ;; This is (among other reasons)
            ;; so it's possible to select a line and cut it.
            ;; It also has some minor added benefits.
            ;; - The cursor doesn't scroll off the RHS of the screen
            ;;   for long lines.
            ;; - Moving the cursor up-down can stick to column zero.
            ;;
            ;; Except at end-of-buffer, where there is no newline to step over.
            (unless (or (eobp) (funcall is-line-empty-fn))
              (forward-char 1)))

           (t
            ;; Step to the line above the selection.
            (forward-line -1)

            ;; Step past a leading blank line so the scan below extends the
            ;; preceding run of non-empty lines.
            (when (funcall is-line-empty-fn)
              (forward-line -1))

            (goto-char (funcall scan-non-empty-fn -1))
            (goto-char (pos-bol))

            ;; Don't start on the empty line.
            (when (funcall is-line-empty-fn)
              (forward-char 1)))))
         (t
          ;; Extend the selection.
          (cond
           (is-forward
            (meep--set-marker beg-next)
            (goto-char end-next))
           (t
            (goto-char beg-next)
            (meep--set-marker end-next))))))))
   (t
    ;; Select line, this setting "requests" future operations use line selection.
    (setq meep-state-region-elem 'line-wise)

    (meep--set-marker (pos-bol))
    (goto-char (min (1+ (pos-eol)) (point-max)))
    (activate-mark t))))


;; ---------------------------------------------------------------------------
;; Selection/Region: Expand/Contract
;;
;; Expand/contract the regions.
;; Initially expanding is performed in both directions until
;; a syntax mismatch is encountered, then expanding is only performed at the point.
;;
;; This allows for expanding across surrounding symmetrical characters which can be useful.

;; When non-nil, don't attempt to extend in both directions.
;; This is useful so it's possible to expand into brackets or quotes
;; but stop symmetrical extending once the first syntax mismatch is found.
(defvar-local meep--region-syntax-asym nil)

;; Special case, if we *only* traversed blank space,
;; traverse all remaining blank space, needed
;; since there are sometimes syntax changes at line bounds
;; which are (from a user perspective) *just* blank-space.

(defun meep--skip-syntax-backward-or-blank (syntax &optional lim)
  "Skip SYNTAX backward limited by LIM.
When only blank space was skipped, skip all blank space."
  (let ((pos-orig (point))
        (is-blank nil)
        (result (skip-syntax-backward syntax lim)))
    (unless (zerop result)
      (setq is-blank (looking-at-p "[[:blank:]\n]"))
      (when is-blank
        (setq is-blank nil)
        (let ((pos-new (point)))
          (save-excursion
            (goto-char pos-orig)
            (skip-chars-backward "[:blank:]\n" pos-new)
            (when (eq (point) pos-new)
              ;; The entire range was blank.
              (setq is-blank t))))))

    ;; If all chars were blank, skip any other blanks
    ;; (even if this crosses other kinds of syntax).
    (when is-blank
      (skip-chars-backward "[:blank:]\n" lim)
      (setq result (- pos-orig (point))))

    result))

(defun meep--skip-syntax-forward-or-blank (syntax &optional lim)
  "Skip SYNTAX forward limited by LIM.
When only blank space was skipped, skip all blank space."
  (let ((pos-orig (point))
        (is-blank (looking-at-p "[[:blank:]\n]"))
        (result (skip-syntax-forward syntax lim)))
    (unless (zerop result)
      (when is-blank
        (setq is-blank nil)
        (let ((pos-new (point)))
          (save-excursion
            (goto-char pos-orig)
            (skip-chars-forward "[:blank:]\n" pos-new)
            (when (eq (point) pos-new)
              ;; The entire range was blank.
              (setq is-blank t))))))

    ;; If all chars were blank, skip any other blanks
    ;; (even if this crosses other kinds of syntax).
    (when is-blank
      (skip-chars-forward "[:blank:]\n" lim)
      (setq result (- (point) pos-orig)))

    result))

(defun meep--region-syntax-or-symbol-forward (syntax &optional lim)
  "Like `skip-syntax-forward' for SYNTAX with LIM, but also skip symbol bounds."
  (let ((bounds-end (cdr (bounds-of-thing-at-point 'symbol))))
    (cond
     ((and bounds-end (< (point) bounds-end))
      (prog1 (- bounds-end (point))
        (goto-char bounds-end)))
     (t
      (meep--skip-syntax-forward-or-blank syntax lim)))))

(defun meep--region-syntax-or-symbol-backward (syntax &optional lim)
  "Like `skip-syntax-backward' for SYNTAX with LIM, but also skip symbol bounds."
  (let ((bounds-beg (car (bounds-of-thing-at-point 'symbol))))
    (cond
     ((and bounds-beg (> (point) bounds-beg))
      (prog1 (- (point) bounds-beg)
        (goto-char bounds-beg)))
     (t
      (meep--skip-syntax-backward-or-blank syntax lim)))))

(defun meep--region-syntax-priority (ch)
  "Return a priority from CH (the result of `syntax-class-to-char')."
  ;; NOTE: while listing all syntax table chars isn't ideal, these don't change often.
  ;;
  ;; NOTE: symmetrical syntax such as open/close begin/end must have the same priority.
  ;; Otherwise expanding matching syntax won't step over both.

  ;; Priority, first to last.
  (cond
   ;; Text-objects.
   ((eq ch ?_) ; Symbol constituents.
    13)
   ((eq ch ?w) ; Word constituents.
    12)

   ;; Brackets and quotes.
   ((memq ch '(?\( ?\))) ; Open/close parenthesis characters.
    11)
   ((memq ch '(?< ?>)) ; Comment start/end.
    10)
   ((eq ?$ ch) ; Paired delimiters.
    9)
   ((eq ?\" ch) ; String quotes.
    8)
   ((eq ?' ch) ; Expression prefixes.
    7)
   ((eq ?/ ch) ; Character quotes.
    6)

   ;; Other punctuation.
   ((eq ?! ch) ; Generic comment delimiters.
    5)
   ((eq ?| ch) ; Generic string delimiters.
    4)
   ((eq ?\\ ch) ; Escape-syntax characters.
    3)
   ((eq ?@ ch) ; Inherit standard syntax (needs investigation, may not happen in practice).
    2)
   ((eq ?. ch) ; Punctuation characters.
    1)
   ((eq ?\s ch) ; Blank space (always last).
    -1)
   (t
    ;; Should never reach this (all chars must be accounted for).
    (meep--assert-unreachable))))

(defun meep--region-syntax-expand-impl (n)
  "Expand matching syntax table N times."
  (when meep--region-syntax-asym
    (let ((local-last-command (meep--last-command)))
      (unless (memq local-last-command '(meep-region-syntax-expand meep-region-syntax-contract))
        (setq meep--region-syntax-asym nil))))
  (let ((syn-as-str-fn (lambda (syn) (char-to-string (syntax-class-to-char (syntax-class syn)))))
        (found nil))
    (while (prog1 (< 0 n)
             (meep--decf n))
      (cond
       ((region-active-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (is-forward (eq end (point)))
               (beg-ok
                (and (or (null meep--region-syntax-asym) (null is-forward)) (/= beg (point-min))))
               (end-ok (and (or (null meep--region-syntax-asym) is-forward) (/= end (point-max))))
               (beg-syn (and beg-ok (syntax-after (1- beg))))
               (end-syn (and end-ok (syntax-after end)))

               (beg-syn-str (and beg-ok (funcall syn-as-str-fn beg-syn)))
               (end-syn-str (and end-ok (funcall syn-as-str-fn end-syn)))

               (do-beg nil)
               (do-end nil)
               (do-symmetry nil))

          (cond
           ((and (null beg-ok) (null end-ok))) ; Do nothing.
           ((null beg-ok)
            (setq do-end t))
           ((null end-ok)
            (setq do-beg t))

           ((string-equal beg-syn-str end-syn-str)
            (setq do-beg t)
            (setq do-end t))
           ;; It's handy for including/excluding surrounding brackets.
           ((and (member beg-syn-str '("(" ")")) (member end-syn-str '("(" ")")))
            (setq do-symmetry t)
            (setq do-beg t)
            (setq do-end t))
           ((eq beg (point))
            (setq do-beg t))
           ((eq end (point))
            (setq do-end t))
           (t
            (user-error "Invalid state")))

          (let ((beg-next beg)
                (end-next end))

            (when do-beg
              (save-excursion
                (goto-char beg)
                (meep--region-syntax-or-symbol-backward beg-syn-str)
                (setq beg-next (point))))
            (when do-end
              (save-excursion
                (goto-char end)
                (meep--region-syntax-or-symbol-forward end-syn-str)
                (setq end-next (point))))

            ;; Avoid expanding an unbalanced number of brackets as it makes it
            ;; difficult to copy blocks of code.
            ;; NOTE: instead of limiting to 1, we could make them even,
            ;; in practice it might be more useful to only expand one bracket at a time though.
            (when do-symmetry
              (setq beg-next (max beg-next (1- beg)))
              (setq end-next (min end-next (1+ end))))

            (cond
             ((eq beg (point))
              (goto-char beg-next)
              (meep--set-marker-and-activate end-next))
             (t
              (goto-char end-next)
              (meep--set-marker-and-activate beg-next))))

          ;; Don't attempt symmetry in future.
          (unless meep--region-syntax-asym
            (unless (and do-beg do-end)
              (setq meep--region-syntax-asym t)))))
       (t
        (let ((syn nil)
              (do-beg nil)
              (do-end nil)
              (beg-next-override nil)
              (end-next-override nil))

          ;; Perform a symmetrical detection for how expansion should be done:
          ;; - Check symbol bounds at point.
          ;;   This takes priority over all else.
          ;; - Otherwise check the priorities on surrounding syntax and scan over the
          ;;   highest priority or scan both directions when they match.
          (let ((bounds (bounds-of-thing-at-point 'symbol)))
            (cond
             (bounds
              (setq beg-next-override (car bounds))
              (setq end-next-override (cdr bounds))
              (setq do-beg t)
              (setq do-end t))
             (t
              ;; Detect the symbol.
              (let ((beg-syn (and (null (bobp)) (syntax-after (1- (point)))))
                    (end-syn (and (null (eobp)) (syntax-after (point)))))
                (cond
                 ((and beg-syn end-syn)
                  (let ((beg-syn-cls (syntax-class beg-syn))
                        (end-syn-cls (syntax-class end-syn)))
                    (cond
                     ((eq beg-syn-cls end-syn-cls)
                      ;; Simple case, expand over matching syntax in both directions.
                      (setq syn beg-syn)
                      (setq do-beg t)
                      (setq do-end t))
                     (t
                      (let* ((beg-syn-ch (syntax-class-to-char beg-syn-cls))
                             (end-syn-ch (syntax-class-to-char end-syn-cls))
                             (beg-priority (meep--region-syntax-priority beg-syn-ch))
                             (end-priority (meep--region-syntax-priority end-syn-ch)))

                        ;; Enforce space syntax for newlines.
                        ;; For some reason comment start/end is sometimes used here.
                        (when (eq ?\n (char-before (point)))
                          (setq beg-priority (meep--region-syntax-priority ?\s)))
                        (when (eq ?\n (char-after (point)))
                          (setq end-priority (meep--region-syntax-priority ?\s)))

                        (cond
                         ((eq beg-priority end-priority)
                          (setq syn beg-syn)
                          (setq do-beg t)
                          (setq do-end t))
                         ((> beg-priority end-priority)
                          (setq syn beg-syn)
                          (setq do-beg t))
                         (t
                          (setq syn end-syn)
                          (setq do-end t)))))))))))))
          (cond
           ((and (null syn) (null do-beg) (null do-end))
            ;; Only on the first step, otherwise silently skip.
            (unless found
              (message "No syntax around the point (empty buffer?)"))
            ;; Break.
            (setq n 0)
            nil)
           (t
            (let ((beg-next
                   (or beg-next-override
                       (cond
                        (do-beg
                         (save-excursion
                           (meep--region-syntax-or-symbol-backward (funcall syn-as-str-fn syn))
                           (point)))
                        (t
                         (point)))))
                  (end-next
                   (or end-next-override
                       (cond
                        (do-end
                         (save-excursion
                           (meep--region-syntax-or-symbol-forward (funcall syn-as-str-fn syn))
                           (point)))
                        (t
                         (point))))))
              ;; Place the point at the beginning and the mark at the end.
              ;; This is somewhat arbitrary but matches:
              ;; - `meep-region-mark-bounds-of-char-inner' and related functions.
              ;; - `meep-move-matching-bracket-inner' and related functions that
              ;;   first jump to the beginning.
              (goto-char beg-next)
              (meep--set-marker-and-activate end-next)
              (setq found t))))))))
    found))

(defun meep--region-syntax-contract-impl (n)
  "Contract matching syntax table N times."
  (when meep--region-syntax-asym
    (let ((local-last-command (meep--last-command)))
      (unless (memq local-last-command '(meep-region-syntax-expand meep-region-syntax-contract))
        (setq meep--region-syntax-asym nil))))
  (let ((syn-as-str-fn (lambda (syn) (char-to-string (syntax-class-to-char (syntax-class syn)))))
        (found nil))
    (while (prog1 (< 0 n)
             (meep--decf n))
      (let* ((beg (region-beginning))
             (end (region-end))
             (is-forward (eq end (point)))
             (beg-ok
              (and (or (null meep--region-syntax-asym) (null is-forward)) (/= beg (point-min))))
             (end-ok (and (or (null meep--region-syntax-asym) is-forward) (/= end (point-max))))

             (beg-syn (syntax-after beg))
             (end-syn (syntax-after (1- end)))

             (beg-syn-str (funcall syn-as-str-fn beg-syn))
             (end-syn-str (funcall syn-as-str-fn end-syn))

             (do-beg nil)
             (do-end nil)

             (do-symmetry nil))

        (cond
         ((and (null beg-ok) (null end-ok))) ; Do nothing.
         ((null beg-ok)
          (setq do-end t))
         ((null end-ok)
          (setq do-beg t))

         ((string-equal beg-syn-str end-syn-str)
          (setq do-beg t)
          (setq do-end t))
         ;; It's handy for including/excluding surrounding brackets.
         ((and (member beg-syn-str '("(" ")")) (member end-syn-str '("(" ")")))
          (setq do-symmetry t)
          (setq do-beg t)
          (setq do-end t))
         ((eq beg (point))
          (setq do-beg t))
         ((eq end (point))
          (setq do-end t))
         (t
          (user-error "Invalid state")))

        (let ((beg-next beg)
              (end-next end))

          (when do-beg
            (save-excursion
              (goto-char beg)
              (meep--region-syntax-or-symbol-forward beg-syn-str)
              (setq beg-next (point))))
          (when do-end
            (save-excursion
              (goto-char end)
              (meep--region-syntax-or-symbol-backward end-syn-str)
              (setq end-next (point))))

          ;; Avoid expanding an unbalanced number of brackets as it makes it
          ;; difficult to copy blocks of code.
          ;; NOTE: instead of limiting to 1, we could make them even,
          ;; in practice it might be more useful to only expand one bracket at a time though.
          (when do-symmetry
            (setq beg-next (min beg-next (1+ beg)))
            (setq end-next (max end-next (1- end))))

          (cond
           ((<= end-next beg-next)
            ;; Contract to nothing.
            (deactivate-mark t)
            (setq found t)
            ;; Break.
            (setq n 0))

           ((eq beg (point))
            (goto-char beg-next)
            (meep--set-marker-and-activate end-next)
            (setq found t))

           (t
            (goto-char end-next)
            (meep--set-marker-and-activate beg-next)
            (setq found t))))

        ;; Don't attempt symmetry in future.
        (unless meep--region-syntax-asym
          (unless (and do-beg do-end)
            (setq meep--region-syntax-asym t)))))
    found))

;;;###autoload
(defun meep-region-syntax-expand (arg)
  "Expand on matching syntax table elements ARG times.

When there is no active region, activate and expand the region.
This can be used to quickly mark symbols or blocks of contiguous syntax,
including blank-space."
  (interactive "p")
  (cond
   ((< arg 0)
    (meep--region-syntax-contract-impl (- arg)))
   (t
    (meep--region-syntax-expand-impl arg))))

;;;###autoload
(defun meep-region-syntax-contract (arg)
  "Contract matching syntax table ARG times."
  (interactive "p")
  (cond
   ((< arg 0)
    (meep--region-syntax-expand-impl (- arg)))
   (t
    (meep--region-syntax-contract-impl arg))))

;; ---------------------------------------------------------------------------
;; Command: Repeat N

;; Only ever use this when the last command was numeric.
(defvar meep--numeric-last-command nil)
(defvar meep--numeric-last-prefix-arg nil)

(defun meep--last-command ()
  "Like `last-command' but not masked by numeric arguments."
  (declare (important-return-value t))
  (cond
   ;; The symbol check is needed as keys may be bound to a lambda,
   ;; in that case checking for the symbol raises an error.
   ((and (symbolp last-command) (meep-command-is-digit-argument last-command))
    meep--numeric-last-command)
   (t
    last-command)))

(defun meep--last-prefix-arg ()
  "Like `last-prefix-arg' but not masked by numeric arguments."
  (declare (important-return-value t))
  (cond
   ;; The symbol check is needed as keys may be bound to a lambda,
   ;; in that case checking for the symbol raises an error.
   ((and (symbolp last-command) (meep-command-is-digit-argument last-command))
    meep--numeric-last-prefix-arg)
   (t
    last-prefix-arg)))

;;;###autoload
(defun meep-digit-argument-repeat ()
  "Repeat the last command multiple times.

This must be bound to keys 0..9 or the minus key."
  (interactive)
  ;; Copied from `digit-command'.
  (let* ((char
          (cond
           ((integerp last-command-event)
            last-command-event)
           (t
            (get last-command-event 'ascii-character))))
         (digit
          (cond
           ((eq char ?-)
            '-)
           ((and (<= char ?9) (<= ?0 char))
            (- (logand char ?\177) ?0))
           (t
            (user-error "Not a digit char %S" char))))
         (was-digit-command
          (and (symbolp last-command) (meep-command-is-digit-argument last-command))))

    (let ((local-last-command
           (cond
            (was-digit-command
             meep--numeric-last-command)
            (t
             (setq meep--numeric-last-command last-command))))
          (local-last-prefix-arg
           (cond
            (was-digit-command
             meep--numeric-last-prefix-arg)
            (t
             (setq meep--numeric-last-prefix-arg last-prefix-arg)))))
      (ignore local-last-prefix-arg)
      ;; TODO: allow typing in larger numbers. 2,3 -> 23 not (2 + 3 = 5).
      ;; TODO: explore how negative should be applied to the existing "digit".
      ;; TODO: check fn using the previous prefix arg.
      (let ((current-prefix-arg digit)
            ;; Ensure an action can be adjusted without setting the mark.
            (meep-mark-set-on-motion-override t))
        (call-interactively local-last-command)))))


;; ---------------------------------------------------------------------------
;; Keyboard Macro Access
;;
;; This provides access to macros where keys can be
;; conveniently assigned to macros (VIM style).

;; Temporary, use to track which register is being recorded to.
(defvar-local meep--kmacro-current-register nil)

;;;###autoload
(defun meep-register-kmacro-start-or-end ()
  "Start or stop recording a keyboard macro to a register."
  (interactive)
  (cond
   ;; End recording.
   (defining-kbd-macro
    (kmacro-end-macro nil)
    (when meep--kmacro-current-register
      (set-register meep--kmacro-current-register (kmacro-ring-head))
      (setq meep--kmacro-current-register nil)))
   (t ;; Start recording.
    (setq meep--kmacro-current-register (register-read-with-preview "(Over)write to register: "))
    (set-register meep--kmacro-current-register nil)
    (kmacro-start-macro nil))))

;;;###autoload
(defun meep-register-jump-to (arg)
  "Jump to a register or execute a macro stored in a register, ARG times."
  (interactive "p")
  (let* ((reg (register-read-with-preview "Use register: "))
         (val (get-register reg)))
    (cond
     ;; Keyboard macro.
     ((or (vectorp val) (functionp val))
      (with-undo-amalgamate
        (dotimes (_ (abs arg))
          (register-val-jump-to val nil))))
     ;; Anything else.
     (t
      (register-val-jump-to val nil)))))


;; ---------------------------------------------------------------------------
;; ISEARCH Wrapper
;;
;; Support searching in both directions as well as
;; searching based on the active region.

(defcustom meep-isearch-activate-mark t
  "ISEARCH activates the mark (transient).
So motion drops the selection.

Useful for pasting while stepping over search results."
  :type 'boolean)


(defun meep--isearch-done-hook ()
  "Temporary local hook, when starting ISEARCH, remove itself afterwards."
  ;; Assume the `has-region' argument is false,
  ;; this is OK as starting a new search typically doesn't continue
  ;; to use the prior selection.
  (remove-hook 'isearch-mode-end-hook #'meep--isearch-done-hook t)
  (meep--isearch-handle-done nil))


(defun meep--isearch-handle-done (had-region)
  "Handle the result of ISEARCH being done.

This is only to be called from within ISEARCH functions.

When HAD-REGION is non-nil, mark the region."
  ;; When repeating a search with no previous search data,
  ;; it's possible for `isearch-success' to be set without the other end.
  ;; Use the other-end to detect success as well.
  (when (and isearch-success isearch-other-end)
    ;; Select.
    (meep--set-marker isearch-other-end)
    (cond
     ((or had-region meep-isearch-activate-mark)
      (activate-mark)
      (setq transient-mark-mode (cons 'only t)))
     (t
      (setq deactivate-mark t)))))

;;;###autoload
(defun meep-isearch-regexp-next ()
  "Search forward for a regexp."
  (interactive)
  (add-hook 'isearch-mode-end-hook #'meep--isearch-done-hook 0 t)
  (call-interactively #'isearch-forward-regexp))

;;;###autoload
(defun meep-isearch-regexp-prev ()
  "Search backward for a regexp."
  (interactive)
  (add-hook 'isearch-mode-end-hook #'meep--isearch-done-hook 0 t)
  (call-interactively #'isearch-backward-regexp))

(defun meep--isearch-repeat-impl (dir)
  "Repeat search in direction DIR.
Return non-nil on success."
  ;; Re-display can flicker.
  (let ((inhibit-redisplay t)
        ;; Opinionated, but ISEARCH is not that usable without these.
        (isearch-wrap-pause 'no-ding)
        (isearch-repeat-on-direction-change t)
        (had-region (region-active-p))
        (pos-init (point)))

    (prog1 (cond
            ((< dir 0)
             (isearch-repeat-backward (- dir)))
            (t
             (isearch-repeat-forward dir)))
      (meep--isearch-handle-done had-region)

      ;; NOTE(@ideasman42): Without an explicit exit,
      ;; this leaves a stale/ugly overlay that may wrap lines
      ;; (based on a previous search that may have been edited out).
      ;; This looks to be default behavior as of Emacs 30.1,
      ;; although it seems like it could be arguably considered a bug.
      (unless isearch-success
        (let ((isearch-opoint (point)))
          (isearch-exit))
        ;; Moving the cursor on a failed search also seems strange, restore it.
        (goto-char pos-init)))))

;;;###autoload
(defun meep-isearch-repeat-next (arg)
  "Repeat ISEARCH forwards ARG times.
Return non-nil on success."
  (interactive "p")
  (meep--isearch-repeat-impl arg))

;;;###autoload
(defun meep-isearch-repeat-prev (arg)
  "Repeat ISEARCH backwards ARG times.
Return non-nil on success."
  (interactive "p")
  (meep--isearch-repeat-impl (- arg)))

(defun meep--isearch-bounds-at-point-impl ()
  "Return the region for `meep-isearch-next-at-point' to use."
  (cond
   ((region-active-p)
    ;; TODO: don't attempt multi-line.
    (cons (region-beginning) (region-end)))
   (t
    (bounds-of-thing-at-point 'symbol))))


(defun meep--isearch-extract-regex-from-bounds (text-bounds)
  "Extract a regexp from TEXT-BOUNDS for the purpose of searching."

  (let ((text (buffer-substring-no-properties (car text-bounds) (cdr text-bounds)))
        (beg nil)
        (end nil)
        (beg-test-list (list "\\_<" "\\<" "\\b"))
        (end-test-list (list "\\_>" "\\>" "\\b")))

    ;; NOTE: exactly how to do this isn't clear, looking-at commands work well enough.
    (save-excursion
      (goto-char (car text-bounds))
      (while beg-test-list
        (let ((beg-test (pop beg-test-list)))
          (when (looking-at-p beg-test)
            (setq beg-test-list nil) ; Break.
            (setq beg beg-test))))

      (goto-char (cdr text-bounds))
      (while end-test-list
        (let ((end-test (pop end-test-list)))
          (when (looking-at-p end-test)
            (setq end-test-list nil) ; Break.
            (setq end end-test)))))

    (concat (or beg "") (regexp-quote text) (or end ""))))

(defun meep--isearch-at-point-impl (dir)
  "Perform ISEARCH at point along DIR."
  (let ((had-region (region-active-p))
        (text-bounds (meep--isearch-bounds-at-point-impl))
        ;; Re-display can flicker.
        (inhibit-redisplay t)
        ;; Always wrap.
        (isearch-wrap-pause 'no-ding))
    (unless text-bounds
      (user-error "No symbol at cursor"))
    (cond
     ((< dir 0)
      ;; Unlike searching forward the point needs to be moved *before* the symbol.
      ;; Skip before this instance, -1 or wrap.
      (goto-char
       (cond
        ((eq (point-min) (car text-bounds))
         (point-max))
        (t
         (1- (car text-bounds)))))
      (call-interactively #'isearch-backward-regexp))
     (t
      ;; Skip past this instance.
      (goto-char (cdr text-bounds))
      (call-interactively #'isearch-forward-regexp)))

    (let ((text (meep--isearch-extract-regex-from-bounds text-bounds)))
      ;; Push onto `regexp-search-ring', applying de-duplicating & limiting rules.
      (isearch-update-ring text t)
      ;; This function defines the search as being "regex",
      ;; so it's important ISEARCH's variable is set accordingly.
      (setq isearch-regexp t)

      ;; Inline `isearch-yank-string' because it expects non regex text,
      ;; however this text is already quoted.
      (progn
        (setq isearch-yank-flag t)
        (isearch-process-search-string text (mapconcat #'isearch-text-char-description text ""))))

    ;; Repeat for prefix arguments greater than 1.
    (let ((n (1- (abs dir))))
      (unless (zerop n)
        (cond
         ((< dir 0)
          (isearch-repeat-backward n))
         (t
          (isearch-repeat-forward n)))))

    (isearch-exit)

    (meep--isearch-handle-done had-region)))

;;;###autoload
(defun meep-isearch-at-point-next (arg)
  "Search forwards for the symbol or region at point.
Repeat the search ARG times."
  (interactive "p")
  (meep--isearch-at-point-impl arg))

;;;###autoload
(defun meep-isearch-at-point-prev (arg)
  "Search backwards for the symbol or region at point.
Repeat the search ARG times."
  (interactive "p")
  (meep--isearch-at-point-impl (- arg)))


;; ---------------------------------------------------------------------------
;; Text Editing: Delete

(defun meep--respect-goal-column-impl (fn)
  "Run FN, maintaining the goal column."
  (cond
   (goal-column
    (prog1 (funcall fn)
      (line-move-to-column goal-column)))
   ((and temporary-goal-column
         (and (symbolp last-command)
              (meep-command-is-mark-respect-temporary-goal-column last-command)))
    (let ((column
           (cond
            ((consp temporary-goal-column)
             (truncate (car temporary-goal-column)))
            ((numberp temporary-goal-column)
             temporary-goal-column)
            (t ;; Unlikely.
             0))))

      ;; Use max since it's possible the user moved to the right since the last motion.
      ;; (setq temporary-goal-column (max (or temporary-goal-column 0) (current-column)))

      (prog1 (funcall fn)

        (line-move-to-column column)
        (cond
         ((consp temporary-goal-column)
          (setcar temporary-goal-column column))
         (t
          (setq temporary-goal-column column))))))
   (t ;; Set a new temporary column.
    (let ((column (current-column)))
      (prog1 (funcall fn)
        (line-move-to-column column)
        (setq temporary-goal-column column))))))

(defmacro meep--with-respect-goal-column (&rest body)
  "Execute BODY, maintaining the goal column."
  `(meep--respect-goal-column-impl (lambda () ,@body)))

(defun meep--delete-from-motion-fn (fn)
  "Delete text to point after calling FN."
  (let ((pos-orig (point))
        (pos-next
         (save-excursion
           (funcall fn)
           (point))))
    (delete-region pos-orig pos-next)))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-symbol-next (arg)
  "Delete the symbol forwards ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn (lambda () (forward-thing 'symbol arg))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-symbol-prev (arg)
  "Delete the symbol backwards ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn (lambda () (forward-thing 'symbol (- arg)))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-next (arg)
  "Delete characters with the same syntax class forwards, ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn (lambda () (meep--move-same-syntax-impl arg t (cons nil nil) nil))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-prev (arg)
  "Delete characters with the same syntax class backwards, ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn
   (lambda () (meep--move-same-syntax-impl (- arg) t (cons nil nil) nil))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-or-symbol-next (arg)
  "Delete characters with the same syntax class or symbols forwards, ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn
   (lambda () (meep--move-same-syntax-impl arg t (cons nil nil) 'symbol))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-or-symbol-prev (arg)
  "Delete characters with the same syntax class or symbols backwards, ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn
   (lambda () (meep--move-same-syntax-impl (- arg) t (cons nil nil) 'symbol))))


;; ---------------------------------------------------------------------------
;; Text Editing: Character Delete/Backspace

;;;###autoload
(defun meep-delete-char-next (arg)
  "Delete the next character ARG times.
This deletion is not sent to the `kill-ring'."
  (interactive "*p")
  (delete-char arg nil))

;;;###autoload
(defun meep-delete-char-prev (arg)
  "Delete the previous character ARG times.
This deletion is not sent to the `kill-ring'."
  (interactive "*p")
  (delete-char (- arg) nil))


;; ---------------------------------------------------------------------------
;; Text Editing: Character Delete/Backspace (Ring)
;;
;; Character level delete which has its own kill-ring.
;; This can be useful for quickly relocating characters.
;;
;; Note that this is only accumulated on successive calls.

(defvar meep-delete-char-ring nil
  "Ring of deleted characters.
Used by `meep-delete-char-ring-next', `meep-delete-char-ring-prev',
and `meep-delete-char-ring-yank'.")

(defun meep--delete-char-ring-maybe-clear ()
  "Clear the delete character ring as needed."
  ;; Only accumulate successive calls,
  ;; otherwise this would either grow indefinitely or need to be "managed".
  (unless (memq last-command '(meep-delete-char-ring-next meep-delete-char-ring-prev))
    (setq meep-delete-char-ring nil)))

;;;###autoload
(defun meep-delete-char-ring-next (arg)
  "Delete the next character ARG times.
This deletion is sent to the `meep-delete-char-ring'."
  (interactive "*p")
  (when meep-delete-char-ring
    (meep--delete-char-ring-maybe-clear))
  (let ((pos-init (point)))
    (forward-char arg)
    (unless (eq pos-init (point))
      (let ((text (buffer-substring-no-properties pos-init (point))))
        (push (cons text t) meep-delete-char-ring)
        (delete-region pos-init (point))))))

;;;###autoload
(defun meep-delete-char-ring-prev (arg)
  "Delete the previous character ARG times.
This deletion is sent to the `meep-delete-char-ring'."
  (interactive "*p")
  (when meep-delete-char-ring
    (meep--delete-char-ring-maybe-clear))
  (let ((pos-init (point)))
    (forward-char (- arg))
    (unless (eq pos-init (point))
      (let ((text (buffer-substring-no-properties (point) pos-init)))
        (push (cons text nil) meep-delete-char-ring)
        (delete-region pos-init (point))))))

(defun meep--delete-char-ring-yank-impl (n keep)
  "Yank from the delete character ring N times.
When KEEP is non-nil, don't modify the char-ring."
  (cond
   (meep-delete-char-ring
    (let ((char-ring meep-delete-char-ring))
      (while-let ((item
                   (and (prog1 (< 0 n)
                          (meep--decf n))
                        (pop char-ring))))
        (let ((pos-init (point)))
          (insert (car item))
          (when (cdr item)
            (goto-char pos-init))))
      (unless keep
        (setq meep-delete-char-ring char-ring))))
   (t
    (message "Delete char-ring empty"))))

;;;###autoload
(defun meep-delete-char-ring-yank (arg)
  "Yank from the delete character ring ARG times."
  (interactive "*p")
  (meep--delete-char-ring-yank-impl arg nil))

;;;###autoload
(defun meep-delete-char-ring-yank-no-pop (arg)
  "Yank from the delete character ring ARG times.

Leave the char-ring unmodified afterwards."
  (interactive "*p")
  (meep--delete-char-ring-yank-impl arg t))


;; ---------------------------------------------------------------------------
;; Text Editing: Character Operations

(defun meep--char-is-ok-or-error (action ch)
  "Check that CH is printable and can be used for ACTION."
  ;; Escape.
  (when (eq ch 27)
    (user-error "Char %s canceled" action))

  ;; Backspace, delete, other control characters.
  (unless (aref printable-chars ch)
    (user-error "Char from key not printable: %s" (format-kbd-macro `[,ch]))))

;;;###autoload
(defun meep-char-replace (ch)
  "Read a character CH and replace the selection with it."
  (interactive "*cReplace Char:")

  (meep--char-is-ok-or-error "Replace" ch)

  ;; Ideally this would run before accepting a character.
  (when (eobp)
    (user-error "Cannot replace at the end of the buffer"))

  (let ((replace-in-range-from-columns-fn
         `(lambda (col-beg col-end)
            (let ((beg nil)
                  (end nil)
                  (col-beg-found nil)
                  (col-end-found nil)
                  (changed nil))
              (save-excursion
                (setq col-beg-found (move-to-column col-beg))
                (setq beg (point))
                (setq col-end-found (move-to-column col-end))
                (setq end (point))
                ;; Ensure the line contains text at this column.
                (unless (eq col-beg-found col-end-found)
                  (meep--replace-in-region
                   (make-string (- col-end-found col-beg-found) ,ch) beg end)
                  (setq changed t)))
              (when changed
                (let ((line-offset (- (- col-end col-beg) (- end beg))))
                  (goto-char (+ line-offset end)))))))
        (replace-in-range-fn
         `(lambda (beg end)
            (let ((col-beg nil)
                  (col-end nil))
              (save-excursion
                (goto-char beg)
                (setq col-beg (current-column))
                (goto-char end)
                (setq col-end (current-column))
                (meep--replace-in-region (make-string (- col-end col-beg) ,ch) beg end))
              (let ((line-offset (- (- col-end col-beg) (- end beg))))
                line-offset)))))
    (cond
     ((bound-and-true-p rectangle-mark-mode)
      (let* ((beg (region-beginning))
             (end (region-end))
             (is-forward (eq (point) end))
             (pos-max-orig (point-max)))
        (apply-on-rectangle
         ;; Make the values global.
         `(lambda (col-beg col-end)
            (progn
              (funcall ,replace-in-range-from-columns-fn col-beg col-end)
              nil))
         beg end)

        ;; Update end.
        (meep--incf end (- (point-max) pos-max-orig))

        ;; Restore the region (without activating it).
        (cond
         (is-forward
          (meep--set-marker beg)
          (goto-char end))
         (t
          (meep--set-marker end)
          (goto-char beg)))))
     (t
      (cond
       ((region-active-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (is-forward (eq (point) end)))
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (let ((end-iter (min end (pos-eol))))
                (when (< (point) end-iter)
                  (let ((line-offset (funcall replace-in-range-fn (point) end-iter)))
                    ;; For variable width chars (mainly tabs).
                    (unless (zerop line-offset)
                      (meep--incf end-iter line-offset)
                      (meep--incf end line-offset))))
                (cond
                 ((eq end-iter end)
                  ;; Break.
                  (goto-char end))
                 (t
                  (goto-char (1+ end-iter))
                  (skip-chars-forward "\n\r" end))))))

          ;; Restore the region (without activating it).
          ;; Only the `end' needs correcting.
          (cond
           (is-forward
            (goto-char end))
           (t
            (meep--set-marker end)))))
       (t
        ;; Mirror the end-of-buffer guard above: at the end of a line point
        ;; is on the newline, so there is no character on this line to
        ;; replace (spanning it would give a negative column count).
        (when (eolp)
          (user-error "Cannot replace at the end of the line"))
        (funcall replace-in-range-fn (point) (1+ (point)))))))))


;;;###autoload
(defun meep-char-insert (ch arg)
  "Read a character CH and insert it or replace the active region.
Insert ARG times."
  (interactive "*cInsert Char:\np")
  ;; Sanitize numeric prefix.
  (when (< arg 0)
    (setq arg (abs arg)))

  (meep--char-is-ok-or-error "insert" ch)

  (let ((insert-in-range-fn
         `(lambda (beg end n) (meep--replace-in-region (make-string n ,ch) beg end)))

        (insert-in-range-from-columns-fn
         `(lambda (col-beg col-end n)
            ;; Make the values global.
            (let ((beg nil)
                  (end nil)
                  (col-beg-found nil)
                  (col-end-found nil))
              (save-excursion
                (setq col-beg-found (move-to-column col-beg))
                (setq beg (point))
                (setq col-end-found (move-to-column col-end))
                (setq end (point))

                ;; The region doesn't intersect this line at all, skip it.
                (unless (eq col-beg-found col-end-found)
                  (meep--replace-in-region (make-string n ,ch) beg end)))))))
    (cond
     ((bound-and-true-p rectangle-mark-mode)
      (apply-on-rectangle
       ;; Make the values global.
       `(lambda (col-beg col-end) (funcall ,insert-in-range-from-columns-fn col-beg col-end ,arg))
       (region-beginning)
       (region-end)))
     (t
      (cond
       ((region-active-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (let ((end-iter (min end (pos-eol))))
                (when (< (point) end-iter)
                  (funcall insert-in-range-fn (point) end-iter arg))
                (cond
                 ((eq end-iter end)
                  ;; Break.
                  (goto-char end))
                 (t
                  (goto-char (1+ end-iter))
                  (skip-chars-forward "\n\r" end))))))))
       (t
        (funcall insert-in-range-fn (point) (point) arg)))))))


;; ---------------------------------------------------------------------------
;; Text Editing: Surround Configuration
;;
;; Surround resolves a single key-press into a delimiter pair via two layers:
;; `meep-surround-alist' maps a key to a semantic symbol (shared across modes),
;; and `meep-surround-pairs' maps that symbol to the concrete delimiters for the
;; current mode (set per-mode by presets).  A key with no alias is taken
;; literally, paired the same way as the bounds-of-char machinery.

(defcustom meep-surround-alist
  ;; Mnemonic markup characters, not initials (b/i/c/s): the key stays clear of
  ;; the letter keys the verbs borrow (replace tracks `meep-char-replace' /
  ;; `meep-insert-change'), so an alias never masks a verb.
  '((?* . bold) (?/ . italic) (?\` . code) (?~ . strike))
  "Alist mapping a surround key to a semantic symbol.

Each entry is `(KEY . SYMBOL)' where KEY is the character read after the
surround dispatch key and SYMBOL names the markup intent (e.g. `bold').  The
symbol is resolved to concrete delimiters via `meep-surround-pairs', typically
populated per `major-mode' by a preset.

This layer is shared across modes so a key means the same intent everywhere,
with the mode supplying the syntax.  A key absent from this alist is taken
literally as a delimiter (paired via `meep-symmetrical-chars')."
  :type '(alist :key-type character :value-type symbol))

(defcustom meep-surround-pairs nil
  "Alist mapping a surround SYMBOL to its delimiter pair.

Each entry is `(SYMBOL . SPEC)' where SYMBOL matches a value in
`meep-surround-alist' and SPEC is either:

  (OPEN . CLOSE)   two delimiter strings (single or multi-character), or
  FUNCTION         a function of no arguments returning such a cons
                   (used for prompted delimiters such as tags).

When nil, falls back to the preset for the current `major-mode'.  A mode that
does not define a symbol simply has no surround for that key, rather than
wrapping with the wrong characters.

To add your own kind, bind a key to a new symbol in `meep-surround-alist' (e.g.
`(?h . heading)'), then map that symbol to its pair here for each mode - in a
`meep-preset-MODE.el', a mode hook, or as a global default.  The symbol is
arbitrary; nothing is special about the built-in `bold' / `italic' / `code' /
`strike'.

A pair declared here is also recognized by delete and replace, even a
single-character bracket the syntax table does not mark as one (e.g. `<' `>') -
unlike the generic fall-back, which drops such brackets to avoid mistaking
operators for them, see `meep--surround-recognition-pairs'."
  :type
  '(alist
    :key-type symbol
    :value-type (choice (cons (string :tag "Open") (string :tag "Close")) (function :tag "Builder"))))

(defcustom meep-surround-mark-result nil
  "When non-nil, surround delete and replace mark the affected content.
Point is left just inside the opening delimiter and the mark just inside the
closing delimiter - without activating the region - so the operated-on content
spans point..mark and can be re-selected (e.g. with `meep-region-activate').
When nil, point and the mark are left where they were before the command.

Applies to the region / point verbs only.  The line-wise and rectangle verbs
operate on multiple disjoint spans with no single span to mark, so they are
unaffected by this option."
  :type 'boolean)

(defun meep--surround-pairs-config ()
  "Return the resolved `meep-surround-pairs' alist for the current buffer."
  (declare (important-return-value t))
  (meep-preset-ensure-variable 'meep-surround-pairs))

(defun meep--surround-pair-valid-p (pair)
  "Return non-nil when PAIR is a `(OPEN . CLOSE)' cons of two non-empty strings.
An empty delimiter would wrap with nothing (and match a zero-width span), so a
builder returning `(\"\" . \"\")' is rejected rather than silently no--op'ing."
  (declare (important-return-value t))
  (and (consp pair)
       (stringp (car pair))
       (not (string-empty-p (car pair)))
       (stringp (cdr pair))
       (not (string-empty-p (cdr pair)))))

(defun meep--surround-pair-from-symbol (sym &optional config)
  "Return the `(OPEN . CLOSE)' pair for SYM, or nil when undefined or malformed.
A function-valued spec is called to build the pair (for prompted delimiters); a
cons of strings is returned as-is.  A spec - literal or builder result - that is
not a cons of two strings yields nil, so a caller cancels cleanly rather than
crashing or silently dropping a delimiter.

A signaling builder also yields nil rather than propagating its error out of the
interactive verb; a `quit' (the user pressing `C-g' at a builder's own prompt) is
not caught, so it still cancels the command.

CONFIG is the resolved `meep--surround-pairs-config' when the caller already holds
it, so an alias resolution need not re-resolve it; it defaults to a fresh resolve."
  (declare (important-return-value t))
  (let* ((spec (cdr (assq sym (or config (meep--surround-pairs-config)))))
         (pair
          (cond
           ((functionp spec)
            (ignore-errors
              (funcall spec)))
           (t
            spec))))
    (and (meep--surround-pair-valid-p pair) pair)))

(defun meep--surround-pair-from-key (ch)
  "Return the `(OPEN . CLOSE)' surround pair for key CH, or nil.
CH is resolved as an alias via `meep-surround-alist' (its symbol looked up in
`meep-surround-pairs'); otherwise CH is taken literally and paired the same way
as the bounds-of-char machinery, see `meep--region-mark-ch-pair-from-char'.

A key that names an alias *unconfigured* in this buffer falls back to a literal
pair, but only when CH is not alphanumeric.  This matters for a delimiter that
doubles as an alias: backtick maps to the `code' alias, so without the fall-back
`t `' (and the direct binding) would fail with \"No surround for that key\" in a
buffer whose pairs lack a `code' entry, rather than wrapping with a literal
backtick - so it behaves like the other eight direct delimiters, none of which is
an alias.  An alphanumeric alias key is a mnemonic (e.g. `b' for bold), so it
stays nil rather than wrapping with the letter itself; and a *configured* alias
whose builder returns nil (a cancelled prompt) is also left nil, so cancelling
still cancels."
  (declare (important-return-value t))
  (let* ((sym (cdr (assq ch meep-surround-alist)))
         ;; Resolve the config once here so the alias branch does not re-resolve it
         ;; inside `meep--surround-pair-from-symbol'; only needed when CH is an alias.
         (config (and sym (meep--surround-pairs-config))))
    (cond
     ((and sym (assq sym config))
      (meep--surround-pair-from-symbol sym config))
     ;; Unconfigured *mnemonic* alias (an alphanumeric key like `b' for bold):
     ;; leave it unresolved rather than wrap with the letter, see above.  The
     ;; `[:alnum:]' class covers non-ASCII letters too (an accented or Greek
     ;; mnemonic), so the test matches the policy, not just the ASCII subset.
     ((and sym (string-match-p "[[:alnum:]]" (char-to-string ch)))
      nil)
     (t
      ;; Order the pair `(OPEN . CLOSE)' regardless of which side CH names, so a
      ;; close delimiter (`]') still wraps as `[...]' rather than `]...['.  The
      ;; motion helper `meep--region-mark-ch-pair-from-char' leads with the typed
      ;; character - right for a search that looks for it, wrong for wrapping - so
      ;; it is the fall-back only for a non-symmetrical CH (a quote or `*'), where
      ;; the order does not matter.
      (or (meep--symmetrical-char-pair-canonical (char-to-string ch))
          (meep--region-mark-ch-pair-from-char ch))))))

(defun meep--surround-recognizable-pair-p (pair)
  "Return non-nil when PAIR may be auto-detected as a surrounding pair.
A single-character bracket (distinct open/close) is excluded unless its open has
paren syntax in the current buffer, so operators like `<' / `>' in code are not
mistaken for brackets.  Same-delimiter and multi-character pairs are always kept.

Applied only to the generic fall-back pairs; an explicitly configured
`meep-surround-pairs' bracket bypasses this, see `meep--surround-recognition-pairs'."
  (declare (important-return-value t))
  (let ((open (car pair))
        (close (cdr pair)))
    (or
     ;; Same delimiter both sides (quotes, markup tokens).
     (string-equal open close)
     ;; Multi-character delimiters (tags, markup tokens).
     (not (and (length= open 1) (length= close 1)))
     ;; Single-character bracket - require real paren syntax in this buffer.
     ;; `open' is already length 1 here (the multi-character arm above).
     (meep--syntax-pair-is-paren-p pair))))

(defun meep--surround-fallback-recognizable-p (pair)
  "Return non-nil when generic fall-back PAIR may be auto-detected in this buffer.
PAIR must be recognizable by shape (`meep--surround-recognizable-pair-p') and, under
the `syntax' backend, additionally be a single paren-syntax bracket.  The fall-back
eligibility predicate for `meep--surround-recognition-pairs', which owns the policy
and its rationale; the mechanism that *locates* a recognized pair is
`meep--syntax-enclosing-pair'."
  (declare (important-return-value t))
  (and (meep--surround-recognizable-pair-p pair)
       (or (not (eq (meep--syntax-backend-resolve) 'syntax)) (meep--syntax-pair-is-paren-p pair))))

(defvar-local meep--surround-recognition-pairs-cache nil
  "Buffer-local memo `(INPUTS . PAIRS)' for `meep--surround-recognition-pairs'.
INPUTS is the resolved configuration PAIRS derives from, plus a snapshot of the
syntax class of each single-character fall-back delimiter; the memo is reused while
INPUTS stays `equal', so a preset switch, a `setq' of a source variable, or a
`modify-syntax-entry' that flips a tracked bracket's paren syntax at run-time
recomputes.")

(defun meep--surround-recognition-pairs ()
  "Return the `(OPEN . CLOSE)' string pairs surround can recognize.
Combines the literal pairs of `meep-surround-pairs' (skipping prompted,
function-valued entries) with `meep-match-bounds-of-char-contextual' and
`meep-symmetrical-chars'.  Earlier entries take priority; duplicates are
removed so the more specific markup wins.

A `meep-surround-pairs' entry is an explicit choice, so it is trusted as-is - a
mode may surround with a bracket the syntax table does not mark as one (e.g. `<'
`>').  The generic fall-backs instead drop single-character brackets lacking
paren syntax, so operators are not mistaken for brackets, see
`meep--surround-recognizable-pair-p'.

Under the `syntax' backend (see `meep-syntax-backend') the generic fall-backs are
narrowed to single-character paren-syntax brackets, so auto-detected quotes and
markup are dropped and point inside a string lands on the enclosing real bracket.
An explicit `meep-surround-pairs' entry is still trusted; a non-paren pair it
names is located by the text backend, see `meep--syntax-enclosing-pair'.

The result is memoized per buffer, see `meep--surround-recognition-pairs-cache'."
  (declare (important-return-value t))
  ;; Resolve the inputs once - they key the memo and feed the build, so a config
  ;; change recomputes without re-running the `seq-filter' / `delete-dups' on every
  ;; surround command.  `major-mode' and `meep-syntax-backend' together fix the
  ;; resolved backend (`meep--syntax-backend-resolve').
  (let* ((configured-src (meep--surround-pairs-config))
         (contextual (meep-preset-ensure-variable 'meep-match-bounds-of-char-contextual))
         ;; Snapshot each single-character fall-back open delimiter's syntax class, so
         ;; a `modify-syntax-entry' that flips a bracket's paren syntax at run-time
         ;; (no mode change) invalidates the memo - the fall-back filter consults it,
         ;; see `meep--surround-recognizable-pair-p' / `meep--syntax-pair-is-paren-p'.
         (syntax-snapshot
          (mapcar
           (lambda (pair)
             (let ((open (car pair)))
               (and (length= open 1) (char-syntax (string-to-char open)))))
           (append contextual meep-symmetrical-chars)))
         (inputs
          (list
           major-mode
           meep-syntax-backend
           configured-src
           contextual
           meep-symmetrical-chars
           syntax-snapshot)))
    (cond
     ((and meep--surround-recognition-pairs-cache
           (equal (car meep--surround-recognition-pairs-cache) inputs))
      (cdr meep--surround-recognition-pairs-cache))
     (t
      ;; Keep only `(OPEN . CLOSE)' string pairs; a function-valued (prompted)
      ;; entry or a malformed spec cannot be recognized, see
      ;; `meep--surround-pair-valid-p'.
      (let* ((configured
              (mapcar
               #'cdr
               (seq-filter
                (lambda (entry) (meep--surround-pair-valid-p (cdr entry))) configured-src)))
             ;; The generic fall-backs are filtered by the backend's eligibility
             ;; policy, see `meep--surround-fallback-recognizable-p'.
             (fallbacks
              (seq-filter
               #'meep--surround-fallback-recognizable-p
               (append contextual meep-symmetrical-chars)))
             ;; Trust the explicit config; both parts are freshly consed, so
             ;; `delete-dups' may mutate the spine safely.
             (pairs (delete-dups (append configured fallbacks))))
        (setq meep--surround-recognition-pairs-cache (cons inputs pairs))
        pairs)))))


;; ---------------------------------------------------------------------------
;; Text Editing: Surround Finder
;;
;; Locate the delimiter pair around an anchor for delete/replace.  The same
;; finder serves both the point-anchored (innermost) and line-anchored
;; (outermost) cases: it prefers the pair formed by the anchor's own edges,
;; falling back to an outward search for the nearest enclosing pair.  A
;; line-anchored caller passes the line's content as the anchor and the line as
;; the search clamp, so the outward search finds nothing and only the
;; edges-first case can match (cleanly wrapped lines).

(defun meep--surround-edges-balanced-p (inner-beg inner-end open close)
  "Return non-nil when OPEN left of INNER-BEG pairs with CLOSE right of INNER-END.
INNER-BEG..INNER-END is the span between the two delimiters; it must be balanced
so the outer OPEN pairs with the outer CLOSE rather than a delimiter inside."
  (declare (important-return-value t))
  (save-excursion
    (save-match-data
      (cond
       ;; Same delimiter both sides (quotes, markdown): the span must not contain
       ;; the token, else the first token would pair with an inner one.  Match
       ;; case-sensitively, as `meep--region-mark-bounds-of-char-calc' does for this
       ;; case, so a letter-bearing token does not pair with a different-case one.
       ((string-equal open close)
        (let ((case-fold-search nil))
          (goto-char inner-beg)
          (not (search-forward open inner-end t))))
       (t
        ;; Distinct brackets: the depth must stay non-negative and end at zero
        ;; (no premature close, no leftover open within the span).  Match
        ;; case-sensitively, as the same-delimiter branch above and the outward
        ;; search `meep--syntax-enclosing-pair-from-text' both do, so a
        ;; different-case token inside the span is not counted as a delimiter.
        (let ((case-fold-search nil)
              (re (meep--bracket-pair-regex open close))
              (depth 0)
              (ok t))
          (goto-char inner-beg)
          (while (and ok (re-search-forward re inner-end t))
            (cond
             ((match-beginning 1)
              (meep--incf depth))
             ((zerop depth)
              ;; Premature close, the outer open pairs inside the span.
              (setq ok nil))
             (t
              (meep--decf depth))))
          (and ok (zerop depth))))))))

(defun meep--surround-bounds-edges (anchor pairs)
  "Return `((BEG . END) . (OPEN . CLOSE))' when ANCHOR's own edges form a pair.
Try each entry in PAIRS; the first whose OPEN starts ANCHOR and whose CLOSE
ends it (with a balanced span between) wins.  Return nil otherwise."
  (declare (important-return-value t))
  (let ((beg (car anchor))
        (end (cdr anchor))
        (result nil))
    (while (and pairs (null result))
      (let* ((pair (pop pairs))
             (open (car pair))
             (close (cdr pair))
             (olen (length open))
             (clen (length close)))
        ;; Need room for both delimiters without overlap.
        (when (and (<= (+ beg olen clen) end)
                   (string-equal (buffer-substring-no-properties beg (+ beg olen)) open)
                   (string-equal (buffer-substring-no-properties (- end clen) end) close)
                   (meep--surround-edges-balanced-p (+ beg olen) (- end clen) open close))
          (setq result (cons (cons beg end) pair)))))
    result))

(defun meep--surround-bounds-search (anchor clamp pairs &optional match-at-start clamp-single-line)
  "Return the nearest enclosing pair around ANCHOR as `((BEG . END) . PAIR)'.
ANCHOR is the `(BEG . END)' search anchor and CLAMP the `(MIN . MAX)' search
limit.  Each entry in PAIRS is tried; the innermost enclosing pair across all
delimiter types wins.  MATCH-AT-START is passed to
`meep--region-mark-bounds-of-char-calc': non-nil treats a delimiter sitting at
ANCHOR's start as enclosing (cursor-on-open); the outward peel passes it nil so
the pair ANCHOR already spans is skipped for the one enclosing it.
CLAMP-SINGLE-LINE non-nil means CLAMP spans one line, so the paragraph-parity
fall-back - which only recovers a *multi-line* same-delimiter span - is skipped,
saving a forward+backward scan per same-delimiter pair.  `meep--surround-targets-enclosing'
resolves it once (CLAMP is fixed across the outward peel) so it is not re-probed
per layer.  Return nil when none enclose ANCHOR within CLAMP."
  (declare (important-return-value t))
  ;; Copy CLAMP, narrowed in-place to the best enclosing pair as nearer ones
  ;; are found.
  (let ((bounds-limit (cons (car clamp) (cdr clamp)))
        (result nil)
        (result-pair nil))
    (dolist (pair pairs)
      ;; Same-delimiter: try per-line parity first, then paragraph parity on a miss.
      ;; Per-line is tried first because it ignores a stray same-delimiter token on
      ;; an earlier line of the same paragraph (which paragraph parity would wrongly
      ;; count, rejecting a real single-line pair); paragraph parity then catches a
      ;; genuine multi-line span the per-line scan cannot.  Cursor-on-open is handled
      ;; inside the calc (paragraph-scoped) regardless, so a wrong per-line match
      ;; cannot stand in for the pair at point.
      (let* ((paragraph-fallback
              ;; A same-delimiter span may cross lines, so let the calc accept a
              ;; paragraph-scope opener when per-line parity rejects it; a
              ;; single-line clamp (every rectangle row) can hold no such span, so
              ;; skip the fall-back there.
              (and (not clamp-single-line) (string-equal (car pair) (cdr pair))))
             (bounds-test
              (meep--region-mark-bounds-of-char-calc anchor bounds-limit 1 pair
                                                     match-at-start
                                                     paragraph-fallback)))
        ;; Keep the innermost across pair types; the recognized pairs are ordered
        ;; multi-character-first (`meep-region-mark-bounds-of-char-contextual-impl'
        ;; runs the same reduction with a one-sided clamp).
        (when (meep--bounds-supersedes-p bounds-test result result-pair)
          (setq result bounds-test)
          (setq result-pair pair)
          ;; Narrow the search window to the found pair so a later pair wins only
          ;; when it is nearer the anchor (the innermost enclosing pair).  Both
          ;; edges tighten, else a same-delimiter forward scan re-runs to the
          ;; clamp end (`point-max' in the region path) every iteration.
          (setcar bounds-limit (car bounds-test))
          (setcdr bounds-limit (cdr bounds-test)))))
    (when result
      (cons result result-pair))))

(defun meep--surround-bounds-at (anchor clamp pairs &optional clamp-single-line)
  "Return the surrounding pair for ANCHOR as `((BEG . END) . (OPEN . CLOSE))'.
ANCHOR is the `(BEG . END)' search anchor, CLAMP the `(MIN . MAX)' search limit,
and PAIRS the recognized delimiter list.  Prefer the pair formed by ANCHOR's own
edges (the outermost / region-strip case); otherwise search outward within CLAMP
for the nearest enclosing pair.  BEG sits at the open delimiter and END just past
the close, so the delimiters occupy `[BEG BEG+len(OPEN))' and `[END-len(CLOSE)
END)'.  CLAMP-SINGLE-LINE is forwarded to `meep--surround-bounds-search'.  Return
nil when no pair is found."
  (declare (important-return-value t))
  ;; This is the innermost find, so match a delimiter at ANCHOR's start
  ;; (cursor-on-open); the count-peel searches outward from here without it.
  (or (meep--surround-bounds-edges anchor pairs)
      (meep--surround-bounds-search anchor clamp pairs t clamp-single-line)))


;; ---------------------------------------------------------------------------
;; Text Editing: Surround Insert/Delete

;; Scope iteration helpers shared by the add (insert) and delete/replace paths,
;; so both agree on what "each line's content" and "a rectangle row" mean.

(defun meep--surround-row-span (col-beg col-end)
  "Return the line's `(BEG . END)' span between COL-BEG/COL-END, or nil when empty.
An empty span (BEG equals END) means the line is too short or the columns fall
within a TAB - there is no character to surround, so the row is skipped.  Built
on `meep--rectangle-row-span'."
  (declare (important-return-value t))
  (let ((span (meep--rectangle-row-span col-beg col-end)))
    (and (< (car span) (cdr span)) span)))

(defmacro meep--with-temp-marker (spec &rest body)
  "Evaluate BODY with a marker bound per SPEC, released afterward.
SPEC is `(VAR POSITION &optional INSERTION-TYPE)': VAR is bound to a marker at
POSITION (INSERTION-TYPE as in `copy-marker'), then set to nil on exit - via
`unwind-protect' so it is released even when BODY signals (a read-only edit or a
quit), rather than dangling in the buffer."
  (declare (indent 1))
  (let ((var (nth 0 spec))
        (position (nth 1 spec))
        (insertion-type (nth 2 spec)))
    `(let ((,var (copy-marker ,position ,insertion-type)))
       (unwind-protect
           (progn
             ,@body)
         (set-marker ,var nil)))))

(defun meep--surround-for-each-line-content (beg end fn &optional clamp-to-span)
  "Call FN once per line spanning BEG..END with that line's content bounds.
FN is called as `(FN CONTENT LINE-BOUNDS)' where CONTENT is the `(CBEG . CEND)'
bounds contracted past surrounding blank-space and LINE-BOUNDS the raw
`(BOL . EOL)'.  When CLAMP-TO-SPAN is non-nil, CONTENT is the line clamped to
BEG..END before contracting, so a partial first / last line yields only its
selected columns (the end marker tracks delimiters inserted on earlier lines);
otherwise CONTENT spans the whole line.  Point and the mark are restored on exit
\(`save-mark-and-excursion') and the end marker is managed here; FN must not rely
on point.  The mark is held to protect against an FN that moves it - the
line-wise `meep--surround-apply-targets' deliberately does not (it leaves the
mark to the region path, see `meep-surround-mark-result'), so callers handle
`deactivate-mark' themselves."
  (save-mark-and-excursion
    (meep--with-temp-marker (end-mark end)
      (goto-char beg)
      (forward-line 0)
      (while (< (point) (marker-position end-mark))
        (let* ((line-bounds (cons (pos-bol) (pos-eol)))
               (span
                (cond
                 (clamp-to-span
                  (cons
                   (max (car line-bounds) beg) (min (cdr line-bounds) (marker-position end-mark))))
                 (t
                  line-bounds)))
               (content (meep--bounds-contract-by-chars span "[:blank:]" "[:blank:]")))
          (funcall fn content line-bounds))
        (forward-line 1)))))

(defun meep--surround-add-scope ()
  "Return the implied region for a surround add, or nil when there is none.
Like `meep--region-or-mark-bounds', but an empty `(P . P)' span - a mark set at
point - counts as no selection, so the caller wraps point (or the whole line)
instead of an empty span."
  (declare (important-return-value t))
  (let ((scope (meep--region-or-mark-bounds)))
    (and scope (< (car scope) (cdr scope)) scope)))

(defun meep--surround-count-sanitize (n)
  "Clamp a surround repeat / depth count N to at least one.
A negative N folds to its magnitude and zero clamps to one.  Shared by the add path
\(`meep--surround-add-impl', where N repeats the delimiter) and the verbs that take N
as an enclosing-pair depth (`meep--surround-operate' for delete / replace,
`meep--surround-region-activate-impl' for region-activate).

Unlike the bounds-of-char motions, where a zero count is a no-op (see
`meep--region-mark-bounds-of-char-impl'), a surround verb always acts: a stray
`C-u 0' wraps or peels once rather than doing nothing - a leftover prefix is likelier
than an intent to wrap zero times.  See test `surround-add-zero-count-clamps-to-one'."
  (declare (important-return-value t))
  (max 1 (abs n)))

(defun meep--surround-add-impl (open close arg line-wise)
  "Surround the region with OPEN and CLOSE strings, each repeated ARG times.
When LINE-WISE is non-nil, surround each line's content, contracted past
surrounding blank-space so indentation and trailing blanks stay outside the
delimiters and an all-blank line is left unwrapped; with a rectangle region,
surround each row's column span; otherwise use the region bounds.
A negative ARG folds to its magnitude; zero clamps to one repetition."
  (setq arg (meep--surround-count-sanitize arg))

  (let* ((prefix (apply #'concat (make-list arg open)))
         (suffix (apply #'concat (make-list arg close)))
         (prefix-len (length prefix))

         (surround-in-range-fn
          (lambda (beg end)
            (save-excursion
              (goto-char end)
              (insert suffix)
              (goto-char beg)
              (insert prefix))))

         ;; A rectangle row is wrapped exactly like any range, on its column span;
         ;; an empty span (too short, or columns inside a TAB) is skipped.
         (surround-in-range-from-columns-fn
          (lambda (col-beg col-end)
            (let ((span (meep--surround-row-span col-beg col-end)))
              (when span
                (funcall surround-in-range-fn (car span) (cdr span))))))

         ;; Capture the region edges before editing.  Only the left edge (the
         ;; smaller of point / mark, or point alone when no mark is set) is
         ;; adjusted afterward: it must step past a prefix inserted exactly there
         ;; to sit just inside the open, while the right edge tracks that prefix on
         ;; its own (its position shifts by the prefix length as the prefix goes in
         ;; to its left).
         (p-orig (point))
         (m-orig (mark t))
         (lo (min p-orig (or m-orig p-orig)))
         (point-is-left (eq p-orig lo)))

    ;; A marker at the left edge, with an advancing insertion-type, lands just
    ;; inside the open when a prefix goes in exactly there and stays put when the
    ;; prefix goes in further along - line-wise / rectangle add inserts at each
    ;; line's content-start, past any indentation, not at the region edge.  This
    ;; replaces matching the inserted text (which could not tell an inserted prefix
    ;; from identical leading content) with the marker mechanism the delete /
    ;; replace path already uses, see `meep--surround-apply-edits'.
    (meep--with-temp-marker (left-mark lo t)
      (let ((wrapped-empty nil)
            (rect (bound-and-true-p rectangle-mark-mode)))
        (cond
         (rect
          (apply-on-rectangle surround-in-range-from-columns-fn (region-beginning) (region-end)))
         ;; Surround each line's content, contracted past blank-space so
         ;; indentation and trailing blanks stay outside the delimiters.  A partial
         ;; first / last line wraps only its selected columns, not text outside the
         ;; selection: add is constructive so it must not wrap what was never
         ;; selected (delete / replace instead operate on the whole line).  The
         ;; scope is the active or implied region, else the current line.  See
         ;; `meep--region-or-mark-bounds'.
         (line-wise
          (let* ((scope (meep--surround-add-scope))
                 ;; Fall back to the whole line when there is no selection (an empty
                 ;; span included), else every line clamps to nothing and the add
                 ;; silently no-ops, the opposite of the no-mark state.
                 (bounds (or scope (cons (pos-bol) (pos-eol))))
                 (beg (car bounds))
                 (end (cdr bounds)))
            ;; CLAMP-TO-SPAN so the iterator hands back each line's selected,
            ;; blank-contracted span directly; a blank or fully-trimmed line
            ;; collapses to an empty span and is skipped.
            (meep--surround-for-each-line-content beg end
                                                  (lambda (content _line-bounds)
                                                    (when (< (car content) (cdr content))
                                                      (funcall surround-in-range-fn
                                                               (car content)
                                                               (cdr content))))
                                                  t)))
         (t
          (let ((scope (meep--surround-add-scope)))
            (cond
             (scope
              (funcall surround-in-range-fn (car scope) (cdr scope)))
             (t
              ;; No selection: wrap the empty span at point.  There is no left edge
              ;; to preserve, so step point inside the inserted pair directly.
              (setq wrapped-empty t)
              (funcall surround-in-range-fn (point) (point))
              (goto-char (+ (point) prefix-len)))))))

        ;; Move the region's left edge to just inside the open.  The right edge
        ;; already sits inside, before the suffix, so it is never moved - vital for
        ;; a symmetric delimiter, where the suffix equals the prefix.  For a
        ;; rectangle the advancing marker at `lo' tracks the prefix on the first
        ;; row, so the same single-edge adjustment applies (point / mark stay on
        ;; their respective rows; only the column of the left corner shifts).
        ;;
        ;; NOTE: when the left edge sits in a line's leading blank-space (an
        ;; indented or blank-led line-wise edge) the prefix goes in further along,
        ;; at the content-start, so the advancing marker never tracks it and the
        ;; edge stays put - point / mark then land just *outside* the wrap.  This
        ;; reads like a bug but is deliberate: the edge is kept where the caller put
        ;; it, and only steps inside when the prefix lands exactly there.  A naive
        ;; "fix" re-homes the marker onto the content-start to drag it inside, which
        ;; reintroduces the prefix-length shift this design dropped and breaks
        ;; `surround-add-lines-region-edge-before-indent'.  Do *not* "fix" it.
        (unless wrapped-empty
          (cond
           (point-is-left
            (goto-char left-mark))
           (t
            (set-marker (mark-marker) left-mark))))))))


;; Surround delete/replace share a finder pass (the targets) and an edit pass.
;; Delete removes each delimiter, replace swaps in new ones; the targeting and the
;; right-to-left edit application are common to both.

(defun meep--surround-content-start (target)
  "Return the content start of TARGET `((BEG . END) . (OPEN . CLOSE))'.
That is the position just inside the opening delimiter."
  (declare (important-return-value t))
  (let ((bounds (car target))
        (pair (cdr target)))
    (+ (car bounds) (length (car pair)))))

(defun meep--surround-content-end (target)
  "Return the content end of TARGET `((BEG . END) . (OPEN . CLOSE))'.
That is the position just inside the closing delimiter."
  (declare (important-return-value t))
  (let ((bounds (car target))
        (pair (cdr target)))
    (- (cdr bounds) (length (cdr pair)))))

(defun meep--surround-targets-lines (anchor pairs count)
  "Return the outermost COUNT nested pairs around ANCHOR, outermost first.
Each target is `((BEG . END) . (OPEN . CLOSE))'.  ANCHOR is the `(BEG . END)'
search anchor and PAIRS the recognized delimiters.  Peel inward: each layer is
formed by the shrinking inner's own edges (`meep--surround-bounds-edges'), so the
outward enclosing-pair fall-back is not used - with the clamp staying the whole
line it would re-find the same pair as ANCHOR contracts, emitting duplicate
targets that double-delete.  Fewer than COUNT layers stops early.  Return nil when
none are found.  The line-wise counterpart of `meep--surround-targets-enclosing'."
  (declare (important-return-value t))
  (let ((targets nil)
        (i 0))
    (while (< i count)
      (let ((found (meep--surround-bounds-edges anchor pairs)))
        (cond
         (found
          (push found targets)
          ;; Next layer lives inside this pair's delimiters; contract past
          ;; blank-space so an inner pair padded by spaces is still found.
          (setq anchor
                (meep--bounds-contract-by-chars
                 (cons
                  (meep--surround-content-start found) (meep--surround-content-end found))
                 "[:blank:]" "[:blank:]"))
          (meep--incf i))
         (t
          ;; Fewer than COUNT layers, stop.
          (setq i count)))))
    (nreverse targets)))

(defun meep--surround-targets-enclosing (anchor clamp pairs count &optional single-line)
  "Return the COUNT-th enclosing pair around ANCHOR as a one-element list, or nil.
The target is `((BEG . END) . (OPEN . CLOSE))'.  ANCHOR is the `(BEG . END)'
search anchor, CLAMP the `(MIN . MAX)' search limit, and PAIRS the recognized
delimiters.  Peel outward from the innermost; when COUNT exceeds the available
depth, clamp to the outermost pair rather than discard the pair already found,
matching `meep--surround-targets-lines' which peels what exists.  SINGLE-LINE
asserts CLAMP spans one line (every rectangle row); CLAMP is fixed across the
peel, so its single-line-ness is resolved once here (honouring SINGLE-LINE, else
probing for a newline) rather than re-probed per layer."
  (declare (important-return-value t))
  (let* ((clamp-single-line
          (or single-line
              ;; Probe outward from the anchor, not from `(car clamp)': any
              ;; newline within CLAMP proves it multi-line, and the nearest one
              ;; to the cursor settles it without scanning the leading span.  On
              ;; the region path CLAMP is `point-min'/`point-max', so a probe from
              ;; its start would scan every character before the cursor on a
              ;; single-long-line (minified) buffer.  The two probes meet at the
              ;; anchor and together cover the whole clamp.
              (save-match-data
                (save-excursion
                  (goto-char (car anchor))
                  (and (not (search-forward "\n" (cdr clamp) t))
                       (progn
                         (goto-char (car anchor))
                         (not (search-backward "\n" (car clamp) t))))))))
         (found
          (meep--peel-outward
           (meep--surround-bounds-at anchor clamp pairs clamp-single-line) count
           ;; Search outward without matching at the start: the layer's bounds
           ;; begin on its own open delimiter, so cursor-on-open would re-find it
           ;; instead of the pair enclosing it.
           (lambda (layer)
             (meep--surround-bounds-search (car layer) clamp pairs nil clamp-single-line)))))
    (when found
      (list found))))

(defun meep--surround-edits-from-targets (targets new-pair)
  "Return edit specs `(START END REPLACEMENT)' for TARGETS.
NEW-PAIR nil deletes each delimiter, otherwise replaces the open and close with
its `(OPEN . CLOSE)' strings."
  (declare (important-return-value t))
  (let ((edits nil)
        (new-open (car new-pair))
        (new-close (cdr new-pair)))
    (dolist (target targets)
      (let* ((bounds (car target))
             (beg (car bounds))
             (end (cdr bounds)))
        (push (list beg (meep--surround-content-start target) new-open) edits)
        (push (list (meep--surround-content-end target) end new-close) edits)))
    edits))

(defun meep--surround-apply-edits (edits content-start)
  "Apply EDITS right-to-left, leaving point at CONTENT-START.
Each edit is `(START END REPLACEMENT)'; a nil REPLACEMENT deletes the span.
Applying from the rightmost span keeps earlier positions valid."
  (meep--with-temp-marker (m content-start t)
    (dolist (edit (sort edits (lambda (a b) (> (car a) (car b)))))
      (let ((start (nth 0 edit))
            (end (nth 1 edit))
            (repl (nth 2 edit)))
        (delete-region start end)
        (when repl
          (goto-char start)
          (insert repl))))
    (goto-char m)))

(defun meep--surround-apply-targets (targets new-pair &optional allow-mark)
  "Apply the surround edits for TARGETS.
When ALLOW-MARK and `meep-surround-mark-result' are non-nil and there is a single
target, leave point just inside the target's opening delimiter and set the mark
\(without activating the region) just inside its closing delimiter, so the
operated-on content spans point..mark and can be re-selected; otherwise leave
point and the mark where they were.  ALLOW-MARK is set only by the single-span
region path; the line-wise and rectangle paths leave it nil - their edits run
inside a `save-mark-and-excursion' (or span disjoint content) so a mark would be
restored away or have no single span to cover.  NEW-PAIR nil deletes each
delimiter, otherwise replaces with its `(OPEN . CLOSE)'.  Empty TARGETS is a
no-op."
  (when targets
    (let ((edits (meep--surround-edits-from-targets targets new-pair))
          (content-start (meep--surround-content-start (car targets))))
      (cond
       ((and allow-mark meep-surround-mark-result (null (cdr targets)))
        ;; Mark the content end before editing; the marker tracks the edits so it
        ;; lands just inside the (new) closing delimiter, opposite the point that
        ;; `meep--surround-apply-edits' leaves at the opening.
        (meep--with-temp-marker (end-mark (meep--surround-content-end (car targets)) nil)
          (meep--surround-apply-edits edits content-start)
          (set-marker (mark-marker) end-mark)))
       (t
        ;; Restore point; `meep--surround-apply-edits' would leave it at the
        ;; content start.  The marker tracks the edits so point returns to where
        ;; the user left it.
        (meep--with-temp-marker (pt (point) nil)
          (meep--surround-apply-edits edits content-start)
          (goto-char pt)))))))

(defun meep--surround-operate-finish (applied)
  "Conclude a surround operate given whether any pair was APPLIED.
On success drop the active region (the edit consumes the selection); on a miss
report that no pair was found.  The region / rectangle path applies through
`meep--surround-operate-apply', which drops the region itself before editing (to
keep a mark it may set), so it reports only the miss here; the line-wise path,
editing per line, calls this once after its loop."
  (cond
   (applied
    (when (region-active-p)
      (deactivate-mark)))
   (t
    (message "No surrounding pair found"))))

(defun meep--surround-operate-apply (targets new-pair &optional allow-mark)
  "Apply the collected TARGETS, or message when there are none.
Drop an active region first, as a successful edit consumes the selection;
NEW-PAIR nil deletes, otherwise replaces with its `(OPEN . CLOSE)'.  Shared by the
region and rectangle paths; the line-wise path edits and reports per line.
ALLOW-MARK is threaded to `meep--surround-apply-targets' - only the region path
sets it, the rectangle path (multiple disjoint spans) leaves it nil."
  (cond
   (targets
    (when (region-active-p)
      (deactivate-mark))
    (meep--surround-apply-targets targets new-pair allow-mark))
   (t
    (meep--surround-operate-finish nil))))

(defun meep--surround-operate-region (count pairs new-pair)
  "Delete or replace the COUNT-th enclosing pair around the region or point.
PAIRS is the recognized delimiters; NEW-PAIR nil deletes, otherwise it is the
`(OPEN . CLOSE)' replacement.  Message when no pair is found."
  ;; Anchor on the active region or point - not the implied region.  Delete and
  ;; replace look for an enclosing pair, and that search is almost always run from
  ;; inside the pair; a prior motion's span would only pull the target outward.
  (let* ((anchor (meep--region-mark-bounds-init))
         (targets
          (meep--surround-targets-enclosing anchor (cons (point-min) (point-max)) pairs count)))
    ;; ALLOW-MARK: the single-span region path is the only one that honours
    ;; `meep-surround-mark-result' (the line-wise and rectangle paths leave it
    ;; nil), see `meep--surround-apply-targets'.
    (meep--surround-operate-apply targets new-pair t)))

(defun meep--surround-region-activate-impl (count pairs)
  "Activate the region inside the COUNT-th enclosing pair around the region or point.
PAIRS is the recognized delimiters.  The pair is found as for surround delete (see
`meep--surround-operate-region'), but instead of editing, the region is activated
over the content between the delimiters - point just inside the open, mark just
inside the close - so it can be operated on next.  This never modifies the buffer,
so unlike the delete / replace verbs it works in a read-only buffer.  COUNT is
clamped like the other verbs (`meep--surround-count-sanitize').  Message when no
pair is found.

Single-region only: there is no line-wise or rectangle variant, since a single
region cannot cover the disjoint per-line / per-row spans those would target."
  (let* ((count (meep--surround-count-sanitize count))
         (anchor (meep--region-mark-bounds-init))
         (targets
          (meep--surround-targets-enclosing anchor (cons (point-min) (point-max)) pairs count)))
    (cond
     (targets
      ;; A single enclosing pair (the region path is single-span); activate over its
      ;; inner content with point at the start and the mark at the end, matching the
      ;; mark-bounds-of-char motions invoked from inside a pair.
      (let ((target (car targets)))
        (meep--region-mark-bounds-to-region
         (cons (meep--surround-content-start target) (meep--surround-content-end target)) nil)))
     (t
      (message "No surrounding pair found")))))

(defun meep--surround-operate-lines (count pairs new-pair)
  "Delete or replace the outermost COUNT pairs on each line in scope.
PAIRS is the recognized delimiters; NEW-PAIR nil deletes, otherwise it is the
`(OPEN . CLOSE)' replacement.  Lines with no surrounding pair are skipped."
  ;; Scope is the active region or the current line - not the implied region,
  ;; see `meep--surround-operate-region'.
  (let* ((region (region-active-p))
         (bounds
          (cond
           (region
            (cons (region-beginning) (region-end)))
           (t
            (cons (pos-bol) (pos-eol)))))
         (beg (car bounds))
         (end (cdr bounds))
         (applied nil))
    (meep--surround-for-each-line-content
     beg end
     (lambda (content _line-bounds)
       (let ((targets (meep--surround-targets-lines content pairs count)))
         (when targets
           (setq applied t)
           (meep--surround-apply-targets targets new-pair)))))
    ;; Edits run per line, so report once here rather than through
    ;; `meep--surround-operate-apply' (which the region / rectangle paths use).
    (meep--surround-operate-finish applied)))

(defun meep--surround-operate-rectangle (count pairs new-pair)
  "Delete or replace the surrounding pair around each rectangle row's column span.
COUNT is the enclosing-pair depth and PAIRS the recognized delimiters; NEW-PAIR nil
deletes, otherwise it is the `(OPEN . CLOSE)' replacement.  Rows whose column span
holds no pair are skipped."
  (let ((all-targets nil))
    ;; Collect targets for every row first without editing, so the recorded
    ;; positions stay valid; the edits are applied together afterward.
    (apply-on-rectangle
     (lambda (col-beg col-end)
       ;; Skip rows the column range does not intersect.
       (let ((span (meep--surround-row-span col-beg col-end)))
         (when span
           ;; Clamp the search start to the column span, not the line start, so a
           ;; pair whose open sits left of the rectangle is not found and edited.
           ;; Leave the end at the line so a pair opening inside the span but
           ;; closing past it is still matched (the open-at-span-start row case).
           (let ((targets
                  ;; The clamp is the row's column span (single-line by
                  ;; construction), so skip the per-row newline probe.
                  (meep--surround-targets-enclosing span (cons (car span) (pos-eol)) pairs count
                                                    t)))
             (when targets
               ;; Prepend, then reverse once below; appending per row is O(rows^2).
               (setq all-targets (nconc targets all-targets)))))))
     (region-beginning) (region-end))
    (setq all-targets (nreverse all-targets))
    (meep--surround-operate-apply all-targets new-pair)))

(defun meep--surround-operate (count pairs new-pair line-wise)
  "Route a surround delete/replace to the rectangle, line, or region handler.
COUNT, PAIRS and LINE-WISE are forwarded to the chosen handler; NEW-PAIR nil
deletes, otherwise it is the `(OPEN . CLOSE)' replacement."
  (setq count (meep--surround-count-sanitize count))
  (cond
   ((bound-and-true-p rectangle-mark-mode)
    (meep--surround-operate-rectangle count pairs new-pair))
   (line-wise
    (meep--surround-operate-lines count pairs new-pair))
   (t
    (meep--surround-operate-region count pairs new-pair))))

(defun meep--surround-delete-impl (arg line-wise)
  "Delete the delimiters surrounding the region or point, ARG times.
When LINE-WISE is non-nil, operate on each line's outermost pair(s); with a
rectangle region, operate on each row's column span.
The pair is found among every recognized delimiter; the type-directed search of
`meep-surround-delete' instead operates only on the pair of the source delimiter
the user types."
  (meep--surround-operate arg (meep--surround-recognition-pairs) nil line-wise))

(defun meep--surround-replace-impl (open close arg line-wise)
  "Replace the surrounding delimiters with OPEN and CLOSE, ARG times.
When LINE-WISE is non-nil, operate on each line's outermost pair(s); with a
rectangle region, operate on each row's column span.
See `meep--surround-delete-impl' on the recognized-pair search."
  (meep--surround-operate arg (meep--surround-recognition-pairs) (cons open close) line-wise))


;; ---------------------------------------------------------------------------
;; Text Editing: Surround
;;
;; Surround is just commands the user binds directly - separate add / replace /
;; delete verbs, plus their line-wise variants - the same as the rest of the
;; keymap.  Each verb reads at most one further key, the delimiter; only
;; `meep-surround-delete-at-point' (and its line-wise variant) reads none.
;;
;; That delimiter read is a real keymap, built at run-time from the buffer's
;; `meep-surround-pairs' (see `meep--surround-make-delimiter-map') and installed
;; transiently (`meep--surround-set-keymap'), so it always reflects the current
;; buffer and the echo leads with the verb's prompt.  Every key in the map routes
;; to the verb's named event command (see `meep--surround-event-command'), which
;; resolves the invoking key with `meep--surround-from-event'; the verb lives in
;; the binding, so one command resolves any delimiter (alias, literal,
;; multi-character, or `RET' to prompt).  The one map serves a key-bound verb and a
;; direct `M-x' call alike - there is no prefix keymap to dispatch through.
;;
;; Resolving the delimiter from the event - rather than capturing it in a closure -
;; is what keeps each a *named* command, so `repeat-fu' can record and replay the
;; gesture.  The dispatch commands below rely on this and do not restate it.
;;
;; By-type surround is not a separate dispatch: it is the same verbs bound directly.
;; `meep-surround-replace-by-type' replaces and `meep-surround-delete' deletes, each
;; reading a source delimiter that names which pair *type* to act on (the nearest
;; pair of that type, skipping closer pairs of other types).  The default binds them
;; under a plain `s t' / `s T' prefix.

(defun meep--surround-mirror-string (open)
  "Return a closing delimiter mirroring the opening string OPEN.
Each character is paired via `meep-symmetrical-chars', then the order is reversed,
so the close is the geometric mirror of the open.  Used only as the minibuffer
default for an arbitrary pair; the user may edit it."
  (declare (important-return-value t))
  ;; NOTE: `push' reverses the characters as it builds, which is intentional - the
  ;; close is the geometric mirror of the open, so nested pairs read correctly:
  ;; `([' -> `])', `<%' -> `%>', `{%' -> `%}'.  Do *not* add an `nreverse' here
  ;; (that would give `)]' / `>%').  A tag-like open has no character mirror - `<div>'
  ;; yields `<vid>', not `</div>' - but as an editable default that is acceptable.
  (let ((result nil))
    (dolist (ch (append open nil))
      (push (cdr (meep--region-mark-ch-pair-from-char ch)) result))
    (apply #'concat result)))

(defun meep--surround-read-literal-pair ()
  "Read an arbitrary `(OPEN . CLOSE)' surround pair from the minibuffer.
An empty OPEN or CLOSE cancels (returns nil) rather than building a one-sided
pair.  An empty close is the dangerous case: add would wrap with the open alone,
and replace would delete the existing close and insert nothing - silently
dropping it.  This is the same non-empty invariant `meep--surround-pair-valid-p'
enforces for every other pair source, applied here since this reader bypasses it."
  (let ((open (read-string "Surround open: ")))
    (cond
     ((string-empty-p open)
      nil)
     (t
      (let ((close (read-string "Surround close: " (meep--surround-mirror-string open))))
        (cond
         ((string-empty-p close)
          nil)
         (t
          (cons open close))))))))

(defun meep--surround-key-is-delimiter-p (ch)
  "Return non-nil when CH is a character usable as a literal surround delimiter.
Only printable characters qualify; non-character events and control characters
such as `C-g' and `ESC' are rejected, so they cancel the dispatch rather than
wrapping with one."
  (declare (important-return-value t))
  (and (characterp ch) (aref printable-chars ch) t))

(defun meep--surround-pair-from-event (&optional ev)
  "Resolve event EV to a surround `(OPEN . CLOSE)' pair, or nil.
EV defaults to `last-command-event'.  `RET' / `<return>' prompts for an arbitrary
pair; a printable character names an alias or literal pair; any other event
cancels.  Message and return nil when no pair results, so callers act only on a
non-nil return."
  (declare (important-return-value t))
  (let ((ev (or ev last-command-event)))
    (cond
     ;; `RET' / `<return>' prompts for an arbitrary pair.  `C-j' / `LFD' (`?\n')
     ;; is left out so it cancels like every other control key, keeping it free
     ;; for a later meaning.
     ((memq ev '(?\r return))
      (meep--surround-read-literal-pair))
     ;; A printable character names a literal delimiter; a configured alias key
     ;; resolves even when not printable (e.g. a `TAB' alias), so it is not mistaken
     ;; for a cancel.  Any other event (a control key, a non-character) cancels.
     ((and (characterp ev)
           (or (meep--surround-key-is-delimiter-p ev) (assq ev meep-surround-alist)))
      (or (meep--surround-pair-from-key ev)
          (progn
            (message "No surround for that key")
            nil)))
     (t
      (message "Surround cancelled")
      nil))))

(defun meep--surround-from-event (count line-wise action)
  "Apply ACTION with the delimiter named by `last-command-event'.
COUNT is the nesting depth and LINE-WISE selects per-line operation.  ACTION is
`add' to wrap with the delimiter, `replace' to replace the surrounding pair with
it, `delete' to remove the surrounding pair *of that delimiter's type* (the key
chooses which surround to strip), or `region-activate' to select the content of
that pair instead of editing it.  The delimiter is resolved by
`meep--surround-pair-from-event'."
  (let ((pair (meep--surround-pair-from-event)))
    (when pair
      (cond
       ((eq action 'delete)
        ;; NOTE: search only the read delimiter's type, so the key picks which
        ;; surround to strip; `meep-surround-delete-at-point' instead removes the
        ;; nearest pair of any type with no read.
        (meep--surround-operate count (list pair) nil line-wise))
       ((eq action 'region-activate)
        ;; Activate the region of the read delimiter's type only, the same by-type
        ;; choice the delete branch makes.  LINE-WISE is ignored - region-activate is
        ;; single-region, see `meep--surround-region-activate-impl'.
        (meep--surround-region-activate-impl count (list pair)))
       ((eq action 'replace)
        (meep--surround-replace-impl (car pair) (cdr pair) count line-wise))
       (t
        (meep--surround-add-impl (car pair) (cdr pair) count line-wise))))))

(defun meep--surround-event-command (action line-wise)
  "Return the named event command applying ACTION.
ACTION is `add', `replace', `delete', or `region-activate'.  LINE-WISE selects the
per-line variant, except for `region-activate' which is single-region and has none.
One named command per verb / line-wise pair, so `meep--surround-make-delimiter-map'
carries the verb in the binding it chooses; the command resolves its own invoking
key via `meep--surround-from-event'."
  (declare (important-return-value t))
  (cond
   ((eq action 'add)
    (cond
     (line-wise
      #'meep-surround-add-lines-event)
     (t
      #'meep-surround-add-event)))
   ((eq action 'replace)
    (cond
     (line-wise
      #'meep-surround-replace-lines-event)
     (t
      #'meep-surround-replace-event)))
   ((eq action 'region-activate)
    ;; Single-region, so no line-wise variant.
    #'meep-surround-region-activate-event)
   (t
    (cond
     (line-wise
      #'meep-surround-delete-lines-event)
     (t
      #'meep-surround-delete-event)))))

(defun meep--surround-each-alias (config fn)
  "Call FN with (KEY SYMBOL) for each `meep-surround-alist' alias CONFIG defines.
KEY is the alias character and SYMBOL its semantic name; only aliases whose symbol
the buffer's pairs (CONFIG) define are visited, in `meep-surround-alist' order.
`single-key-description' (not `char-to-string') spells KEY for `keymap-set' - the
kbd form (\"SPC\", \"TAB\"...) a raw character string is not.  Shared by the
run-time keymap builders so the alias scan lives in one place."
  (dolist (entry meep-surround-alist)
    (when (assq (cdr entry) config)
      (funcall fn (car entry) (cdr entry)))))

(defun meep--surround-keymap-add-fallbacks (km cmd)
  "Bind `RET' / `<return>' (arbitrary pair) and the `[t]' catch-all (literal) to CMD.
Each carries a `menu-item' label so `which-key' shows the intent rather than the
shared command name.  The command loop honours the `[t]' default when it
dispatches the key; `lookup-key' only consults it with ACCEPT-DEFAULT, which
dispatch does not need.  KM is the keymap to populate; shared by the run-time
delimiter keymap builders."
  (keymap-set km "RET" (list 'menu-item "arbitrary" cmd))
  (keymap-set km "<return>" (list 'menu-item "arbitrary" cmd))
  (define-key km [t] (list 'menu-item "literal" cmd)))

(defun meep--surround-make-delimiter-map (cmd)
  "Build a surround delimiter keymap routing every key to CMD, at run-time.
Each `meep-surround-alist' alias the buffer's `meep-surround-pairs' defines is
bound (carried as a `menu-item' labelled with its symbol, so `which-key' lists
`bold' / `italic' rather than the shared command name), `RET' / `<return>' for an
arbitrary pair, and a `[t]' catch-all for any other key as a literal delimiter.
CMD resolves the invoking key with `meep--surround-from-event' (the verb event
commands) or wraps it (the by-type replace).  Built fresh on each use so it always
reflects the current buffer's configuration; shared by the verb-first verbs (via
`meep--surround-read-delimiter-verb') and the by-type replace map."
  (declare (important-return-value t))
  (let ((km (make-sparse-keymap))
        (config (meep--surround-pairs-config)))
    ;; Alias keys, carried as menu-items so `which-key' lists the symbol name.
    (meep--surround-each-alias
     config
     (lambda (key sym)
       (keymap-set km (single-key-description key) (list 'menu-item (symbol-name sym) cmd))))
    (meep--surround-keymap-add-fallbacks km cmd)
    km))

(defun meep--surround-read-delimiter-verb (action line-wise prompt)
  "Install the run-time delimiter keymap transiently, echoing PROMPT.
ACTION and LINE-WISE select the verb the delimiter keys route to, see
`meep--surround-event-command'.  The verb commands (`meep-surround-add' etc.) call
this for both a key-bound and an `M-x' invocation: there is no prefix keymap to
traverse, so the delimiter map is installed as a transient map.  A numeric prefix
carries to the delimiter's event command via `prefix-command-preserve-state'."
  (prefix-command-preserve-state)
  (meep--surround-set-keymap
   (meep--surround-make-delimiter-map (meep--surround-event-command action line-wise)) prompt))

;;;###autoload
(defun meep-surround-add ()
  "Read a delimiter, then surround the region with it.
Reads the delimiter through a run-time keymap of the buffer's `meep-surround-pairs'
installed transiently, whether called by key or `M-x'.  A numeric prefix is the
repeat count; the target is the active or implied region, see
`meep--region-or-mark-bounds'."
  (interactive "*")
  (meep--surround-read-delimiter-verb 'add nil "Surround with"))

;;;###autoload
(defun meep-surround-add-lines ()
  "Read a delimiter, then surround each line's content with it.
The line-wise variant of `meep-surround-add'."
  (interactive "*")
  (meep--surround-read-delimiter-verb 'add t "Surround with"))

;;;###autoload
(defun meep-surround-add-event (arg)
  "Surround the region with the delimiter named by the invoking key, ARG times.
A direct shortcut for `meep-surround-add' followed by that delimiter: bind a
delimiter key to this and it wraps with that delimiter immediately, with no
separate delimiter read.  The delimiter is `last-command-event', resolved by
`meep--surround-from-event'; the target is the active or implied region, as for
`meep-surround-add'."
  (interactive "*p")
  (meep--surround-from-event arg nil 'add))

;;;###autoload
(defun meep-surround-add-lines-event (arg)
  "Surround each line's content with the delimiter named by the invoking key.
Wrap ARG times.  The line-wise variant of `meep-surround-add-event'."
  (interactive "*p")
  (meep--surround-from-event arg t 'add))

;;;###autoload
(defun meep-surround-replace-event (arg)
  "Replace the surrounding pair with the delimiter named by the invoking key.
Replace ARG times (the nesting depth).  The replace counterpart of
`meep-surround-add-event': the delimiter is `last-command-event', resolved by
`meep--surround-from-event'.  Routed to by the run-time delimiter keymap, and
bindable directly to a delimiter key."
  (interactive "*p")
  (meep--surround-from-event arg nil 'replace))

;;;###autoload
(defun meep-surround-replace-lines-event (arg)
  "Replace each line's surrounding pair with the delimiter named by the invoking key.
The line-wise variant of `meep-surround-replace-event'."
  (interactive "*p")
  (meep--surround-from-event arg t 'replace))

;;;###autoload
(defun meep-surround-delete-event (arg)
  "Delete the surrounding pair of the type named by the invoking key, ARG times.
The by-type delete counterpart of `meep-surround-add-event': the delimiter is
`last-command-event', resolved by `meep--surround-from-event', and names which
pair type to strip - a closer pair of another type is skipped.  To strip the
nearest pair of any type with no read, use `meep-surround-delete-at-point'."
  (interactive "*p")
  (meep--surround-from-event arg nil 'delete))

;;;###autoload
(defun meep-surround-delete-lines-event (arg)
  "Delete each line's surrounding pair of the type named by the invoking key.
The line-wise variant of `meep-surround-delete-event'."
  (interactive "*p")
  (meep--surround-from-event arg t 'delete))

;;;###autoload
(defun meep-surround-replace ()
  "Read a delimiter, then replace the surrounding pair to it.
A numeric prefix is the nesting depth.  Reads the delimiter through a run-time
keymap of the buffer's `meep-surround-pairs' installed transiently, whether called
by key or `M-x'."
  (interactive "*")
  (meep--surround-read-delimiter-verb 'replace nil "Replace surround to"))

;;;###autoload
(defun meep-surround-replace-lines ()
  "Read a delimiter, then replace each line's surrounding pair to it.
The line-wise variant of `meep-surround-replace'."
  (interactive "*")
  (meep--surround-read-delimiter-verb 'replace t "Replace surround to"))

;;;###autoload
(defun meep-surround-delete ()
  "Read a delimiter, then delete the surrounding pair of that type.
Reading a delimiter parallels `meep-surround-replace'; here it names which pair
type to strip, so only a surrounding pair of that type is removed - a closer pair
of another type is skipped, as in the by-type surround flow (`s t t').  A numeric
prefix takes the Nth enclosing pair of the type.  To strip the nearest pair of any
type with no prompt, use `meep-surround-delete-at-point'."
  (interactive "*")
  (meep--surround-read-delimiter-verb 'delete nil "Delete surround of"))

;;;###autoload
(defun meep-surround-delete-lines ()
  "Read a delimiter, then delete each line's surrounding pair of that type.
A numeric prefix strips that many nested layers per line.  The line-wise variant
of `meep-surround-delete'."
  (interactive "*")
  (meep--surround-read-delimiter-verb 'delete t "Delete surround of"))

;;;###autoload
(defun meep-surround-delete-at-point (arg)
  "Delete the delimiters surrounding the region or point, ARG times.
No delimiter is read - the surrounding pair is found contextually (innermost when
ARG is 1, the ARG-th enclosing pair otherwise).  To choose which delimiter type to
strip, use `meep-surround-delete'."
  (interactive "*p")
  (meep--surround-delete-impl arg nil))

;;;###autoload
(defun meep-surround-delete-lines-at-point (arg)
  "Delete each line's outermost surrounding delimiters, ARG times.
The line-wise variant of `meep-surround-delete-at-point'.
When a region is active, every line it spans is processed; lines without a
surrounding pair are skipped.  ARG peels that many nested layers per line."
  (interactive "*p")
  (meep--surround-delete-impl arg t))

;;;###autoload
(defun meep-surround-region-activate-event (arg)
  "Select the content of the surrounding pair of the type named by the invoking key.
The region-activate counterpart of `meep-surround-delete-event': the delimiter is
`last-command-event', resolved by `meep--surround-from-event', and names which pair
type to select - a closer pair of another type is skipped.  ARG is the nesting
depth.  To select the nearest pair of any type with no read, use
`meep-surround-region-activate-at-point'."
  (interactive "p")
  (meep--surround-from-event arg nil 'region-activate))

;;;###autoload
(defun meep-surround-region-activate ()
  "Read a delimiter, then activate the region inside the surrounding pair of that type.
Reading a delimiter parallels `meep-surround-delete'; here it names which pair type
to select, so the region is activated over the content of the nearest surrounding
pair of that type - a closer pair of another type is skipped.  A numeric prefix
takes the Nth enclosing pair.  To select the nearest pair of any type with no
prompt, use `meep-surround-region-activate-at-point'.  Never modifies the buffer."
  (interactive)
  (meep--surround-read-delimiter-verb 'region-activate nil "Activate region of"))

;;;###autoload
(defun meep-surround-region-activate-at-point (arg)
  "Activate the region inside the delimiters surrounding the region or point, ARG times.
No delimiter is read - the surrounding pair is found contextually (innermost when
ARG is 1, the ARG-th enclosing pair otherwise).  To choose which delimiter type to
select, use `meep-surround-region-activate'."
  (interactive "p")
  (meep--surround-region-activate-impl arg (meep--surround-recognition-pairs)))

(defun meep--surround-set-keymap (km prefix)
  "Activate KM as a transient map echoing PREFIX.
Used by the `M-x' fall-backs of the run-time keymaps, where there is no following
key in a real keymap for `which-key' to display."
  ;; Don't append `%k': in `set-transient-map' it expands to *every* key bound in
  ;; KM, which leaks the `[t]' catch-all (`meep--surround-make-delimiter-map') as
  ;; `<t>'.  So the whole message is the escaped PREFIX (no live `%k'), see
  ;; `meep--set-transient-map-echo'.
  (meep--set-transient-map-echo km "" prefix))

;; By-type surround is verb-first, like the at-point `t' / `T' prefixes, but bound
;; as plain verb commands placed directly (the default uses an `s t' / `s T' prefix):
;; `meep-surround-replace-by-type' replaces and `meep-surround-delete' deletes, each
;; then reading a source delimiter naming which pair type to act on (the nearest
;; enclosing pair of that type, skipping closer pairs of other types), where the
;; at-point verbs take the pair at point instead.
;;
;; Delete (`meep-surround-delete') reads one source delimiter and strips that type
;; immediately.  Replace needs a second delimiter, the destination, so each source
;; key opens a destination delimiter map (the verbs' own
;; `meep--surround-make-delimiter-map') routed to
;; `meep-surround-replace-by-type-event', a named command that recovers the source
;; from the invoking key sequence and the destination from `last-command-event'.
;; A source resolved only when typed - an arbitrary literal via `[t]', or
;; `RET' to prompt - cannot name itself in a later key sequence, so it defers to
;; `meep-surround-replace-by-type-pick', which resolves the source eagerly, stashes it
;; in `meep--surround-replace-by-type-picked-pair', and installs the destination step
;; transiently routed to the named `meep-surround-replace-by-type-picked-event' (the
;; source recovered from the stash rather than the keys).

(defvar meep--surround-replace-by-type-picked-pair nil
  "Source pair stashed by `meep-surround-replace-by-type-pick' for its destination step.
The pick path resolves the source eagerly (it may prompt), then installs a transient
destination map routed to `meep-surround-replace-by-type-picked-event', which reads
this variable rather than a captured closure (keeping it a named command).  Read once
and cleared by the destination command.")

(defun meep--surround-replace-by-type-apply (count line-wise pair dest-event)
  "Replace the surrounding pair of type PAIR with the destination DEST-EVENT names.
Operate COUNT times; LINE-WISE selects per-line operation.  The destination is
resolved from DEST-EVENT by `meep--surround-pair-from-event'.  It is passed
explicitly - not read from the live `last-command-event' - because a source
resolution that prompts via the minibuffer clobbers `last-command-event' with the
terminating `RET' (see `meep--surround-replace-by-type-from-events', which captures
the destination before resolving the source).  Message and do nothing when PAIR is
nil or the destination names no pair.  The shared tail of the two by-type replace
paths, `meep--surround-replace-by-type-from-events' and `-from-picked'."
  (declare (important-return-value t))
  (cond
   (pair
    (let ((target (meep--surround-pair-from-event dest-event)))
      (when target
        (meep--surround-operate count (list pair) target line-wise))))
   (t
    (message "No surround for that key"))))

(defun meep--surround-replace-by-type-from-picked (count line-wise)
  "Replace the surrounding pair stashed by the pick step, COUNT times.
The source pair was resolved eagerly by `meep-surround-replace-by-type-pick' and left
in `meep--surround-replace-by-type-picked-pair' (read once and cleared here); the
destination is `last-command-event'.  Reading the stashed pair from a variable, not a
captured closure, keeps this a named command.  LINE-WISE selects per-line operation."
  (declare (important-return-value t))
  ;; The source is already resolved (so no prompt clobbered `last-command-event' here,
  ;; unlike the keymap-traversed `meep--surround-replace-by-type-from-events'), so the
  ;; live event is already the destination.
  (let ((pair meep--surround-replace-by-type-picked-pair))
    (setq meep--surround-replace-by-type-picked-pair nil)
    (meep--surround-replace-by-type-apply count line-wise pair last-command-event)))

(defun meep-surround-replace-by-type-picked-event (arg)
  "Replace the picked by-type source pair with the destination named by the invoking key.
The destination step of `meep-surround-replace-by-type-pick', replacing ARG times.
Bound only in that command's transient map; reads the stashed source from
`meep--surround-replace-by-type-picked-pair'."
  (interactive "*p")
  (meep--surround-replace-by-type-from-picked arg nil))

(defun meep-surround-replace-by-type-picked-lines-event (arg)
  "Replace each line's picked by-type source pair with the destination key.
The line-wise variant of `meep-surround-replace-by-type-picked-event'."
  (interactive "*p")
  (meep--surround-replace-by-type-from-picked arg t))

(defun meep--surround-replace-by-type-from-events (count line-wise)
  "Replace the surrounding pair of one delimiter type with another, COUNT times.
Both delimiters come from the invoking key sequence: the source - the pair type to
act on - is the key before last in `this-command-keys-vector', the destination is
`last-command-event'.  Recovering the source from the key sequence (not a captured
closure) keeps this a named command.  LINE-WISE selects per-line operation."
  (declare (important-return-value t))
  (let* ((keys (this-command-keys-vector))
         ;; Capture the destination event before resolving the source: a
         ;; function-valued source alias spec prompts via the minibuffer, which
         ;; overwrites `last-command-event' with the terminating `RET', so reading
         ;; the destination after would re-prompt for an arbitrary pair instead.
         (dest-event last-command-event)
         ;; The source is the key before the destination in the sequence; a direct
         ;; `M-x' or single-key call has no preceding source key, so resolve no
         ;; source rather than index before the vector's start.  Guard that the
         ;; recovered event is a character: a function-key or mouse event in that
         ;; slot is not a delimiter, and `meep--surround-pair-from-key' assumes a
         ;; character, so a non-character resolves to no source and falls through to
         ;; the "No surround for that key" message rather than signalling.
         (src (and (length> keys 1) (aref keys (- (length keys) 2))))
         (pair (and (characterp src) (meep--surround-pair-from-key src))))
    (meep--surround-replace-by-type-apply count line-wise pair dest-event)))

;;;###autoload
(defun meep-surround-replace-by-type-event (arg)
  "Replace a surrounding pair of one type with another, both named by the key sequence.
The source - the pair type to act on - is the key pressed just before this one, the
destination is the invoking key; replace ARG times (the nesting depth).  Routed to
by the by-type replace source map (`meep--surround-replace-by-type-map'); the
keymap-traversed gesture stays a single named command."
  (interactive "*p")
  (meep--surround-replace-by-type-from-events arg nil))

;;;###autoload
(defun meep-surround-replace-by-type-lines-event (arg)
  "Replace each line's surrounding pair of one type with another, named by the key sequence.
The line-wise variant of `meep-surround-replace-by-type-event'."
  (interactive "*p")
  (meep--surround-replace-by-type-from-events arg t))

(defun meep--surround-replace-by-type-map (line-wise)
  "Build the source-delimiter keymap for by-type replace at run-time.
Each source whose key is known up-front - an alias the buffer defines, or either
side of a `meep-symmetrical-chars' pair - opens the one destination delimiter map
\(`meep--surround-make-delimiter-map') routed to the named by-type replace event
command, which recovers the source from the invoking key sequence.  `RET' and the
`[t]' catch-all defer to `meep-surround-replace-by-type-pick' for a source resolved
only when typed.  LINE-WISE selects per-line operation."
  (declare (important-return-value t))
  (let* ((km (make-sparse-keymap))
         (config (meep--surround-pairs-config))
         (cmd
          (cond
           (line-wise
            #'meep-surround-replace-by-type-lines-event)
           (t
            #'meep-surround-replace-by-type-event)))
         (pick
          (cond
           (line-wise
            #'meep-surround-replace-by-type-lines-pick)
           (t
            #'meep-surround-replace-by-type-pick)))
         ;; One destination map shared by every source: the routed event command
         ;; recovers the source from the invoking key sequence, not the submap, so
         ;; the map need not be rebuilt (nor re-rendered by `which-key') per source.
         ;; Each source binds it behind a `menu-item' carrying that source's label,
         ;; so `which-key' lists the source keys by name though they share the map.
         (dest-map (meep--surround-make-delimiter-map cmd)))
    ;; Symmetrical delimiters first; an alias (bound next) wins any key collision.
    (dolist (entry meep-symmetrical-chars)
      (dolist (side (list (car entry) (cdr entry)))
        ;; Single-key sources only: a multi-character `meep-symmetrical-chars' side
        ;; cannot be typed as one source key, so skip it rather than bind a bogus
        ;; source from its first character.
        (when (length= side 1)
          (let ((ch (string-to-char side)))
            (keymap-set km (single-key-description ch) (list 'menu-item side dest-map))))))
    (meep--surround-each-alias
     config
     (lambda (key sym)
       (keymap-set km (single-key-description key) (list 'menu-item (symbol-name sym) dest-map))))
    (meep--surround-keymap-add-fallbacks km pick)
    km))

(defun meep--surround-replace-by-type-pick-impl (line-wise)
  "Resolve the invoking source delimiter, then read a destination and replace it.
The `RET' / literal fall-back of `meep--surround-replace-by-type-map': a source
resolved only when typed cannot pre-build a `which-key'-native destination map, so
this resolves the source pair, stashes it in
`meep--surround-replace-by-type-picked-pair', and installs the destination map
\(routed to `meep-surround-replace-by-type-picked-event') as a transient map.
The source is resolved by `meep--surround-pair-from-event'; LINE-WISE selects
per-line operation."
  (prefix-command-preserve-state)
  ;; Resolve the source pair now, before the destination is read.  The `RET' /
  ;; arbitrary-pair path prompts via the minibuffer here, so resolving eagerly keeps
  ;; the result.  Stash it for the named destination command rather than capture it in
  ;; a closure, so the gesture stays a named command.
  (let ((pair (meep--surround-pair-from-event)))
    (when pair
      (setq meep--surround-replace-by-type-picked-pair pair)
      (meep--surround-set-keymap
       (meep--surround-make-delimiter-map
        (cond
         (line-wise
          #'meep-surround-replace-by-type-picked-lines-event)
         (t
          #'meep-surround-replace-by-type-picked-event)))
       "Replace surround to"))))

;;;###autoload
(defun meep-surround-replace-by-type-pick (_arg)
  "Pick a typed source delimiter, then read a destination and replace that pair.
The region / point variant; see `meep--surround-replace-by-type-pick-impl'."
  (interactive "*P")
  (meep--surround-replace-by-type-pick-impl nil))

;;;###autoload
(defun meep-surround-replace-by-type-lines-pick (_arg)
  "Pick a typed source delimiter, then replace each line's surrounding pair of that type.
The line-wise variant of `meep-surround-replace-by-type-pick'."
  (interactive "*P")
  (meep--surround-replace-by-type-pick-impl t))

;;;###autoload
(defun meep-surround-replace-by-type ()
  "Read a source delimiter, then a destination, and replace that surrounding pair.
The source names which pair type to replace - the nearest enclosing pair of that
delimiter, skipping closer pairs of other types - and the destination is the new
pair.  A numeric prefix is the nesting depth.  Installs the source delimiter map
transiently, whether called by key (the default binds `s t g' / `s T g') or `M-x'."
  (interactive "*")
  (prefix-command-preserve-state)
  (meep--surround-set-keymap (meep--surround-replace-by-type-map nil) "Replace surround of"))

;;;###autoload
(defun meep-surround-replace-by-type-lines ()
  "Read a source delimiter and destination, replacing each line's surrounding pair of that type.
The line-wise variant of `meep-surround-replace-by-type'."
  (interactive "*")
  (prefix-command-preserve-state)
  (meep--surround-set-keymap (meep--surround-replace-by-type-map t) "Replace surround of"))


;; ---------------------------------------------------------------------------
;; Text Editing: Join Lines
;;
;; Line joining with support for left-trimming code-comments,
;; so this may be used to conveniently join lines in code.
;;
;; For an example of languages using ``#`` prefixed comments (Python or Shell):
;;
;; .. code-block:: python
;;
;;    # Example block.
;;    # Next line.
;;
;; Joined at the first line removes the leading ``#``:
;;
;; .. code-block:: python
;;
;;    # Example block. Next line.
;;
;; And for C-family languages:
;;
;; .. code-block:: c
;;
;;    /* Example block.
;;     * next line. */
;;
;; Joined at the first line removes the leading ``*``:
;;
;; .. code-block:: c
;;
;;    /* Example block. next line. */

(defun meep--join-maybe-skip-comment-prefix (limit)
  "Skip forward over comment chars and any following blank-space.
Don't skip past LIMIT (the end of the current line)."
  (let ((state (syntax-ppss))
        (comment-start-quote (regexp-quote comment-start))
        (ok t)
        (point-init (point)))

    ;; We may be at the comment start, which isn't considered a comment.
    (unless (nth 4 state)
      (save-excursion
        (skip-syntax-forward "^-")
        (let ((state-test (syntax-ppss)))
          (when (nth 4 state-test)
            (when-let* ((beg-test (nth 8 state-test)))
              (when (<= point-init beg-test)
                (setq state state-test)))))))

    (when (nth 4 state) ; Comment.
      (when (and ok (<= (point) limit) (looking-at-p comment-start-quote))
        (let ((pos-next (+ (point) (length comment-start))))
          (cond
           ((<= pos-next limit)
            (goto-char pos-next))
           (t
            (setq ok nil)))))))
  (when (< (point) limit)
    (skip-chars-forward "[:blank:]" limit)

    ;; Collapse block comments for C style languages.
    (when (and (bound-and-true-p c-buffer-is-cc-mode)
               ;; Less trouble than checking all the derived modes.
               (bound-and-true-p c-block-comment-prefix))
      ;; Blank space has already been skipped, so trim it here.
      ;; The alternative could be to step backwards,
      ;; either way it's not an important difference.
      (let* ((block-prefix (symbol-value 'c-block-comment-prefix))
             (block-trim (string-trim-right block-prefix)))
        (cond
         ((looking-at-p (regexp-quote block-prefix))
          (let ((pos-next (+ (point) (length block-trim))))
            (when (<= pos-next limit)
              (goto-char pos-next)
              (when (< (point) limit)
                (skip-chars-forward "[:blank:]" limit)))))
         ((looking-at-p (concat (regexp-quote block-trim) "\n"))
          (let ((pos-next (+ (point) (length block-trim))))
            (when (<= pos-next limit)
              (goto-char pos-next)
              (when (< (point) limit)
                (skip-chars-forward "[:blank:]" limit))))))))

    (when comment-start-skip
      (let ((pos-prev (point)))
        (save-match-data
          (when (save-excursion (re-search-forward comment-start-skip limit t))
            ;; Only skip when the match is at point
            ;; (blank-space was already skipped above).
            ;; Without this, an inline comment such as "C D # E"
            ;; would match the "# " before "E",
            ;; causing the content before it to be lost.
            (when (eq pos-prev (match-beginning 0))
              (goto-char (match-end 0))
              (skip-chars-forward "[:blank:]" limit))))))))

(defun meep--join-range-is-eol-comment (eol-ws bol-ws-next)
  "Detect if EOL-WS and BOL-WS-NEXT are comments that can be joined."
  (let ((result nil)
        ;; Get the range for the comment at point or nil.
        (comment-range-fn
         (lambda (pos)
           (let ((range nil))
             (save-excursion
               (when-let* ((state (syntax-ppss pos)))
                 (when (nth 4 state)
                   (when-let* ((beg (nth 8 state)))
                     (goto-char beg)
                     (when (forward-comment 1)
                       (setq range (cons beg (point))))))))
             range))))

    (when-let* ((range-a (funcall comment-range-fn eol-ws)))
      (cond
       ;; A multi-line comment which ends after the beginning of the next line.
       ;; No need to check further.
       ((< bol-ws-next (cdr range-a))
        (setq result t))
       ;; Detect two single line comments,
       ;; we only need to check if the first non blank-space is a:
       ;; - comment start 11.
       ;; - comment generic 14.
       ((memq (car (syntax-after bol-ws-next)) (list 11 14))
        (setq result t))))
    result))

(defun meep--join-range (bol eol use-comment-strip)
  "Join newlines between BOL and EOL.
When USE-COMMENT-STRIP is non-nil, strip comments."
  (cond
   ;; Detect end of buffer.
   ((eq eol (point-max))
    nil)
   (t
    (save-excursion
      (goto-char eol)
      (let* ((eol-ws (- eol (- (skip-chars-backward "[:blank:]" bol))))
             (bol-ws-next
              (save-excursion
                (goto-char (1+ eol))
                (let ((eol-next (pos-eol)))
                  (skip-chars-forward "[:blank:]" eol-next)
                  (when (and use-comment-strip (meep--join-range-is-eol-comment eol-ws (point)))
                    (meep--join-maybe-skip-comment-prefix eol-next)))
                (point))))

        (cons eol-ws bol-ws-next))))))

(defun meep--join-delete-region-maybe-space (beg end add-space)
  "Join newlines in the region from BEG to END.
When ADD-SPACE is true, ensure a space separator."
  (let ((result 0))
    (unless (eq beg end)
      ;; Contract the region and don't add the space where possible.
      (when add-space
        (cond
         ((eq ?\s (char-after beg))
          (meep--incf beg)
          (setq add-space nil))
         ((eq ?\s (char-before end))
          (meep--decf end)
          (setq add-space nil))))

      (cond
       (add-space
        (meep--replace-in-region " " beg end)
        (setq result (- end beg 1)))
       (t
        (delete-region beg end)
        (setq result (- end beg)))))
    result))

(defun meep--join-region-impl (beg end use-comment-strip)
  "Join region between BEG and END.
USE-COMMENT-STRIP, strips comments between lines."
  (let ((changed nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((eol (pos-eol)))
          ;;
          (let ((range (meep--join-range beg eol use-comment-strip)))
            (cond
             (range
              (let ((add-space (/= beg (car range))))
                (let ((ofs
                       (meep--join-delete-region-maybe-space (car range) (cdr range) add-space)))
                  (setq eol (pos-eol))
                  (goto-char eol)
                  (meep--decf end ofs)
                  (setq changed t))))
             (t
              (goto-char end)))))))
    changed))

;;;###autoload
(defun meep-join-line-next (arg)
  "Join the next line to this one ARG times."
  (interactive "*p")
  ;; TODO: make optional.
  (let ((use-comment-strip t)
        (changed nil))
    (cond
     ((zerop arg)) ; NOP, unlikely, include for correctness.
     ((region-active-p)
      (when (meep--join-region-impl (region-beginning) (region-end) use-comment-strip)
        (setq changed t)))
     ((< arg 0)
      (meep-join-line-prev (- arg)))
     (t
      (let ((bol (pos-bol))
            (eol nil)
            (range nil))
        (while (and (null
                     (zerop
                      (prog1 arg
                        (meep--decf arg))))
                    (save-excursion
                      (setq eol (pos-eol))
                      (cond
                       ((/= eol (point-max))
                        ;; No need to update `bol' as it doesn't change.
                        (setq range (meep--join-range bol eol use-comment-strip)))
                       (t
                        nil))))
          (let ((add-space (/= bol (car range))))
            (meep--join-delete-region-maybe-space (car range) (cdr range) add-space)
            (let ((pos-new (car range)))
              (when add-space
                (meep--incf pos-new))
              (goto-char pos-new)
              (setq changed t)))))))

    (unless changed
      (message "Join line end: no following line found"))
    changed))

;;;###autoload
(defun meep-join-line-prev (arg)
  "Join the previous line to this one ARG times."
  (interactive "*p")
  ;; TODO: make optional.
  (let ((use-comment-strip t)
        (changed nil))
    (cond
     ((zerop arg)) ; NOP, unlikely, include for correctness.
     ((region-active-p)
      (when (meep--join-region-impl (region-beginning) (region-end) use-comment-strip)
        (setq changed t)))
     ((< arg 0)
      (meep-join-line-next (- arg)))
     (t
      (let ((bol nil)
            (eol nil)
            (range nil))
        (while (and (null
                     (zerop
                      (prog1 arg
                        (meep--decf arg))))
                    (save-excursion

                      (cond
                       ((zerop (forward-line -1))
                        (setq bol (pos-bol))
                        (setq eol (pos-eol))
                        (setq range (meep--join-range bol eol use-comment-strip)))
                       (t
                        nil))))
          (let ((add-space (/= bol (car range))))
            (meep--join-delete-region-maybe-space (car range) (cdr range) add-space)
            (let ((pos-new (car range)))
              (when add-space
                (meep--incf pos-new))
              (goto-char pos-new)
              (setq changed t)))))))

    (unless changed
      (message "Join line beginning: no preceding line found"))
    changed))


;; ---------------------------------------------------------------------------
;; Text Editing: Shrink Space

;;;###autoload
(defun meep-space-shrink-contextual ()
  "Remove blank space contextually.
- When on a blank line, remove surrounding blank lines.
- When on a blank character remove multiple blank characters.
- Otherwise, when over a paragraph, trim the bounds to a single blank line.

Return non-nil when a change was made."
  (interactive "*")
  (let* ((bol (pos-bol))
         (eol (pos-eol))
         (line-bounds
          (cons
           (save-excursion
             (skip-chars-backward "[:blank:]" bol)
             (point))
           (save-excursion
             (skip-chars-forward "[:blank:]" eol)
             (point)))))
    (cond
     ;; An empty line.
     ((and (eq bol (car line-bounds)) (eq eol (cdr line-bounds)))
      ;; Find the buffer bounds.
      (let* ((vert-bounds
              (cons
               (save-excursion
                 (skip-chars-backward "[:blank:]\n" (point-min))
                 (beginning-of-line)
                 (unless (looking-at-p "[[:blank:]]*$")
                   (forward-line 1))
                 (point))
               (save-excursion
                 (skip-chars-forward "[:blank:]\n" (point-max))
                 (beginning-of-line)
                 (unless (looking-at-p "[[:blank:]]*$")
                   (forward-line -1)
                   (end-of-line))
                 (point)))))
        (cond
         ((< (car vert-bounds) (cdr vert-bounds))
          (meep--replace-in-region "" (car vert-bounds) (cdr vert-bounds))
          t)
         (t
          nil))))

     ;; No space or single space.
     ((or (eq (cdr line-bounds) (car line-bounds))
          (and (eq 1 (- (cdr line-bounds) (car line-bounds)))
               (eq ?\s (char-after (car line-bounds)))))
      (let ((changed-prev nil)
            (changed-next nil))
        (save-match-data
          (save-excursion
            (beginning-of-line)
            (when (search-backward-regexp "^[[:blank:]]*$" (point-min) t)
              (setq changed-prev (meep-space-shrink-contextual))))
          (save-excursion
            (end-of-line)
            (when (search-forward-regexp "^[[:blank:]]*$" (point-max) t)
              (setq changed-next (meep-space-shrink-contextual)))))
        (or changed-prev changed-next)))

     ;; Some space.
     ((< 0 (- (cdr line-bounds) (car line-bounds)))
      (meep--replace-in-region " " (car line-bounds) (cdr line-bounds))
      t)
     (t
      nil))))


;; ---------------------------------------------------------------------------
;; Text Editing: Transpose


;; Special handling is only needed because character motions do *not* set the mark.
;; This function adjusts the motion to behave as if it did.
(defun meep--transpose-with-adjusted-motion-char-wise (last-motion-info body-fn)
  "Wrapper for char-wise transpose using LAST-MOTION-INFO and BODY-FN."
  (let* ((orig-cmd (cadr last-motion-info))
         (mark-pos (car last-motion-info))
         (dir
          (cond
           ((> mark-pos (point))
            -1)
           (t
            1)))
         ;; For backward motion, flip positions and use char-next.
         (adjusted-info
          (cond
           ((< dir 0)
            (cons (point) (cons 'meep-move-char-next (cddr last-motion-info))))
           (t
            last-motion-info)))
         (result nil))
    ;; For backward motion, move to mark position before executing body.
    (when (< dir 0)
      (goto-char mark-pos))
    ;; Execute body with adjusted positions.
    (setq result (funcall body-fn adjusted-info))
    (cond
     ((eq result t)
      ;; Adjust cursor to be ON the swapped character.
      ;; Forward: move back 1 to be on the char that moved forward.
      ;; Backward: move back 2 to be on the char that moved backward.
      (backward-char
       (cond
        ((< dir 0)
         2)
        (t
         1)))
      (cons orig-cmd (point)))
     (t
      result))))

;; Special handling is only needed because we want transpose from line motion
;; to behave as though both the point and the mark are positioned
;; at the line beginning - even if they are not.
;;
;; NOTE: this statement is not entirely accurate,
;; there is a minor difference, when transposing to the next line,
;; without any special handling - the (point) would end up on the line *after*
;; the line that was transposed. While technically correct in some sense
;; (given the way transpose relies on wrapping motion in a generic way),
;; it is also not so useful if the point is *not* on the line we are moving
;; since we may want to operate on that line afterwards (indent it or so).
(defun meep--transpose-with-adjusted-motion-line-wise (last-motion-info body-fn)
  "Wrapper for line-wise transpose using LAST-MOTION-INFO and BODY-FN."
  (let* ((orig-cmd (cadr last-motion-info))
         (mark-pos (car last-motion-info))
         (mark-col-bol
          (save-excursion
            (goto-char mark-pos)
            (cons (current-column) (pos-bol))))
         (mark-col (car mark-col-bol))
         (mark-bol (cdr mark-col-bol))
         (point-bol (pos-bol))
         (dir
          (cond
           ((> mark-bol point-bol)
            -1)
           (t
            1)))
         ;; The first line's bol stays valid after swap (earlier position).
         (first-bol (min mark-bol point-bol))
         ;; The second line's bol (later position) is where body executes.
         (second-bol (max mark-bol point-bol))
         ;; Check if last line has no trailing newline (needed for swap fix).
         (no-trailing-newline (not (eq (char-before (point-max)) ?\n)))
         ;; Create adjusted-info for the body.
         (adjusted-info
          (cond
           ((< dir 0)
            ;; Backward: flipped positions and line-next command.
            (cons first-bol (cons 'meep-move-line-next (cddr last-motion-info))))
           (t
            ;; Forward: mark at bol, original command.
            (cons first-bol (cdr last-motion-info)))))
         (result nil))
    ;; Move point to the second line for body execution.
    (goto-char second-bol)
    ;; If buffer lacks trailing newline, add one temporarily so both lines
    ;; have consistent newline endings during swap.
    (when no-trailing-newline
      (save-excursion
        (goto-char (point-max))
        (insert "\n")))
    ;; Execute body with adjusted positions.
    ;; Clear goal-column so line motions go to column 0.
    (let ((goal-column 0))
      (setq result (funcall body-fn adjusted-info)))
    ;; Restore cursor position using relative navigation from first-bol.
    ;; After swap, first-bol is still valid (earlier line's bol doesn't shift).
    (cond
     ((eq result t)
      ;; Remove temporary trailing newline if we added one.
      (when no-trailing-newline
        (meep--assert (eq (char-before (point-max)) ?\n))
        (delete-region (1- (point-max)) (point-max)))
      (goto-char first-bol)
      (cond
       ((< dir 0)
        ;; Backward: point at first line (point's original), mark at second line.
        (meep--set-marker
         (save-excursion
           (forward-line 1)
           (point)))
        (move-to-column mark-col))
       (t
        ;; Forward: point at second line, mark at end of first line.
        (forward-line 1)
        (move-to-column mark-col)
        (meep--set-marker
         (save-excursion
           (goto-char first-bol)
           (pos-eol)))))
      ;; Return repeat info: (PRE-MOTION . REPEAT-MARK-POS).
      (cons orig-cmd (point)))
     (t
      ;; Remove temporary trailing newline if we added one.
      (when no-trailing-newline
        (meep--assert (eq (char-before (point-max)) ?\n))
        (delete-region (1- (point-max)) (point-max)))
      result))))

(defmacro meep--transpose-with-adjusted-motion (last-motion-info-sym &rest body)
  "Execute BODY with LAST-MOTION-INFO-SYM adjusted for line/char motions.
Dispatches to motion-specific wrappers that normalize direction and cursor."
  (declare (indent 1))
  (let ((body-fn (make-symbol "body-fn"))
        (result (make-symbol "result")))
    `(let ((,body-fn (lambda (,last-motion-info-sym) ,@body)))
       (cond
        ((memq (cadr ,last-motion-info-sym) '(meep-move-line-next meep-move-line-prev))
         (meep--transpose-with-adjusted-motion-line-wise ,last-motion-info-sym ,body-fn))
        ((memq (cadr ,last-motion-info-sym) '(meep-move-char-next meep-move-char-prev))
         (meep--transpose-with-adjusted-motion-char-wise ,last-motion-info-sym ,body-fn))
        (t
         ;; Other motions: execute body unchanged, wrap success in repeat info.
         (let ((,result (funcall ,body-fn ,last-motion-info-sym)))
           (cond
            ((eq ,result t)
             ;; Return repeat info: (nil . REPEAT-MARK-POS).
             ;; No pre-motion needed, use current mark as repeat position.
             (cons nil (mark)))
            (t
             ,result))))))))

(defun meep--transpose-any-motion (last-motion-info)
  "Transpose based on any motion using LAST-MOTION-INFO.
Returns t if transpose succeeded, \\='transpose-abort if at boundary, nil if not applicable."
  (let* ((range-a (cons (car last-motion-info) (point)))
         (range-b (cons nil nil))
         (local-last-command (car (cdr last-motion-info)))
         (local-last-prefix-arg (cdr (cdr last-motion-info)))
         (mark-dir
          (cond
           ((< (car last-motion-info) (point))
            1)
           (t
            -1)))
         (last-dir
          (cond
           ((and (integerp local-last-prefix-arg) (< local-last-prefix-arg 0))
            -1)
           (t
            1)))
         (dir (* mark-dir last-dir))
         (abort nil))

    ;; Execute motion to find the end of the next element.
    ;; Catch buffer boundary errors (e.g., from char motion).
    (condition-case nil
        (let ((current-prefix-arg last-dir))
          (call-interactively local-last-command))
      ((beginning-of-buffer end-of-buffer)
       (message "Transpose: no element to swap with")
       (goto-char (cdr range-a))
       (setq abort t)))
    (setcdr range-b (point))

    (cond
     (abort
      'transpose-abort)
     ;; Check if motion failed to advance (nothing to swap with).
     ;; This happens at buffer limits where there's no next element.
     ((and (eq dir 1) (<= (point) (cdr range-a)))
      ;; Motion didn't advance past original position - nothing to swap with.
      (message "Transpose: no element to swap with")
      (goto-char (cdr range-a))
      'transpose-abort)
     ((and (eq dir -1) (>= (point) (cdr range-a)))
      ;; Motion didn't retreat past original position - nothing to swap with.
      (message "Transpose: no element to swap with")
      (goto-char (cdr range-a))
      'transpose-abort)
     (t
      ;; Execute reverse motion to find the start of the next element.
      (let ((current-prefix-arg (- last-dir)))
        (call-interactively local-last-command))

      (cond
       ((eq dir 1)
        ;; Unlikely but it's not impossible for reversing to go back too far.
        ;; So clamp it by the previous bounds.
        (setcar range-b (max (point) (cdr range-a)))

        (let ((str-a (buffer-substring-no-properties (car range-a) (cdr range-a)))
              (str-b (buffer-substring-no-properties (car range-b) (cdr range-b))))
          (meep--replace-in-region str-a (car range-b) (cdr range-b))
          (meep--replace-in-region str-b (car range-a) (cdr range-a)))

        (goto-char (cdr range-b))
        (setq deactivate-mark nil)
        (meep--set-marker (- (point) (- (cdr range-a) (car range-a))))
        t)

       (t
        ;; Unlikely but it's not impossible for reversing to go back too far.
        ;; So clamp it by the previous bounds.
        (setcar range-b (min (point) (cdr range-a)))

        (setq range-a (cons (cdr range-a) (car range-a)))
        (setq range-b (cons (cdr range-b) (car range-b)))

        (let ((str-a (buffer-substring-no-properties (car range-a) (cdr range-a)))
              (str-b (buffer-substring-no-properties (car range-b) (cdr range-b))))
          (meep--replace-in-region str-b (car range-a) (cdr range-a))
          (meep--replace-in-region str-a (car range-b) (cdr range-b)))

        (goto-char (car range-b))
        (setq deactivate-mark nil)
        (meep--set-marker (+ (point) (- (cdr range-a) (car range-a))))
        t))))))

(defvar-local meep--transpose-repeat-motion-info nil
  "Synthetic motion info for transpose repeat.
Structure: (PRE-MOTION . MOTION-INFO) where PRE-MOTION is a command
to call before the next transpose (or nil), and MOTION-INFO is
\(MARK-POS . (COMMAND . PREFIX-ARG)).")

(defun meep--transpose-repeat-store (pre-motion mark-pos last-motion-info)
  "Store repeat info for transpose.
PRE-MOTION is a command to call before repeat (or nil).
MARK-POS is the mark position to use (or nil to keep original).
LAST-MOTION-INFO is the original motion info."
  (let ((cmd (car (cdr last-motion-info)))
        (prefix-arg (cdr (cdr last-motion-info))))
    (setq meep--transpose-repeat-motion-info
          (cons pre-motion (cons (or mark-pos (car last-motion-info)) (cons cmd prefix-arg))))))

(defun meep--transpose-repeat-clear ()
  "Clear the transpose repeat state."
  (setq meep--transpose-repeat-motion-info nil))

(defun meep--transpose-repeat-get-motion-info ()
  "Get motion info from repeat state, executing pre-motion if needed.
Returns motion-info on success, \\='transpose-abort if pre-motion failed."
  (let ((pre-motion (car meep--transpose-repeat-motion-info))
        (motion-info (cdr meep--transpose-repeat-motion-info)))
    (cond
     (pre-motion
      ;; Execute pre-motion to set up cursor and mark.
      (let ((pre-result (meep--transpose-execute-pre-motion pre-motion)))
        (cond
         ((eq pre-result 'transpose-abort)
          'transpose-abort)
         (t
          motion-info))))
     (t
      motion-info))))

(defun meep--transpose-execute-pre-motion (pre-motion)
  "Execute PRE-MOTION before transpose repeat.
Returns t on success, \\='transpose-abort if at buffer boundary.
On abort, point and mark are restored to their original positions."
  (let ((point-before (point))
        (mark-before (mark)))
    (meep--set-marker (point))
    (condition-case nil
        (progn
          (deactivate-mark)
          ;; Prevent motion from overwriting the mark we just set.
          ;; Clear prefix arg so it doesn't leak to the motion.
          (let ((meep-mark-set-on-motion-override t)
                (current-prefix-arg nil))
            (call-interactively pre-motion))
          (cond
           ;; Point moved: success.
           ((/= (point) point-before)
            t)
           ;; Point didn't move: motion failed.
           (t
            (meep--set-marker mark-before)
            (message "Transpose: no element to swap with")
            'transpose-abort)))
      ((beginning-of-buffer end-of-buffer)
       (goto-char point-before)
       (meep--set-marker mark-before)
       (message "Transpose: no element to swap with")
       'transpose-abort))))

;;;###autoload
(defun meep-transpose (arg)
  "Transpose the previous motion ARG times.
This can be used to transpose words if the previous motion was over words.
Transposing lines and characters is also supported."
  (interactive "*p")
  (let ((count (max 1 (or arg 1)))
        (i 0)
        (done nil)
        (changed nil))
    (while (and (< i count) (not done))
      ;; Use save-mark-and-excursion to restore on failure.
      ;; On success, capture final positions and apply after macro exits.
      (let ((success-point nil)
            (success-mark nil))
        (save-mark-and-excursion
          (let ((last-motion-info
                 (cond
                  ;; When repeating transpose (via prefix arg or command repeat),
                  ;; use synthetic motion info.
                  ((and (or (> i 0) (eq last-command 'meep-transpose))
                        meep--transpose-repeat-motion-info)
                   (meep--transpose-repeat-get-motion-info))
                  (t
                   (meep--last-motion-calc-whole-mark-pos t)))))
            (cond
             ((eq last-motion-info 'transpose-abort)
              (setq done t))
             ((null last-motion-info)
              (message "Transpose could not find a last-motion")
              (meep--transpose-repeat-clear)
              (setq done t))
             (t
              ;; Transpose using the generic path.
              ;; The macro adjusts positions for line-wise and char-wise motions,
              ;; and returns (PRE-MOTION . MARK-POS) on success.
              (let ((result
                     (let ((r
                            (meep--transpose-with-adjusted-motion last-motion-info
                              (meep--transpose-any-motion last-motion-info))))
                       (cond
                        ((eq r 'transpose-abort)
                         r)
                        ((consp r)
                         (meep--transpose-repeat-store (car r) (cdr r) last-motion-info)
                         t)))))
                (cond
                 ((eq result 'transpose-abort)
                  ;; save-mark-and-excursion will restore positions.
                  (setq done t))
                 ((null result)
                  (message "Transpose not supported for this motion")
                  (meep--transpose-repeat-clear)
                  (setq done t))
                 (t
                  ;; Success: capture positions before macro restores them.
                  (setq success-point (point))
                  (setq success-mark (mark))
                  (setq changed t)
                  (meep--incf i))))))))
        ;; Apply success positions after save-mark-and-excursion restores.
        (when success-point
          (goto-char success-point)
          (meep--set-marker success-mark))))
    changed))


;; ---------------------------------------------------------------------------
;; Text Editing: Tab Wrapper

;;;###autoload
(defun meep-indent-rigidly ()
  "Indent the active region or the current line.

You may wish to bind this TAB, so pressing TAB twice re-indents."
  (interactive "*")
  ;; A wrapper for `indent-rigidly' which uses the current line when there is no active region.
  (unless (region-active-p)
    ;; Ensure the mark is never the same as point
    ;; (as this causes an error).
    (let ((bol (pos-bol))
          (eol (pos-eol)))
      (cond
       ((eq bol eol)
        (user-error "The line is empty"))
       ((eq (point) bol)
        (meep--set-marker eol))
       (t
        (meep--set-marker bol))))
    (setq deactivate-mark t))
  (call-interactively #'indent-rigidly))


;; ---------------------------------------------------------------------------
;; State: Insert

(defun meep--insert-impl ()
  "Enter insert mode."
  (when (region-active-p)
    (deactivate-mark t))
  (meep--set-marker (point))
  (bray-state-stack-push meep-state-insert))

;;;###autoload
(defun meep-insert ()
  "Enter insert mode."
  (interactive)
  (meep--insert-impl))

;; NOTE: this isn't used by the default key-map
;; but is supported by other popular modal editing systems.
;; Insert after the cursor OR on the opposite end of the region.
;;;###autoload
(defun meep-insert-append ()
  "Enter insert mode after the cursor, or at the opposite end of the region."
  (interactive)
  (cond
   ((region-active-p)
    (goto-char (mark)))
   (t
    (unless (eobp)
      (forward-char 1))))
  (meep--insert-impl))

;;;###autoload
(defun meep-insert-at-last ()
  "Enter insert mode at the position it was last exited."
  (interactive)
  (let ((pos-last-insert (meep--register-position-or-message meep-state-insert-register)))
    (cond
     (pos-last-insert
      (goto-char pos-last-insert)
      (meep--insert-impl))
     (t
      nil))))

(defun meep--insert-overwrite-disable-on-exit ()
  "Disable overwrite mode when leaving INSERT state.
Intended to be called by a hook."
  (overwrite-mode -1)
  ;; Don't check if it's valid since this hook can only be installed if the symbol is found.
  (let ((hook-sym (bray-state-get-hook-exit meep-state-insert)))
    (remove-hook hook-sym #'meep--insert-overwrite-disable-on-exit t)))

;;;###autoload
(defun meep-insert-overwrite ()
  "Enter insert mode and enable `overwrite-mode' while inserting."
  (interactive)
  (meep-insert)
  (let ((hook-sym (bray-state-get-hook-exit meep-state-insert)))
    (cond
     (hook-sym
      (overwrite-mode 1)
      (add-hook hook-sym #'meep--insert-overwrite-disable-on-exit 0 t))
     (t
      (message "No exit hook found for state: %S" meep-state-insert)))))


;; ---------------------------------------------------------------------------
;; Rectangle Edit with Repeat-FU Replay

;; Plist storing rectangle replay state, made buffer-local on demand, killed on exit.
;; - :column
;;   Target column for replay (the left edge of the rectangle).
;; - :line-direction
;;   +1 (top-to-bottom) or -1 (bottom-to-top) replay order.
;; - :line-marker
;;   Marker at the first replay target line, set during enter so it tracks
;;   through any buffer modifications the user makes in insert mode.
;;   Advanced to successive lines during replay.
;; - :undo-tail
;;   Tail of `buffer-undo-list' before the operation, for undo fusing.
;; - :mark-marker
;;   Marker at the rectangle boundary, used to detect the last replay line
;;   and to restore the mark on exit.
;; - :listener
;;   Opaque token from `repeat-fu-listener-register' for recording
;;   keystrokes during insert mode, replayed on remaining lines on exit.
(defvar meep--rectangle-edit-data nil)

(defun meep--rectangle-edit-undo-fuse (undo-tail)
  "Remove all undo boundaries between the head of `buffer-undo-list' and UNDO-TAIL.
This fuses multiple undo steps into a single atomic undo operation."
  ;; Strip leading nil boundaries.
  (while (and (consp buffer-undo-list)
              (not (eq buffer-undo-list undo-tail))
              (null (car buffer-undo-list)))
    (setq buffer-undo-list (cdr buffer-undo-list)))
  ;; Strip internal nil boundaries, stopping before UNDO-TAIL.
  (let ((tail buffer-undo-list))
    (while (and (consp tail) (consp (cdr tail)) (not (eq (cdr tail) undo-tail)))
      (cond
       ((null (cadr tail))
        (setcdr tail (cddr tail)))
       (t
        (setq tail (cdr tail)))))))

(defun meep--rectangle-edit-replay-lines (macro col line-direction line-marker mark-marker)
  "Replay MACRO on remaining rectangle lines.
COL is the target column.  LINE-DIRECTION is +1 or -1.
LINE-MARKER and MARK-MARKER delimit the replay range."
  (let ((error-count 0)
        (attempted-edit-count 0)
        (continue
         ;; Guard: skip replay for single-line rectangles.
         ;; Unlikely but not an error.
         (let ((mark-bol
                (save-excursion
                  (goto-char mark-marker)
                  (pos-bol))))
           (cond
            ((> line-direction 0)
             (<= (marker-position line-marker) mark-bol))
            (t
             (>= (marker-position line-marker) mark-bol))))))
    (while continue
      (goto-char line-marker)
      (move-to-column col t)
      ;; Advance marker to the next target before executing.
      ;; For top-to-bottom this lets the marker track through
      ;; any lines the macro adds or removes.
      (cond
       ((= (pos-bol)
           (save-excursion
             (goto-char mark-marker)
             (pos-bol)))
        ;; Last target line, stop after executing.
        (setq continue nil))
       (t
        (save-excursion
          (cond
           ((not (zerop (forward-line line-direction)))
            ;; Ran out of lines (macro deleted some), stop after executing.
            (setq continue nil))
           (t
            (set-marker line-marker (point)))))))
      (meep--incf attempted-edit-count)
      (condition-case err
          (execute-kbd-macro macro)
        (error
         (meep--incf error-count)
         ;; Still logged to `*Messages*', just avoids echo area flickering.
         (let ((inhibit-message t))
           (message "meep: rectangle replay error at edit %d: %s"
                    attempted-edit-count
                    (error-message-string err))))))
    (when (> error-count 0)
      (message "Warning: found %d error(s) editing %d line(s), see log for details."
               error-count
               attempted-edit-count))))

(defun meep--rectangle-edit-replay-on-exit ()
  "Replay recorded keystrokes on remaining rectangle lines."
  (let ((data meep--rectangle-edit-data))
    ;; Clean up state.
    (kill-local-variable 'meep--rectangle-edit-data)
    ;; Remove self from exit hook.
    (let ((hook-sym (bray-state-get-hook-exit meep-state-insert)))
      (when hook-sym
        (remove-hook hook-sym #'meep--rectangle-edit-replay-on-exit t)))

    ;; Collect and unregister the listener before the data guard
    ;; so it is cleared even if data is unexpectedly nil.
    (let ((macro (repeat-fu-listener-unregister-and-collect (plist-get data :listener))))
      (when data
        (let ((col (plist-get data :column))
              (line-direction (plist-get data :line-direction))
              (line-marker (plist-get data :line-marker))
              (mark-marker (plist-get data :mark-marker))
              (undo-tail (plist-get data :undo-tail)))
          (when macro
            ;; `repeat-fu-listener-unregister-and-collect' returns nil
            ;; when no keys were recorded, never an empty vector.
            (meep--assert (length> macro 0))
            (save-mark-and-excursion
              (meep--rectangle-edit-replay-lines
               macro col line-direction line-marker mark-marker)))
          ;; Clean up marker.
          (meep--assert line-marker)
          (set-marker line-marker nil)
          ;; Fuse the entire operation (delete + edit + replay) into one undo step.
          ;;
          ;; Note: undo entries created by hooks that run after this
          ;; (e.g. state transition hooks) won't be included in the fuse.
          ;; Deferring to `post-command-hook' could capture those, but is unnecessarily
          ;; intrusive since mode switching shouldn't modify the buffer.
          ;; `buffer-undo-list' becoming `t' mid-edit is very unlikely,
          ;; (disabling undo while you change a block)
          ;; but shouldn't break things either.
          (when (and undo-tail (not (eq buffer-undo-list t)))
            (meep--rectangle-edit-undo-fuse undo-tail))
          ;; Restore mark so the rectangle selection can be re-created.
          (when (marker-position mark-marker)
            (set-mark (marker-position mark-marker)))
          (set-marker mark-marker nil))))))

(defun meep--rectangle-edit-enter-real-insert (beg end)
  "Enter real insert mode for rectangle editing between BEG and END.
Record keystrokes on the edit line and replay on remaining lines on exit.
When point is on the last line of the rectangle, edit that line;
otherwise edit the first line.
Callers must ensure `repeat-fu-mode' is active."
  (let ((hook-sym (bray-state-get-hook-exit meep-state-insert)))
    (cond
     ((not hook-sym)
      (message "No exit hook found for state: %S" meep-state-insert))
     (t
      (let* ((col-beg
              (save-excursion
                (goto-char beg)
                (current-column)))
             (col-end
              (save-excursion
                (goto-char end)
                (current-column)))
             (col (min col-beg col-end))
             (edit-last-line
              (>= (point)
                  (save-excursion
                    (goto-char end)
                    (pos-bol))))
             (line-direction
              (cond
               (edit-last-line
                -1)
               (t
                1)))
             (line-marker nil))

        (when (local-variable-p 'meep--rectangle-edit-data)
          (error "Rectangle edit already in progress"))

        (setq line-marker (make-marker))

        ;; Save undo position so the entire operation can be fused into one step on exit.
        ;; Skip when undo is disabled (`buffer-undo-list' is t).
        ;; Push a cursor-position entry first - `undo-boundary' is a no-op when
        ;; `buffer-undo-list' is nil because it considers nil "already a boundary".
        (unless (eq buffer-undo-list t)
          (push (point) buffer-undo-list)
          (undo-boundary))
        (let ((undo-tail
               (unless (eq buffer-undo-list t)
                 buffer-undo-list)))

          ;; Use a marker so `end' tracks through `delete-rectangle'
          ;; (the integer position can shift onto the wrong line
          ;; when characters are removed from lines above).
          (when edit-last-line
            (setq end (copy-marker end)))

          ;; Delete the rectangle content.
          (delete-rectangle beg end)

          (deactivate-mark t)

          ;; Go to the edit line.
          (goto-char
           (cond
            (edit-last-line
             end)
            (t
             beg)))

          (when (markerp end)
            (set-marker end nil))

          ;; Place marker at the first replay target (the adjacent line).
          (save-excursion
            (forward-line line-direction)
            (set-marker line-marker (point)))

          (move-to-column col t)

          ;; Store state (buffer-local for the duration of the edit).
          (make-local-variable 'meep--rectangle-edit-data)
          (setq meep--rectangle-edit-data
                (list
                 :column col
                 :line-direction line-direction
                 :line-marker line-marker
                 :undo-tail undo-tail
                 :mark-marker (copy-marker (mark-marker))
                 ;; Begin listener (records keys starting from the next command).
                 :listener (repeat-fu-listener-register)))

          ;; Add exit hook before entering insert mode so an immediate exit
          ;; (however unlikely) still triggers replay and cleanup.
          (add-hook hook-sym #'meep--rectangle-edit-replay-on-exit 0 t)

          ;; Enter insert mode.
          (meep--insert-impl)))))))

;;;###autoload
(defun meep-insert-change ()
  "Change the region, entering insert mode.
The region may be implied, see `meep-command-is-mark-set-on-motion-any'."
  (interactive "*")
  (cond
   ((bound-and-true-p rectangle-mark-mode)
    (cond
     ((and meep-repeat-fu-replay (bound-and-true-p repeat-fu-mode))
      (meep--rectangle-edit-enter-real-insert (region-beginning) (region-end)))
     (t
      ;; Sort of odd but this is how Emacs supports changing a region.
      (call-interactively #'string-rectangle))))

   (t
    ;; Read the implied region as bounds without activating it; insert mode
    ;; deactivates the region (below), so activating here would be undone.
    (let ((region-bounds (meep--mark-on-motion-maybe-activate-as-bounds)))

      ;; Always de-activate region, so hooks to leave "visual" mode run,
      ;; if any are set. Further it doesn't make sense to have an active
      ;; region when entering insert mode.
      (when (region-active-p)
        (deactivate-mark t))

      (when region-bounds
        (delete-region (car region-bounds) (cdr region-bounds))))

    (bray-state-stack-push meep-state-insert))))

;;;###autoload
(defun meep-insert-change-lines ()
  "Change the region, entering insert mode.
The region may be implied, see `meep-command-is-mark-set-on-motion-any'."
  (interactive "*")
  ;; The purpose of this is twofold:
  ;; - To quickly replace text on the current line.
  ;; - To quickly replace text on all lines,
  ;;   without having to wrestle with box selection in situations
  ;;   where the line at the boundary is shorter than lines in the rest of the block.
  ;;
  ;; NOTE: there are two modes for this action, the behavior is as follows:
  ;; - Without an active region the result is:
  ;;   - Delete the line back until the indentation level,
  ;;   - Enter insert mode.
  ;; - With an active region:
  ;;   - Clear all lines (without deleting the lines).
  ;;   - Fill them to the indentation level of the 1st non blank line.
  ;;   - Add a rectangular region.
  ;;   - Change the rectangular region.
  ;;
  ;;   Details:
  ;;   - When the end of the selection reaches a line without selecting any text on that line,
  ;;     the line is not included in the change.
  ;;     This is done because of the default behavior of `meep-region-expand-to-line-bounds'
  ;;     Where the cursor is always moved to the following line.
  (cond
   ((region-active-p)
    ;; Sort of odd but this is how emacs supports changing a region.

    ;; This is reasonably involved...
    (let* ((beg (region-beginning))
           (end (region-end))
           (is-forward (eq (point) end)))
      (save-excursion
        ;; Contract the last line if it's on the line-beginning.
        (goto-char end)
        (cond
         ((eq end (pos-bol))
          ;; Move to the previous lines end.
          (meep--decf end))
         (t
          ;; Move to the line end.
          (setq end (pos-eol))))

        (goto-char beg)
        (setq beg (pos-bol))
        (let* ((indent-str (meep--indent-calc-in-region-from-first-non-blank-or-non-empty beg end))
               (indent-len (length indent-str))
               (offset 0))
          (goto-char beg)
          (while (< (point) end)
            (let* ((eol (pos-eol))
                   (line-offset (- indent-len (- eol (point)))))
              (meep--incf offset line-offset)
              (meep--replace-in-region indent-str (point) eol)
              (meep--incf end line-offset)
              (goto-char (min end (+ 1 eol line-offset)))))

          (meep--incf beg indent-len)))

      ;; Restore the region (without activating it).
      (cond
       (is-forward
        (meep--set-marker beg)
        (goto-char end))
       (t
        (meep--set-marker end)
        (goto-char beg)))

      ;; Not essential but displays a little strangely otherwise.
      (rectangle-mark-mode 1))

    ;; Run the actual replacement.
    (cond
     ((and meep-repeat-fu-replay (bound-and-true-p repeat-fu-mode))
      (meep--rectangle-edit-enter-real-insert (region-beginning) (region-end)))
     (t
      (call-interactively #'string-rectangle))))

   (t
    ;; Inline `meep-move-line-non-space-beginning'
    (let* ((eol (pos-eol))
           (bol
            (progn
              (beginning-of-line)
              (skip-chars-forward "[:blank:]" (pos-eol))
              (point))))
      (unless (eq bol eol)
        (delete-region bol eol)))
    (bray-state-set meep-state-insert))))

(defun meep--insert-into-last-impl (move)
  "Insert text into the last insert point.
When MOVE is non-nil, delete the original text."
  (let ((pos-last-insert (meep--register-position-or-message meep-state-insert-register)))
    (cond
     ((null pos-last-insert)
      nil)
     ((null (mark))
      (message "No mark found!")
      nil)
     (t
      ;; Intentionally use the region even if it's not active.
      (let* ((beg (region-beginning))
             (end (region-end))
             (text (and beg end (buffer-substring-no-properties beg end))))
        (when (and beg end move)
          (delete-region beg end)

          (cond
           ((<= end pos-last-insert)
            (meep--decf pos-last-insert (- end beg)))
           ;; This is effectively a NOP: moving text into a region *within* the text range.
           ;; Support this for consistency, since it's not technically invalid,
           ;; but it's also unlikely to be a useful edit from a user perspective.
           ((<= beg pos-last-insert)
            (setq pos-last-insert beg))))

        (goto-char pos-last-insert)
        (meep-insert)
        (when text
          (insert text)))
      t))))

;;;###autoload
(defun meep-insert-into-last-copy ()
  "Insert text into last insert point (copying it).

When there is no active region, the symbol at point is used."
  (interactive "*")
  (meep--insert-into-last-impl nil))

;;;###autoload
(defun meep-insert-into-last-move ()
  "Insert text into last insert point (moving it).

When there is no active region, the symbol at point is used."
  (interactive "*")
  (meep--insert-into-last-impl t))

;;;###autoload
(defun meep-insert-open-above ()
  "Open a newline above and switch to INSERT state."
  (interactive "*")
  (beginning-of-line)
  (save-mark-and-excursion
    (newline))
  (indent-according-to-mode)
  (bray-state-set meep-state-insert))

;;;###autoload
(defun meep-insert-open-below ()
  "Open a newline below and switch to INSERT state."
  (interactive "*")
  (end-of-line)
  (newline)
  (indent-according-to-mode)
  (bray-state-set meep-state-insert))

;;;###autoload
(defun meep-insert-line-beginning ()
  "Move to the line indentation start and switch to INSERT state."
  (interactive "*")
  (back-to-indentation)
  (bray-state-set meep-state-insert))

;;;###autoload
(defun meep-insert-line-end ()
  "Move to the line end and switch to INSERT state."
  (interactive "*")
  (end-of-line)
  (bray-state-set meep-state-insert))


;; ---------------------------------------------------------------------------
;; Clipboard: System Only
;;
;; These commands only wrap the system's clipboard,
;; without mixing the kill-ring or primary clipboard - for predictable results.

(defun meep--clipboard-only-cut-or-copy-impl (beg end do-cut)
  "Copy or cut to system clipboard.
Arguments BEG and END define the region.
When DO-CUT is non-nil, cut instead of copying."
  (cond
   ((eq beg end)
    (message "%s empty region, doing nothing."
             (cond
              (do-cut
               "Cut")
              (t
               "Copy"))))
   (t
    (unless interprogram-cut-function
      (user-error "No clipboard integration available in this terminal"))
    (let ((text (buffer-substring-no-properties beg end)))
      (when do-cut
        (delete-region beg end))

      (let ((select-enable-clipboard t)
            (select-enable-primary nil))
        (funcall interprogram-cut-function text))
      (setq deactivate-mark t)))))

;;;###autoload
(defun meep-clipboard-only-copy ()
  "Copy the region to the system clipboard."
  (interactive)
  (meep--clipboard-only-cut-or-copy-impl (region-beginning) (region-end) nil))

;;;###autoload
(defun meep-clipboard-only-cut ()
  "Cut the region to the system clipboard."
  (interactive "*")
  (meep--clipboard-only-cut-or-copy-impl (region-beginning) (region-end) t))

;;;###autoload
(defun meep-clipboard-only-cut-line ()
  "Cut the whole line to the system clipboard."
  (interactive "*")
  ;; Note that this command writes to the system clipboard.
  ;; (kill-whole-line)
  (meep--with-respect-goal-column
   (meep--clipboard-only-cut-or-copy-impl (pos-bol) (min (1+ (pos-eol)) (point-max)) t)))

(defun meep--clipboard-only-yank-impl ()
  "Yank-replace from the system clipboard."
  (unless interprogram-paste-function
    (user-error "No clipboard integration available in this terminal"))
  (let ((text (funcall interprogram-paste-function)))
    ;; This is strange that emacs cannot access its own selection.
    ;; Copy sets this value, could investigate this further.
    (unless text
      (setq text (bound-and-true-p gui--last-selected-text-clipboard)))

    (unless text
      (user-error "No text in clipboard"))

    (let ((bounds nil))
      (when (region-active-p)
        (setq bounds (cons (region-beginning) (region-end)))
        (deactivate-mark t)
        (delete-region (car bounds) (cdr bounds))))

    (let ((pos-init (point)))
      (when yank-transform-functions
        (run-hook-wrapped
         'yank-transform-functions
         (lambda (f)
           (setq text (funcall f text))
           nil)))

      (insert text)

      (meep--set-marker pos-init))))

;; Used by paste-and-indent.
(defun meep--region-strip-indentation (beg end)
  "Remove indentation in region from BEG to END."
  (save-excursion
    (goto-char beg)
    (setq beg (pos-bol))
    (goto-char end)
    (setq end (pos-eol))

    (let ((col-min most-positive-fixnum))
      (goto-char beg)
      (goto-char (pos-bol))
      (while (< (point) end)
        (let ((eol
               (progn
                 (goto-char (pos-eol))
                 (point)))
              (bol
               (progn
                 (goto-char (pos-bol))
                 (point))))
          (ignore bol) ;; Happens to be unused, the motion is needed.
          (skip-chars-forward "[:blank:]" eol)
          (unless (eq (point) eol)
            (setq col-min (min col-min (current-column)))))

        (unless (zerop (forward-line 1))
          (goto-char end)))

      (when (eq col-min most-positive-fixnum)
        (setq col-min 0))

      (unless (zerop col-min)
        (goto-char end)
        (while (and beg (<= beg (point)))
          (let ((eol
                 (progn
                   (goto-char (pos-eol))
                   (point)))
                (bol
                 (progn
                   (goto-char (pos-bol))
                   (point))))
            (ignore eol) ;; Happens to be unused, keep for symmetry.
            (move-to-column col-min)
            (unless (eq (point) bol)
              (delete-region bol (point)))

            (cond
             ((<= bol beg)
              ;; End loop.
              (setq beg nil))
             (t ;; Step to the previous line.
              (unless (zerop (forward-line -1))
                (goto-char beg))))))))))

;;;###autoload
(defun meep-clipboard-only-yank-with-indent ()
  "Yank from the system clipboard, replacing the region (indenting the content)."
  (interactive "*")
  (let ((yank-transform-functions
         (list
          (lambda (str)
            (with-temp-buffer
              (insert str)
              (meep--region-strip-indentation (point-min) (point-max))
              (buffer-string))))))
    (meep-clipboard-only-yank)))

;;;###autoload
(defun meep-clipboard-only-yank ()
  "Yank from the system clipboard, replacing the region."
  (interactive "*")
  (meep--clipboard-only-yank-impl))


;; ---------------------------------------------------------------------------
;; Clipboard: Kill Ring
;;
;; These commands wrap the kill-ring, without mixing the system clipboard.
;;
;; Note that line-wise cut/copy is stored in the kill-ring.
;; Yanking (pasting) a line-wise region yanks from the line beginning.
;;
;; So line-wise kill and yank can be used to operate on lines without the need
;; to place the point at the beginning of the line.
;;
;; If you wish to override this behavior, you may activate the region with an empty range,
;; since an active region always defines the range.
;;
;; Note that rect-wise regions are also stored in the kill-ring and paste from the top-left.

;; Currently only used to differentiate region types (line-wise and rect-wise).
(defun meep--yank-handler-line-wise (&rest args)
  "Line-wise yank-handler, forward ARGS to `insert'."
  (apply #'insert args))
(defun meep--yank-handler-rect-wise (&rest args)
  "Rectangle-wise yank-handler, forward ARGS to `insert'."
  (apply #'insert args))
(defconst meep--yank-handler-from-region-type-alist
  '((line-wise . meep--yank-handler-line-wise) (rect-wise . meep--yank-handler-rect-wise)))

(defun meep--yank-handler-from-region-type (region-type)
  "Return the yank-handler from the REGION-TYPE or nil."
  (declare (important-return-value t))
  (cdr (assq region-type meep--yank-handler-from-region-type-alist)))

(defun meep--yank-handler-to-region-type (yank-handler)
  "Return region type from the YANK-HANDLER or nil."
  (declare (important-return-value t))
  (car (rassq yank-handler meep--yank-handler-from-region-type-alist)))

(defun meep--wrap-current-kill (n &optional do-not-move)
  "Like `current-kill' but only use the kill ring.
N and DO-NOT-MOVE are passed to `current-kill'."
  (declare (important-return-value t))
  (let ((interprogram-paste-function nil))
    (current-kill n do-not-move)))

(defun meep--clipboard-killring-yank-impl (n do-not-move rotate-as-stack)
  "Yank-replace the N'th element from kill ring.
When DO-NOT-MOVE is non-nil, don't modify the kill ring.
When ROTATE-AS-STACK is non-nil, step to the next item in the kill ring."
  (let* ((text (meep--wrap-current-kill n do-not-move))
         (yank-handler (get-text-property 0 'yank-handler text))
         ;; A NOP if yank-handler is nil (harmless).
         (region-type (meep--yank-handler-to-region-type (car yank-handler))))

    (unless do-not-move
      ;; Step onto the next item (behave like a stack).
      (when rotate-as-stack
        ;; (meep--wrap-current-kill (1+ n))
        (cond
         (kill-ring-yank-pointer
          (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer)))
         (t
          (setq kill-ring-yank-pointer kill-ring)))))

    (cond
     ;; Rectangle paste.
     ((eq region-type 'rect-wise)
      (when (region-active-p)
        ;; Important for the point to be at the start.
        ;; So pasting replaces this region.
        ;; This is needed for both `line-wise' and `rect-wise' paste.
        (when (< (mark) (point))
          (let ((pos-orig (point)))
            (goto-char (mark))
            (meep--set-marker pos-orig)))

        (cond
         ((bound-and-true-p rectangle-mark-mode)
          (deactivate-mark t)
          (delete-rectangle (region-beginning) (region-end)))
         (t
          (let ((bounds (cons (region-beginning) (region-end))))
            (deactivate-mark t)
            ;; Deleting the region doesn't work so nicely with block pasting.
            ;; Instead, make each line blank, then the block paste replaces
            ;; the empty lines
            (save-restriction
              (save-excursion
                (narrow-to-region (car bounds) (cdr bounds))
                (goto-char (point-min))
                (let ((keep-searching t))
                  (while keep-searching
                    (delete-region (pos-bol) (pos-eol))
                    (unless (zerop (forward-line 1))
                      (setq keep-searching nil))))))))))

      ;; Note, no need to set the marker.
      (let ((lines (string-split text "\n")))
        (insert-rectangle lines)))

     ((or (null region-type) (eq region-type 'line-wise))
      ;; Ignore the text's region type if we already have an active region.
      ;; Because an active region implies the region is replaced,
      ;; there is no need for line-wise logic as pasting into a line-wise
      ;; region is implicitly line-wise.
      (cond
       ((region-active-p)
        (let ((bounds (cons (region-beginning) (region-end))))
          (deactivate-mark t)
          (delete-region (car bounds) (cdr bounds))))
       ;; Only use line-wise when there is no active-region.
       ((eq region-type 'line-wise)
        (goto-char (pos-bol))))

      (let ((pos-init (point)))
        ;; TODO: there may be aspects of `yank' we want to copy.
        (insert-for-yank text)
        (meep--set-marker pos-init)))
     (t
      ;; Internal error.
      (error "Unexpected region type %S (this is a bug)" region-type)))))

(defun meep--clipboard-killring-yank-impl-interactive (arg do-not-move rotate-as-stack)
  "Handle interactive part of yanking, interpreting raw ARG.
DO-NOT-MOVE and ROTATE-AS-STACK are passed to the implementation."
  (cond
   ;; Simplifies logic below if this is caught early.
   ((null kill-ring)
    (message "Kill ring is empty"))
   (t
    (setq arg
          (cond
           ((listp arg)
            0)
           ((eq arg '-)
            -2)
           (t
            (1- arg))))
    (meep--clipboard-killring-yank-impl arg do-not-move rotate-as-stack))))

(defun meep--clipboard-killring-cut-or-copy (beg end region-type do-cut)
  "Like `kill-region' but respects MEEP clipboard settings.
Arguments BEG and END define the region.
When DO-CUT is non-nil, delete the region.

When REGION-TYPE is non-nil, store the region type in the kill ring.
This may be used when yanking."
  (cond
   ((eq beg end)
    (message "%s empty region, doing nothing."
             (cond
              (do-cut
               "Cut")
              (t
               "Copy"))))
   (t
    (let ((select-enable-clipboard nil)
          (text
           (cond
            ((eq region-type 'rect-wise)
             ;; NOTE: `copy-rectangle-as-kill' does some other things we may want to do.
             ;; Nothing essential though.
             (mapconcat #'identity (extract-rectangle beg end) "\n"))
            (t
             (buffer-substring-no-properties beg end)))))

      (when region-type
        (let ((yank-handler (list (meep--yank-handler-from-region-type region-type) nil)))
          (meep--assert yank-handler) ; Otherwise the region-type is invalid.
          (add-text-properties 0 (length text) (list 'yank-handler yank-handler) text)))

      (kill-new text)
      (when do-cut
        (cond
         ((eq region-type 'rect-wise)
          (delete-rectangle beg end))
         (t
          (delete-region beg end))))

      ;; Match behavior for copying the clipboard.
      (setq deactivate-mark t)))))

;;;###autoload
(defun meep-clipboard-killring-cut ()
  "Kill the current region.
The region need not be active."
  (interactive "*")
  (let* ((scope
          (or (meep--region-or-mark-bounds)
              ;; No mark set: `region-beginning' signals the standard "mark
              ;; is not set" error, matching the prior behavior.
              (cons (region-beginning) (region-end))))
         (region-type (meep--state-region-type)))
    (meep--clipboard-killring-cut-or-copy (car scope) (cdr scope) region-type t)))

;;;###autoload
(defun meep-clipboard-killring-copy ()
  "Add the current region to the `kill-ring'.
The region need not be active."
  (interactive)
  (let* ((scope
          (or (meep--region-or-mark-bounds)
              ;; No mark set: `region-beginning' signals the standard "mark
              ;; is not set" error, matching the prior behavior.
              (cons (region-beginning) (region-end))))
         (region-type (meep--state-region-type)))
    (meep--clipboard-killring-cut-or-copy (car scope) (cdr scope) region-type nil)))

;;;###autoload
(defun meep-clipboard-killring-cut-line ()
  "Kill the whole line."
  (interactive "*")
  ;; Note that this command writes to the system clipboard.
  ;; (kill-whole-line)
  (let ((region-type 'line-wise)
        (beg (pos-bol))
        (end (min (1+ (pos-eol)) (point-max)))
        (do-cut t))
    (meep--with-respect-goal-column
     (meep--clipboard-killring-cut-or-copy beg end region-type do-cut))))

;;;###autoload
(defun meep-clipboard-killring-copy-line ()
  "Copy the whole line to the kill ring."
  (interactive)
  ;; Note that this command writes to the system clipboard.
  ;; (kill-whole-line)
  (let ((region-type 'line-wise)
        (beg (pos-bol))
        (end (min (1+ (pos-eol)) (point-max)))
        (do-cut nil))
    (meep--with-respect-goal-column
     (meep--clipboard-killring-cut-or-copy beg end region-type do-cut))))

;; TODO: a pop like emacs which cycles.
;;;###autoload
(defun meep-clipboard-killring-yank-pop-stack (arg)
  "Yank the ARG'th item from the `kill-ring', rotating it.

Rotating the kill ring means you may kill multiple items,
then conveniently yank those items afterwards."
  (interactive "*P")
  (meep--clipboard-killring-yank-impl-interactive arg nil t))

;;;###autoload
(defun meep-clipboard-killring-yank (arg)
  "Yank the ARG'th item from the `kill-ring'.
The region is replaced (when active)."
  (interactive "*P")
  (meep--clipboard-killring-yank-impl-interactive arg t nil))


;; ---------------------------------------------------------------------------
;; Clipboard: Register (Implementation)

(defun meep--clipboard-register-copy-impl (reg)
  "Copy in the active region and place it in register REG."
  (let ((beg (region-beginning))
        (end (region-end)))
    (copy-to-register reg beg end nil t)))

(defun meep--clipboard-register-cut-impl (reg)
  "Cut in the active region and place it in register REG."
  (let ((beg (region-beginning))
        (end (region-end)))
    (copy-to-register reg beg end nil t)
    (delete-region beg end)))

(defun meep--clipboard-register-yank-impl (reg)
  "Yank from register REG."
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (deactivate-mark t)
      (delete-region beg end)))

  (let ((pos-init (point)))
    (insert-register reg t)
    (meep--set-marker pos-init)))


;; ---------------------------------------------------------------------------
;; Clipboard: Register

;; Allow overriding.
(defvar meep--clipboard-register-current nil)

(defvar meep-clipboard-register-map (make-sparse-keymap)
  "Key-map for register clipboard actions.

Used by `meep-clipboard-register-actions'.")

(defun meep-clipboard-register-actions ()
  "Set the pre-defined register to use for `meep-clipboard-register-*' commands.

Use the `meep-clipboard-register-map' key-map."
  (interactive)
  (setq meep--clipboard-register-current (register-read-with-preview "Clipboard register: "))
  (set-transient-map meep-clipboard-register-map
                     nil ; Don't keep the keymap active.
                     nil ; Don't run anything when exiting.
                     "Clipboard keys: %k or any other to exit"))

;;;###autoload
(defun meep-clipboard-register-copy ()
  "Copy to pre-defined register."
  (interactive)
  (cond
   ((region-active-p)
    (let ((reg meep--clipboard-register-current))
      (meep--clipboard-register-copy-impl reg)))
   (t
    (message "No region to copy"))))

;;;###autoload
(defun meep-clipboard-register-cut ()
  "Cut to pre-defined register."
  (interactive "*")
  (cond
   ((region-active-p)
    (let ((reg meep--clipboard-register-current))
      (meep--clipboard-register-cut-impl reg)))
   (t
    (message "No region to cut"))))

;;;###autoload
(defun meep-clipboard-register-yank ()
  "Yank from pre-defined register."
  (interactive "*")
  (let ((reg meep--clipboard-register-current))
    (meep--clipboard-register-yank-impl reg)))

;;;###autoload
(defun meep-clipboard-register-yank-lines ()
  "Yank from pre-defined register as lines."
  (interactive "*")
  (let ((reg meep--clipboard-register-current))
    (cond
     ((region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (deactivate-mark t)
        (delete-region beg end)))
     (t
      (goto-char (pos-bol))))
    (let ((pos-init (point)))
      (insert-register reg t)
      (meep--set-marker pos-init))))


;; ---------------------------------------------------------------------------
;; Keypad Mode
;;
;; Support entering a sequence of keys without the need to hold modifiers, see:
;; `keypad mode <https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#keypad>`__.

(defmacro meep--keypad-with-maybe-which-key (keyseq-fn &rest body)
  "Run BODY with bindings that let `which-key' display the keypad's prefix.
KEYSEQ-FN is a function returning the current key sequence in the form
`kbd' returns (a string or vector).  The bindings are harmless when
`which-key' isn't loaded.

The bindings span both `read-event' and any `which-key-C-h-dispatch' the
keypad invokes, so which-key's idle timer sees the right prefix while
its own `read-key' is blocking."
  (declare (indent 1))
  `(let ((this-command nil)
         (which-key-this-command-keys-function ,keyseq-fn))
     ,@body))

(defmacro meep--keypad-which-key-c-h-maybe (ch prefix-arg-var)
  "Forward CH to `which-key-C-h-dispatch' if it is `help-char' and which-key is showing.
Returns non-nil when the event was consumed (any binding in
`which-key-C-h-map': paging, digit-arg, undo, toggle-docstrings,
show-standard-help, abort).

PREFIX-ARG-VAR is a symbol naming the caller's local that tracks the
prefix arg for the eventual command.  It is bound through dispatch as
`current-prefix-arg' so reload-style commands preserve it via
`(setq prefix-arg current-prefix-arg)', then read back from `prefix-arg'
which `which-key-digit-argument' sets to the typed digit.
`unread-command-events' is shadowed so the prefix sequence which-key
re-feeds for the normal dispatcher doesn't leak into the keypad's next
`read-event'."
  (unless (symbolp prefix-arg-var)
    (error "PREFIX-ARG-VAR must be a symbol, got: %S" prefix-arg-var))
  `(when (and (eq ,ch help-char)
              (bound-and-true-p which-key-use-C-h-commands)
              (fboundp 'which-key--popup-showing-p)
              (which-key--popup-showing-p))
     (let ((unread-command-events nil)
           (prefix-arg nil)
           (current-prefix-arg ,prefix-arg-var))
       (which-key-C-h-dispatch)
       ;; Skip when dispatch left it nil (e.g. `which-key-show-standard-help')
       ;; - preserves an earlier `C-h <N>' digit.
       (when prefix-arg
         (setq ,prefix-arg-var prefix-arg)))
     t))

;; NOTE: Based on MEOW's keypad mode
;;;###autoload
(defun meep-keypad ()
  "Begin entering a key sequence."
  (interactive)
  ;; Functions and constants.
  (let* ((string-from-keyseq-default
          ;; Return string from sequence, using the first key
          ;; (for items that contain multiple).
          (lambda (keyseq)
            (let ((str ""))
              (while keyseq
                (let ((ch-str-list (pop keyseq)))
                  (setq str (concat (car ch-str-list) str))))
              str)))

         (string-from-keyseq-all
          ;; Return string from sequence, expanded from multiple keys.
          (lambda (keyseq)
            (let ((str "")
                  (result (list)))
              (while keyseq
                (let ((ch-str-list (pop keyseq)))
                  (cond
                   ;; There is more than one key, expand all possible options.
                   ((cdr-safe ch-str-list)
                    (let ((head (funcall string-from-keyseq-default keyseq)))
                      (dolist (ch-str ch-str-list)
                        (push (concat head ch-str str) result)))
                    ;; Break.
                    (setq keyseq nil))
                   (t
                    (setq str (concat (car ch-str-list) str))))))

              (unless result
                (setq result (list str)))
              result)))

         (ellipsis-str
          (cond
           ((char-displayable-p ?…)
            "…")
           (t
            "..."))))

    ;; https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#keypad
    (let ((found nil)
          (is-first t)
          ;; Prefix arg for the eventual command; `which-key-digit-argument'
          ;; may overwrite this via the C-h intercept.
          (captured-prefix-arg current-prefix-arg)
          ;; Build a sequence of keys, note that this is stored in reverse order
          ;; for conveniently adding to the start.
          ;; Each element may contain multiple keys - to ensure both
          ;; <delete> and DEL are properly detected.
          (keyseq (list (list "C-")))
          (was-space nil)

          ;; A pair of maps forward/reverse keys and values may be symbols or integers.
          ;; - `tab' -> `13' translates to:
          ;; - "<tab>" -> "TAB".
          ;; Maps in both directions to allow the result of `read-event' to be either.
          (keymap-subst-map-list
           (let ((map-fwd (make-hash-table :test #'eq))
                 (map-rev (make-hash-table :test #'eq)))
             ;; NOTE: `local-function-key-map' includes `function-key-map'.
             (map-keymap
              (lambda (k v)
                (when (symbolp k)
                  (cond
                   ((symbolp v)
                    (puthash k (cons v (gethash k map-fwd)) map-fwd)
                    (puthash v (cons k (gethash v map-rev)) map-rev))
                   ((and (vectorp v) (eq 1 (length v)))
                    (let ((i (aref v 0)))
                      (when (integerp i)
                        (puthash k (cons i (gethash k map-fwd)) map-fwd)
                        (puthash i (cons k (gethash i map-rev)) map-rev)))))))
              local-function-key-map)
             (list map-fwd map-rev))))

      (while (null found)
        (let ((maybe-complete nil)
              (handled-elsewhere nil))
          (meep--keypad-with-maybe-which-key
              (lambda ()
                (let ((keyseq-str
                       (funcall string-from-keyseq-default keyseq)))
                  ;; Strip incomplete keys, this doesn't help and causes
                  ;; an error when multiple modifiers are used such as "C-M-".
                  ;; Although it could be handy if `which-key' would filter
                  ;; based on the incomplete binding.
                  (when (string-suffix-p "-" keyseq-str)
                    (let ((split-by " "))
                      (setq keyseq-str
                            (mapconcat #'identity (butlast (split-string keyseq-str split-by))
                                       split-by))))
                  (kbd keyseq-str)))
            (let ((ch
                   (read-event
                    (concat
                     "Keypad [" (funcall string-from-keyseq-default keyseq)
                     (cond
                      (was-space
                       (concat " " ellipsis-str))
                      ((string-suffix-p "-" (car (car keyseq)))
                       ellipsis-str)
                      (t
                       (concat " C-" ellipsis-str)))
                     "]"
                     (cond
                      (is-first
                       (propertize (concat ", m for M-" ellipsis-str ", g for C-M-" ellipsis-str)
                                   'face
                                   'font-lock-comment-face))
                      (t
                       "")))))

                  (ch-str-list nil))

              ;; Expand `ch-str-list' to include all translated keys.
              (let ((ch-str (single-key-description ch)))
                (dolist (map keymap-subst-map-list)
                  (let ((stack (list ch))
                        (stack-visited (list)))
                    (while stack
                      (let ((v (pop stack)))
                        (push v stack-visited)
                        (dolist (v-other (gethash v map))
                          (unless (memq v-other stack-visited)
                            ;; Only push others, the original char is pushed later.
                            (let ((ch-str-other (single-key-description v-other)))
                              (unless (string-equal ch-str ch-str-other)
                                (push ch-str-other ch-str-list)))
                            (push v-other stack)))))))
                ;; Not all that likely but possible.
                (setq ch-str-list (delete-dups ch-str-list))
                ;; Important this is first, so `string-from-keyseq-default' is predictable.
                (push ch-str ch-str-list))

              (cond
               ;; `read-event' skips the normal prefix-key dispatcher, so
               ;; forward `C-h' to which-key here.
               ((meep--keypad-which-key-c-h-maybe ch captured-prefix-arg)
                (setq handled-elsewhere t))
               ;; Special case: keypad -> m replaces the initial: `C-' with `M-'.
               ;; Without this, `M-' shortcuts aren't possible.
               ((and is-first (eq ch ?m))
                (setcar keyseq (list "M-")))
               ;; Special case: keypad -> g replaces the initial: `C-' with `C-M-'.
               ;; Without this, `C-M-' shortcuts aren't possible.
               ((and is-first (eq ch ?g))
                (setcar keyseq (list "C-M-")))
               ((string-suffix-p "-" (car (car keyseq)))
                (push ch-str-list keyseq)
                (setq maybe-complete t))

               (was-space
                (push (list " ") keyseq)
                (push ch-str-list keyseq)

                (setq was-space nil)
                (setq maybe-complete t))

               (t
                (cond
                 ((eq ch ?\s)
                  (setq was-space t))
                 ((eq ch ?m)
                  (push (list " M-") keyseq))
                 ((eq ch ?g)
                  (push (list " C-M-") keyseq))
                 (t
                  (push (list " C-") keyseq)
                  (push ch-str-list keyseq)
                  (setq maybe-complete t)))))))

          (when maybe-complete
            (let* ((kbd-keyseq nil)
                   (bind nil)
                   (keyseq-str-list (funcall string-from-keyseq-all keyseq))
                   (keyseq-str-default (car keyseq-str-list))
                   (keyseq-str ""))

              (while keyseq-str-list
                (setq keyseq-str (pop keyseq-str-list))
                (setq kbd-keyseq (kbd keyseq-str))
                (setq bind (key-binding kbd-keyseq))

                (when bind
                  ;; break.
                  (setq keyseq-str-list nil)))

              ;; (printf "RESULT: '%S' | %S | %S\n" keyseq bind (type-of bind))
              (cond
               ((or (and bind (symbolp bind) (commandp bind))
                    (and bind (null (symbolp bind)) (interpreted-function-p bind)))

                ;; Don't look any further.
                (setq found t)

                ;; Don't log, just show the message...
                (let ((message-log-max nil))
                  (message "[%s] -> %s"
                           keyseq-str
                           (cond
                            ((symbolp bind)
                             (symbol-name bind))
                            (t
                             "<interpreted function ...>"))))

                ;; For some reason using `execute-kbd-macro' doesn't activate the command in
                ;; quite the same way, for example, C-x C-f doesn't show the same file finder,
                ;; fortunately it's possible to call `bind' so it's not an issue here.
                ;; Ideally executing the macro could work in all cases though.
                (cond
                 ((and (symbolp bind) (commandp bind))
                  (setq this-command bind)
                  ;; Don't shadow `prefix-arg' - `universal-argument' (and the like)
                  ;; need to write it for the next command.
                  (let ((current-prefix-arg captured-prefix-arg))
                    (call-interactively bind)))
                 (t
                  ;; `execute-kbd-macro' rotates `prefix-arg' ->
                  ;; `current-prefix-arg' for its first command.
                  (let ((prefix-arg captured-prefix-arg))
                    (execute-kbd-macro kbd-keyseq)))))

               ((or (consp bind)
                    ;; A symbol may reference a key-map.
                    (and bind (symbolp bind) (keymapp bind)))
                ;; A key-map, keep looking.
                nil)
               ((null bind)
                (user-error "Keypad [%s] unknown!" keyseq-str-default))
               (t
                ;; Should never happen, but this is early development so it might.
                (user-error "Keypad [%s] unknown type: %S !" keyseq-str-default (type-of bind))))))

          (unless handled-elsewhere
            (setq is-first nil)))))))


;; ---------------------------------------------------------------------------
;; Command Advice
;;
;; Use this advice for commands you wish to use as "motions".
;; This causes them to set the mark before motion (unless overridden when repeating).
;;
;; To support repeating and selecting whole objects
;; (via ``meep-region-activate-and-reverse-motion``),
;; these commands should also accept an integer argument, representing the number of
;; times the motion is made, reversing when negative.
;; This is typically indicated using ``(interactive "p")``.

(defun meep--command-mark-on-motion-advice (old-fn &rest args)
  "Advice to conditionally set the mark on motion (call OLD-FN with ARGS).

Note that this only changes behavior when MEEP is used
\(when `meep-enabled-p' succeeds)."
  ;; Check if MEEP is enabled, so this doesn't change behavior when the same operations
  ;; are used elsewhere (minibuffer or similar).
  (cond
   ((meep-enabled-p)
    (meep--with-mark-on-motion-maybe-set
      (apply old-fn args)))
   (t
    (apply old-fn args))))

;;;###autoload
(defun meep-command-mark-on-motion-advice-add (cmd)
  "Add advice to CMD to set mark-on-motion.
This can be explicitly overridden (when repeating).
Use `meep-command-mark-on-motion-advice-remove' to remove the advice."
  (advice-add cmd :around #'meep--command-mark-on-motion-advice)
  (meep-command-prop-set cmd :mark-on-motion t))

;;;###autoload
(defun meep-command-mark-on-motion-advice-remove (cmd)
  "Remove advice added to CMD by `meep-command-mark-on-motion-advice-add'."
  (advice-remove cmd #'meep--command-mark-on-motion-advice)
  (meep-command-prop-remove cmd :mark-on-motion))


;; ---------------------------------------------------------------------------
;; Command Properties

;;;###autoload
(defun meep-command-prop-set (cmd prop value)
  "Set CMD property PROP to VALUE."
  (let ((sym 'meep))
    (put cmd sym (cons prop (cons value (get cmd sym))))))

;;;###autoload
(defun meep-command-prop-get (cmd prop)
  "Return the PROP property for CMD or nil."
  (declare (important-return-value t))
  (let* ((sym 'meep)
         (plist (get cmd sym)))
    (and plist (plist-get plist prop))))

;;;###autoload
(defun meep-command-prop-remove (cmd prop)
  "Remove PROP from CMD's properties, removing the `meep' property if empty."
  (let* ((sym 'meep)
         (plist (get cmd sym)))
    (when plist
      (setq plist (meep--plist-remove plist prop))
      (cond
       (plist
        (put cmd sym plist))
       (t
        (setplist cmd (meep--plist-remove (symbol-plist cmd) sym)))))))

;; Setup values...
(dolist (cmd
         '( ;; These don't set mark but allow marking.
           meep-move-char-prev
           meep-move-char-next
           ;; Use reverse as an adjustment.
           meep-region-activate-or-reverse
           meep-region-activate-and-reverse
           meep-region-activate-and-reverse-motion))
  (meep-command-prop-set cmd :mark-on-motion 'adjust))

(dolist (cmd
         '(meep-region-activate-or-reverse
           meep-region-activate-and-reverse
           meep-region-activate-and-reverse-motion
           meep-region-mark-bounds-of-char-inner
           meep-region-mark-bounds-of-char-outer
           meep-region-mark-bounds-of-char-contextual-inner
           meep-region-mark-bounds-of-char-contextual-outer))
  (meep-command-prop-set cmd :mark-activate t))

;; Commands whose primary purpose is to activate or modify the region, and which should be kept
;; as a repeatable selection operation (unlike commands that merely leave a region active as a
;; side-effect, such as jumping to a search match).
;; Currently only `repeat-fu' uses this.
(dolist (cmd
         '(meep-region-enable
           meep-region-enable-rectangle
           meep-region-expand-to-line-bounds
           meep-region-mark-bounds-of-char-contextual-inner
           meep-region-mark-bounds-of-char-contextual-outer
           meep-region-mark-bounds-of-char-inner
           meep-region-mark-bounds-of-char-outer
           meep-region-syntax-expand
           meep-region-toggle
           meep-region-toggle-rectangle))
  (meep-command-prop-set cmd :mark-activate-repeat t))

(dolist (cmd '(meep-digit-argument-repeat))
  (meep-command-prop-set cmd :digit-repeat t))

(dolist (cmd
         '(meep-clipboard-killring-cut-line
           meep-clipboard-only-cut-line
           meep-move-line-next
           meep-move-line-prev
           next-line
           previous-line))
  (meep-command-prop-set cmd :respect-temporary-goal-column t))

;; Currently only repeat-fu uses this.
(dolist (cmd
         '(meep-char-replace
           meep-char-insert
           meep-delete-char-next
           meep-delete-char-prev
           meep-delete-char-ring-next
           meep-delete-char-ring-prev
           meep-delete-char-ring-yank))
  (meep-command-prop-set cmd :mark-on-motion-exclude t))

;; The inactive mark should be used, but the action should not be repeated.
(dolist (cmd
         '( ;; Only this is used for interactive ISEARCH.
           isearch-exit
           ;; `'meep-isearch-regexp-prev'
           ;; `'meep-isearch-regexp-next'

           meep-isearch-at-point-prev
           meep-isearch-at-point-next
           meep-isearch-repeat-prev
           meep-isearch-repeat-next))
  (meep-command-prop-set cmd :mark-on-motion-no-repeat t))

(dolist (cmd
         '(meep-move-by-sexp-any-next
           meep-move-by-sexp-any-prev
           meep-move-by-sexp-out-next
           meep-move-by-sexp-out-prev
           meep-move-by-sexp-over-next
           meep-move-by-sexp-over-prev
           meep-move-find-char-on-line-at-next
           meep-move-find-char-on-line-at-prev
           meep-move-find-char-on-line-repeat-at-next
           meep-move-find-char-on-line-repeat-at-prev
           meep-move-find-char-on-line-repeat-till-next
           meep-move-find-char-on-line-repeat-till-prev
           meep-move-find-char-on-line-till-next
           meep-move-find-char-on-line-till-prev
           meep-move-same-syntax-or-symbol-next
           meep-move-same-syntax-or-symbol-prev
           meep-move-same-syntax-and-space-next
           meep-move-same-syntax-and-space-next-end
           meep-move-same-syntax-and-space-prev
           meep-move-same-syntax-next
           meep-move-same-syntax-prev
           meep-move-line-beginning
           meep-move-line-end
           meep-move-line-next
           meep-move-line-non-space-beginning
           meep-move-line-non-space-end
           meep-move-line-prev
           meep-move-list-item-next
           meep-move-list-item-next-end
           meep-move-list-item-prev
           meep-move-list-item-prev-end
           meep-move-matching-bracket-inner
           meep-move-matching-bracket-outer
           meep-move-matching-contextual-inner
           meep-move-matching-contextual-outer
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
           meep-move-to-bounds-of-comment-inner
           meep-move-to-bounds-of-defun-inner
           meep-move-to-bounds-of-line-inner
           meep-move-to-bounds-of-list-item-inner
           meep-move-to-bounds-of-paragraph-inner
           meep-move-to-bounds-of-sentence-inner
           meep-move-to-bounds-of-string-inner
           meep-move-to-bounds-of-visual-line-inner
           meep-move-word-next
           meep-move-word-next-end
           meep-move-word-prev
           meep-move-word-prev-end

           meep-move-to-bounds-of-paragraph
           meep-move-to-bounds-of-comment
           meep-move-to-bounds-of-string
           meep-move-to-bounds-of-line
           meep-move-to-bounds-of-visual-line
           meep-move-to-bounds-of-defun
           meep-move-to-bounds-of-list-item
           meep-move-to-bounds-of-sentence))
  (meep-command-prop-set cmd :mark-on-motion t))

(dolist (cmd-pair
         '((meep-move-line-next . next-line)
           (meep-move-line-prev . previous-line)
           (meep-move-line-end . move-end-of-line)
           (meep-move-line-beginning . move-beginning-of-line)))
  (meep-command-prop-set (car cmd-pair) :substitute (cdr cmd-pair)))


;; ---------------------------------------------------------------------------
;; Wrap Bray

;; Avoid dependencies having to depend directly on Bray.
;;;###autoload
(defalias 'meep-state #'bray-state)


;; ---------------------------------------------------------------------------
;; Wrapper Functions
;;
;; These only exist so variables can be accessed via forward declared functions.

;;;###autoload
(defun meep-state-insert ()
  "Return the current insert state as a symbol."
  (declare (important-return-value t))
  meep-state-insert)

;;;###autoload
(defun meep-command-is-mark-set-on-motion-no-repeat (cmd)
  "Return t if CMD is a motion command not to repeat."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :mark-on-motion-no-repeat)))
;;;###autoload
(defun meep-command-is-mark-set-on-motion-adjust (cmd)
  "Return t if CMD is a command that can adjust the mark."
  (declare (important-return-value t))
  (eq 'adjust (meep-command-prop-get cmd :mark-on-motion)))
;;;###autoload
(defun meep-command-is-mark-set-on-motion (cmd)
  "Return t if CMD is a set-mark-on-motion command."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :mark-on-motion)))
;;;###autoload
(defun meep-command-is-mark-set-on-motion-any (cmd)
  "Return t if CMD is any type of mark-on-motion command."
  (declare (important-return-value t))
  (or (meep-command-is-mark-set-on-motion cmd)
      (meep-command-is-mark-set-on-motion-adjust cmd)
      (meep-command-is-mark-set-on-motion-no-repeat cmd)))
;;;###autoload
(defun meep-command-is-mark-activate (cmd)
  "Return t if CMD activates the mark."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :mark-activate)))
;;;###autoload
(defun meep-command-is-mark-activate-repeat (cmd)
  "Return t if CMD activates the region as a repeatable selection operation."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :mark-activate-repeat)))
;;;###autoload
(defun meep-command-is-mark-on-motion-exclude (cmd)
  "Return t if CMD should be excluded from mark-on-motion."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :mark-on-motion-exclude)))
;;;###autoload
(defun meep-command-is-mark-respect-temporary-goal-column (cmd)
  "Return t if CMD should maintain the temporary goal column."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :respect-temporary-goal-column)))
;;;###autoload
(defun meep-command-is-digit-argument (cmd)
  "Return t if CMD is a numeric command."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :digit-repeat)))

;; ---------------------------------------------------------------------------
;; Presets
;;
;; Per-major-mode configuration is loaded on demand from files named
;; `meep-preset-MODE.el' anywhere on `load-path'.  For `major-mode'
;; MODE, a preset is the triple:
;;
;;   File:     meep-preset-MODE.el
;;   Feature:  (provide 'meep-preset-MODE)
;;   Function: (defun meep-preset-MODE () ALIST)
;;
;; The preset function returns an alist of `(VARIABLE . VALUE)' pairs
;; intended to override existing user options for that mode.

(defvar meep-preset-variables
  (list
   'meep-bounds-for-inner-comment
   'meep-match-bounds-of-char-contextual
   'meep-list-item-bounds
   'meep-surround-pairs)
  "Variables that meep presets are allowed to set.

A bundled `meep-preset-MODE.el' must restrict the keys of its
returned alist to symbols in this list.  The contract is checked
by the test suite, not enforced at runtime.")

(defvar meep--preset-cache (make-hash-table :test 'eq)
  "Hash table mapping `major-mode' to its preset alist.

Values:
- nil / absent  - preset has not yet been attempted for this mode.
- t             - cached-missing-state: attempted but no usable
                  preset (file missing, errored, no function defined,
                  preset returned nil/empty, or preset returned a
                  non-list value).
- ALIST         - attempted, the preset returned this alist.

The cached-missing-state value t is internal and never surfaces
from `meep-preset-ensure'.")

(defun meep--preset-try-mode (mode)
  "Attempt to load and call the preset for MODE.

Return one of:
- `continue' - no preset file exists for MODE; walk to its parent.
- `missing'  - an attempt was made but the result is unusable
               (load error, file present but missing the entry
               function, preset returning nil/empty or a
               non-list value).  Stop the walk; cache as missing.
- ALIST      - a non-empty alist returned by the preset.  Stop
               the walk; cache and apply this alist.

Execution errors signaled by the preset function are not caught
and propagate to the caller."
  (declare (important-return-value t))
  ;; NOTE: `intern' adds a symbol to the obarray for unsupported
  ;; modes, but `meep--preset-cache' ensures this happens at most
  ;; once per ancestor per session.
  (let* ((preset-sym (intern (concat "meep-preset-" (symbol-name mode))))
         (loaded
          (condition-case err
              (require preset-sym nil t)
            (error
             (lwarn 'meep :error "preset for %S failed to load: %S" mode err)
             'errored))))
    (cond
     ((null loaded)
      'continue)
     ((eq loaded 'errored)
      'missing)
     ((not (fboundp preset-sym))
      (lwarn 'meep :error "preset for %S loaded but `%S' is not defined" mode preset-sym)
      'missing)
     (t
      (let ((result (funcall preset-sym)))
        (cond
         ((null result)
          'missing)
         ((listp result)
          result)
         (t
          (lwarn 'meep :error "preset %S returned %S; expected an alist" preset-sym result)
          'missing)))))))

(defun meep--preset-lookup ()
  "Return the preset alist for the current `major-mode', or nil.

Memoized: performs the `derived-mode-parent' chain walk on the
first call for each `major-mode' and caches the result.  Does
*not* apply the alist to the current buffer; callers that want
to apply must do so themselves.

Shared cache/walk core for `meep-preset-ensure' and
`meep-preset-ensure-variable'."
  (declare (important-return-value t))
  (let ((cached (gethash major-mode meep--preset-cache)))
    (cond
     ;; Cached-missing-state.
     ((eq cached t)
      nil)
     ;; Cached alist.
     (cached
      cached)
     ;; First attempt - walk the `derived-mode-parent' chain.
     ;; `visited' guards against any pathological cycle in the
     ;; chain - Emacs prevents this in practice, but bounding the
     ;; loop is cheap insurance.
     (t
      (let ((mode major-mode)
            (visited nil)
            (result nil)
            (done nil))
        (while (and mode (not done) (not (memq mode visited)))
          (push mode visited)
          (pcase (meep--preset-try-mode mode)
            ('continue (setq mode (get mode 'derived-mode-parent)))
            ('missing (setq done t))
            (found
             (setq
              result found
              done t))))
        (puthash major-mode (or result t) meep--preset-cache)
        result)))))

;;;###autoload
(defun meep-preset-ensure ()
  "Load and apply the preset for the current `major-mode'.

Walks the `derived-mode-parent' chain starting at `major-mode';
the first ancestor with a `meep-preset-MODE' file on `load-path'
wins.  That file's preset function returns an alist of
`(VARIABLE . VALUE)' pairs.  Each VARIABLE is then set
buffer-locally to VALUE unless it is already buffer-local - a
prior buffer-local user override is preserved.

The alist is memoized globally under the current `major-mode',
so the chain walk happens at most once per Emacs session per
mode (including the negative case: a mode whose chain has no
preset is cached and not retried).

The walk continues to the parent only when *no preset file
exists* for an ancestor (the silent, common path).  Any explicit
attempt - load error, file present but missing the entry
function, preset returning nil/empty, preset returning a
non-list - is treated as the user's intent for that ancestor:
the walk stops, the result is cached as missing, and an explicit
empty or broken preset shadows any further parent.

Errors signaled by the preset function itself propagate; the
cache is left untouched so a fixed preset is retried on the
next call.

Return the alist, or nil if no preset is available."
  (declare (important-return-value t))
  (let ((alist (meep--preset-lookup)))
    (dolist (entry alist)
      (let ((var (car entry)))
        (unless (local-variable-p var)
          (set (make-local-variable var) (cdr entry)))))
    alist))

;;;###autoload
(defun meep-preset-ensure-variable (var)
  "Return VAR's value: buffer-local, then global, falling back to the preset."
  (declare (important-return-value t))
  (let ((val (and (boundp var) (symbol-value var))))
    (cond
     ((or (local-variable-p var) val)
      val)
     (t
      (cdr (assq var (meep--preset-lookup)))))))


;; ---------------------------------------------------------------------------
;; Public API
;;
;; Public non-interactive functions.

;;;###autoload
(defun meep-enabled-p ()
  "Return non-nil if MEEP is enabled."
  (declare (important-return-value t))
  (bound-and-true-p bray-mode))

;;;###autoload
(defun meep-bootstrap-once ()
  "Initialize MEEP.

This may be used with `use-package' to defer loading MEEP until needed."
  (ignore))

(provide 'meep)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; meep.el ends here
