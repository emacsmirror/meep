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
;; Starting out you may want to load Emacs with on of the
;; bundled `init.el' files, linked from this projects URL.

;;; Usage

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


;; ---------------------------------------------------------------------------
;; Custom Variables

(defcustom meep-mark-set-on-motion t
  "Motion sets the mark."
  :type 'boolean)

(defcustom meep-state-insert nil
  "The name of the state used for insert-mode (must be set)."
  :type 'symbol)

(defcustom meep-state-insert-register ?^
  "The register set when leaving insert mode.

Used by `meep-insert-last' which will enter insert mode at this location."
  :type 'register)

(defvar-local meep-state-region-elem nil
  "Supported values are symbols nil or \\='line-wise.

Note that line-wise navigation is not enforced,
this is a hint that commands may use.")


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

(defun meep--assert-failed ()
  "Raise an error."
  (error "Assertion failed"))

(defmacro meep--assert (condition)
  "Assert CONDITION is non-nil."
  `(unless ,condition
     (meep--assert-failed)))

(defmacro meep--swap-vars (i j)
  "Swap I & J."
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

;; ---------------------------------------------------------------------------
;; Internal Functions: Algorithms

(defun meep--plist-remove (plist key)
  "Remove KEY and its value from PLIST destructively.
Returns the modified P-list, or the original if KEY is not found."
  (cond
   ;; Empty P-list.
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
      ;; Walk the list two cells at a time
      (while (progn
               (setq next (cdr tail))
               (setq next-next (cdr next))
               (and next-next (not (eq (car next-next) key))))
        (setq tail next-next))
      ;; If we found the key, splice it out
      (when next-next
        (setcdr next (cdr (cdr next-next))))
      plist))))

(defun meep--ranges-overlap-p (list-a list-b)
  "Return t if any range in LIST-A overlaps any range in LIST-B.
Each list contains cons cells (START . END) with START <= END.
Stops at the first detected overlap."
  (let ((found nil))
    (while (and list-a (not found))
      (let* ((range-a (car list-a))
             (a-start (car range-a))
             (a-end (cdr range-a))
             (b-list list-b))
        (while (and b-list (not found))
          (let* ((range-b (car b-list))
                 (b-start (car range-b))
                 (b-end (cdr range-b)))
            ;; Overlap if ranges intersect at all:
            (when (and (<= a-start b-end) (<= b-start a-end))
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
- If all lines are blank the first non-empty.
- If all lines are empty - return the longest empty string.

It is expected that BEG & END have been extended to line end-points.
The behavior if they have not is undefined."
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
             ((eq (point) eol) ; Blank line (with white-space).
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
  "Utility to replace region from BEG to END with STR.
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
  "Move the point to the start of comment syntax.
When the point is at the very beginning of a comment
there may be no comment syntax information at point, move forward until
`syntax-ppss' information is available (typically only 1-2 characters).
Return t when the point was moved to the comment start."
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

(defun meep--rectangle-range-list-from-rectangle (beg end)
  "Return a list of ranges from a rectangle from BEG & END."
  (let* ((result (list))
         (accum-fn (lambda (beg end) (push (cons beg end) result))))
    (apply-on-rectangle
     ;; Make the values global.
     `(lambda (col-beg col-end)
        (let ((pos-beg nil)
              (pos-end nil))
          (save-excursion
            (move-to-column col-beg)
            (setq pos-beg (point))
            (move-to-column col-end)
            (setq pos-end (point))
            (funcall ,accum-fn pos-beg pos-end))))
     beg end)
    (nreverse result)))

(defun meep--columns-from-point-range (beg end)
  "Calculate the column offset between points BEG & END.

Typically these will be on the same line but this isn't a requirement."
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
  "Execute BODY, replacing `last-command' with it's :substitute if defined."
  (declare (indent 0))
  `(let ((last-command
          (or (and (symbolp last-command) (meep-command-prop-get last-command :substitute))
              ;; Keep existing binding.
              last-command)))
     ,@body))


;; ---------------------------------------------------------------------------
;; Public Variables/Constants

;; This value only exists in order to temporarily override it.
(defconst meep-mark-set-on-motion-override nil
  "This constant exists so it's possible to let-bind the value to t.

Used so a motion can be repeated without setting the mark.")


;; ---------------------------------------------------------------------------
;; Motion: Symbol/Word
;;
;; Command properties:
;; commands may have a `meep' property, this is expected to be a P-list of properties.
;;
;; :mark-on-motion
;;    - t: Mark on motion.
;;    - 'adjust: Adjust the previous motion.
;;
;;      This is used so a motion can be adjusted,
;;      without breaking the chain of commands used to repeat an action.
;;      So it's possible to perform a motion & any number of adjustments before an edit-command.
;;
;;      When repeating the motion, adjustments and edit will all be repeated.
;;      Single character motion commands take advantage of this.
;;
;;    - nil: don't mark on motion (same as missing).
;;
;; :mark-on-motion-no-repeat
;;    - t: These motions that should not be repeated such as search.
;;      (used by repeat-fu).
;; :digit-repeat
;;    - t: The command is a digit command.
;;
;;      This command can repeat other commands multiple times.

(defun meep--mark-on-motion-set (pos always)
  "Set the mark to POS the region is not active.
When ALWAYS is non-nil, mark-on-motion even if the cursor didn't move."
  (when (and meep-mark-set-on-motion
             ;; Has motion, or always.
             (or always (/= pos (point)))
             ;; Has no region.
             (not (region-active-p))
             ;; Allow numeric commands to adjust the motion,
             ;; without moving the mark.
             (null meep-mark-set-on-motion-override))
    (setq deactivate-mark t)
    (meep--set-marker pos)))

(defmacro meep--with-mark-on-motion-maybe-set (&rest body)
  "Run the given BODY, motion will set the mark."
  (declare (indent 0))
  (let ((pos-orig (make-symbol "pos-orig")))
    `(let ((,pos-orig (point)))
       (prog1 (progn
                ,@body)
         ;; Some extra checks could be added here,
         ;; reserve for the function call to prevent code-bloat.
         (meep--mark-on-motion-set ,pos-orig nil)))))

(defmacro meep--with-mark-on-motion-always-set (&rest body)
  "Run the given BODY, motion will always set the mark."
  (declare (indent 0))
  (let ((pos-orig (make-symbol "pos-orig")))
    `(let ((,pos-orig (point)))
       (prog1 (progn
                ,@body)
         ;; Some extra checks could be added here,
         ;; reserve for the function call to prevent code-bloat.
         (meep--mark-on-motion-set ,pos-orig t)))))

(defun meep--maintain-line-based-region (pos-orig mrk-orig)
  "Internal utility to maintain line based selection.
POS-ORIG & MRK-ORIG define the original region."
  (when (and
         ;; Check line based selection is in use.
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
  "Run the given BODY, motion will set the mark."
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

(defun meep--mark-on-motion-maybe-activate ()
  "Activate the region in preparation for a command to use the active region."
  (when (and meep-mark-set-on-motion (not (region-active-p)))
    (let ((local-last-command (meep--last-command)))
      (when (and (symbolp local-last-command)
                 (meep-command-is-mark-set-on-motion-any local-last-command))
        (setq deactivate-mark nil)
        (activate-mark t))))
  nil)

(defun meep--mark-on-motion-maybe-activate-as-bounds ()
  "A version of `meep--mark-on-motion-maybe-activate' returning bounds or nil.
The bounds represent the region that would have been activated.
this should be used in situations commands do not result in a user visible region."
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
  "Return the equivalent region beginning & end.
These are the values that *would* be set if the motion
were to be made into the active region."
  (when (and meep-mark-set-on-motion (not (region-active-p)))
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
  "Return the region beginning & end or motion bounds."
  (cond
   ((region-active-p)
    (cons (region-beginning) (region-end)))
   (t
    (meep--mark-on-motion-bounds))))

(defun meep--calc-beginning-of-next-thing (thing n)
  "Move to the beginning of the next THING N times."
  ;; Move to the start of the next thing.
  ;; Otherwise the point moves to the end.
  (let ((pos-orig (point)))
    (save-excursion
      (when-let* ((bounds (bounds-of-thing-at-point thing)))
        (goto-char (cdr bounds)))
      (forward-thing thing n)
      (cond
       ((eq pos-orig (point))
        ;; Nothing moved.
        nil)
       (t
        (when-let* ((bounds (bounds-of-thing-at-point thing)))
          (goto-char (car bounds)))
        ;; Unlikely but theoretically possible jumping to the start
        ;; of the bounds could cause the point not to move.
        (cond
         ((eq pos-orig (point))
          nil)
         (t
          (point))))))))

(defun meep--calc-end-of-prev-thing (thing n)
  "Move to the end of the previous THING N times."
  ;; Move to the end of the previous thing.
  ;; Otherwise the point moves to the end.
  (let ((pos-orig (point)))
    (save-excursion
      (when-let* ((bounds (bounds-of-thing-at-point thing)))
        (goto-char (car bounds)))
      (forward-thing thing (- n))
      (cond
       ((eq pos-orig (point))
        ;; Nothing moved.
        nil)
       (t
        (when-let* ((bounds (bounds-of-thing-at-point thing)))
          (goto-char (cdr bounds)))
        ;; Unlikely but theoretically possible jumping to the end
        ;; of the bounds could cause the point not to move.
        (cond
         ((eq pos-orig (point))
          nil)
         (t
          (point))))))))

(defun meep--move-thing-prev-next-end-impl (thing n)
  "Implementation for next/previous THING, move N times."
  (forward-thing thing n)
  nil)

;;;###autoload
(defun meep-move-symbol-prev (arg)
  "Move point to the beginning of the previous symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-thing-prev-next-end-impl 'symbol (- arg))))

;;;###autoload
(defun meep-move-symbol-prev-end (arg)
  "Move to the end of the previous symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep--move-thing-prev-next-end-impl 'symbol (- arg)))
     (t
      (when-let* ((pos (meep--calc-end-of-prev-thing 'symbol arg)))
        (goto-char pos))
      nil))))

;;;###autoload
(defun meep-move-symbol-next-end (arg)
  "Move to the end of the next symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-thing-prev-next-end-impl 'symbol arg)))

;;;###autoload
(defun meep-move-symbol-next (arg)
  "Move point to the beginning next symbol, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep--move-thing-prev-next-end-impl 'symbol arg))
     (t
      (when-let* ((pos (meep--calc-beginning-of-next-thing 'symbol arg)))
        (goto-char pos))))))

;;;###autoload
(defun meep-move-word-prev (arg)
  "Move point to the beginning of the previous word, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-thing-prev-next-end-impl 'word (- arg))))

;;;###autoload
(defun meep-move-word-next-end (arg)
  "Move to the end of the next word ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-thing-prev-next-end-impl 'word arg)))

;;;###autoload
(defun meep-move-word-prev-end (arg)
  "Move to the end of the previous word ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep--move-thing-prev-next-end-impl 'word (- arg)))
     (t
      (when-let* ((pos (meep--calc-end-of-prev-thing 'word arg)))
        (goto-char pos))
      nil))))

;;;###autoload
(defun meep-move-word-next (arg)
  "Move point to the beginning of the next word, ARG times."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (cond
     ((< arg 0)
      (meep--move-thing-prev-next-end-impl 'word arg))
     (t
      (when-let* ((pos (meep--calc-beginning-of-next-thing 'word arg)))
        (goto-char pos))
      nil))))


;; ---------------------------------------------------------------------------
;; Motion: Same Syntax

(defun meep--move-same-syntax-impl (n skip-single skip-space or-thing)
  "Move forward over N syntax-spans, a negative argument skips backwards.
When SKIP-SINGLE isn't nil, initial single motion isn't counted as a step for N.
SKIP-SPACE a cons cell additional space skipping before & after the motion.
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
      (while (and (not (bobp))
                  (not
                   (zerop
                    (prog1 n
                      (meep--decf n)))))
        (syntax-ppss)
        (setq syn (syntax-after (1- (point))))
        (while (and (not (bobp)) (equal syn (syntax-after (1- (point)))))
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
      (while (and (not (eobp))
                  (not
                   (zerop
                    (prog1 n
                      (meep--decf n)))))
        (syntax-ppss)
        (setq syn (syntax-after (point)))
        (while (and (not (eobp)) (equal syn (syntax-after (point))))
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
  "Move back a syntax-spans ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-next (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl (- arg) t (cons nil nil) nil)))))

;;;###autoload
(defun meep-move-same-syntax-next (arg)
  "Move to the end of the next word ARG times."
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
;; Skips over the same syntax or entire symbols.

;;;###autoload
(defun meep-move-same-syntax-or-symbol-prev (arg)
  "Move back a syntax-spans or symbols ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-or-symbol-next (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl (- arg) t (cons nil nil) 'symbol)))))

;;;###autoload
(defun meep-move-same-syntax-or-symbol-next (arg)
  "Move forward a syntax-spans or symbols ARG times."
  (interactive "^p")
  (cond
   ((< arg 0)
    (meep-move-same-syntax-or-symbol-prev (- arg)))
   (t
    (meep--with-mark-on-motion-maybe-set
      (meep--move-same-syntax-impl arg t (cons nil nil) 'symbol)))))


;; ---------------------------------------------------------------------------
;; Motion: Same Syntax & Space
;;
;; Skips over the same syntax with changes to behavior for surrounding space,
;; where space at the bounds of text is skipped over, matching
;; how this is handled for skipping words & symbols.

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
  "Move to the beginning of the next syntax-and-space, ARG times."
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
  "Implementation for line beginning/end, using N."
  (cond
   ((< n 0)
    (beginning-of-line))
   (t
    (end-of-line))))

;;;###autoload
(defun meep-move-line-beginning (arg)
  "Move to the beginning of the current line end.
Moves to the end when ARG is negative."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-beginning-end-impl (- arg))))

;;;###autoload
(defun meep-move-line-end (arg)
  "Move to the end of the current line end.
Moves to the beginning when ARG is negative."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-beginning-end-impl arg)))

(defun meep--move-line-non-space-beginning-end-impl (n)
  "Implementation for non-space line beginning/end, using N."
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
  "Move the the beginning of the line, ignoring end of line white-spaces.
A negative ARG moves to the end."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-non-space-beginning-end-impl (- arg))))

;;;###autoload
(defun meep-move-line-non-space-end (arg)
  "Move the the end of the line, ignoring end of line white-spaces.
A negative ARG moves to the beginning."
  (interactive "^p")
  (meep--with-mark-on-motion-maybe-set
    (meep--move-line-non-space-beginning-end-impl arg)))

(defun meep--move-line-wrapper (n &optional noerror)
  "Call `line-move' N, with `last-command' set to respect the goal column.
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
                ;; even after horizon motion which feels too constrained.
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
  (left-char arg))

;;;###autoload
(defun meep-move-char-next (arg)
  "Move to the next character ARG times."
  (interactive "^p")
  ;; Intentionally don't include in "mark-on-motion",
  ;; allow adjustments after motion.
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

;; Useful, for e.g. `/** comment */` or `/// comment.`
(defcustom meep-move-comment-skip-repeated t
  "When navigating comment bounds, skip repeated characters."
  :type 'boolean)

(defun meep--goto-comment-or-string-bounds (dir)
  "Move point to the beginning/end of the comment or string.

When DIR is -1, the beginning, 1 the end.
Return t when stepping out of string or comment bounds."
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
  (cond
   ;; Modes that use {} brackets.
   ;; For now, assume if "{}" are brackets.
   ;; Then these are the "main" brackets to use for navigation,
   ;; true for C/C++/Java ... etc.
   ((and (eq (char-syntax ?\{) ?\() ; Check both match.
         (eq (char-syntax ?\}) ?\)))
    (list ?{ ?}))
   (t
    nil)))

(defun meep--jump-next-sexp-step-over-impl ()
  "Step over the next SEXP or return nil."
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
  (let ((pos nil)
        (changed nil))
    (cond
     ((and step-over (meep--jump-next-sexp-step-over-impl))
      (setq changed t))
     (t
      (save-excursion
        ;; When in a comment or string, skip out of it.
        (meep--goto-comment-or-string-bounds 1)
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
  (setq n (- n))
  (let ((pos nil)
        (changed nil))
    (cond
     ((and step-over (meep--jump-prev-sexp-step-over-impl))
      (setq changed t))
     (t
      (save-excursion
        ;; When in a comment or string, skip out of it.
        (meep--goto-comment-or-string-bounds -1)
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

(defun meep--is-point-after-bracket-close (bracket-chars)
  "Return t if the point is after a closed BRACKET-CHARS."
  (cond
   ((and (save-match-data (looking-back "\\s)" (pos-bol)))
         (or (null bracket-chars) (memq (char-before (point)) bracket-chars)))
    t)
   (t
    nil)))

(defun meep--is-point-after-bracket-open (bracket-chars)
  "Return t if the point is after an open BRACKET-CHARS."
  (cond
   ((and (save-match-data (looking-back "\\s(" (pos-bol)))
         (or (null bracket-chars) (memq (char-before (point)) bracket-chars)))
    t)
   (t
    nil)))

(defun meep--is-point-before-bracket-close (bracket-chars)
  "Return t if the point is before a closed BRACKET-CHARS."
  (cond
   ((and (looking-at-p "\\s)") (or (null bracket-chars) (memq (char-after (point)) bracket-chars)))
    t)
   (t
    nil)))

(defun meep--is-point-before-bracket-open (bracket-chars)
  "Return t if the point is before an open BRACKET-CHARS."
  (cond
   ((and (looking-at-p "\\s(") (or (null bracket-chars) (memq (char-after (point)) bracket-chars)))
    t)
   (t
    nil)))

(defun meep--move-by-sexp-any-impl (n step-over)
  "Jump to the next/previous SEXP by N.
When STEP-OVER is non-nil don't step into nested blocks."
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
                               (not (memq (char-after (point)) bracket-chars)))
                              (t
                               nil))))
                        (unless keep-searching
                          (setq pos-step (point)))
                        keep-searching))))

        (when pos-step
          (goto-char pos-step)
          (when (meep--is-point-before-bracket-open bracket-chars)
            (forward-char 1))
          (unless (eq pos-init pos-found)
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
                               (not (memq (char-before (point)) bracket-chars)))
                              (t
                               nil))))
                        (unless keep-searching
                          (setq pos-step (point)))
                        keep-searching))))

        (when pos-step
          (goto-char pos-step)
          (when (meep--is-point-after-bracket-close bracket-chars)
            (forward-char -1))
          (unless (eq pos-init pos-found)
            (setq pos-found (point)))))))
    pos-found))

;;;###autoload
(defun meep-move-by-sexp-any-next (arg)
  "Jump to the next SEXP.
Step ARG times or 1 when default."
  (interactive "^p")
  (when-let* ((pos-found (meep--move-by-sexp-any-impl arg nil)))
    (meep--with-mark-on-motion-maybe-set
      (goto-char pos-found))))

;;;###autoload
(defun meep-move-by-sexp-any-prev (arg)
  "Jump to the previous SEXP.
Step ARG times or 1 when default."
  (interactive "^p")
  (when-let* ((pos-found (meep--move-by-sexp-any-impl (- arg) nil)))
    (meep--with-mark-on-motion-maybe-set
      (goto-char pos-found))))


(defvar-local meep-move-by-sexp-over-depth nil
  "The target depth when moving over S-expressions.
Used to maintain the depth even when moving over causes
navigation to move to outer scope.

Only used between successive
`meep-move-by-sexp-over-next' & `meep-move-by-sexp-over-prev' calls.")

;; Internal, avoids multiple similar S-expression lookups,
;; the cache is valid as long as `meep--sexp-depth-calc' calls
;; are done without any buffer edits.
;;
;; - Numbers are ordered small to large.
;; - The place represents the depth (1 based).
;;   Where the first item has depth 1, the second depth 2 and so on.
(defvar-local meep--sexp-depth-calc-cache nil)

(defun meep--sexp-depth-calc ()
  "Return the S-expression depth."
  (let* ((bracket-chars (meep--jump-brackets-from-mode))
         (cache-prev meep--sexp-depth-calc-cache)
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

    (setq meep--sexp-depth-calc-cache (cdr cache-cell))
    depth))

(defun meep--move-by-sexp-over-last-command-check ()
  "Return non-nil when the `last-command' moved over an S-expression."
  (memq last-command (list 'meep-move-by-sexp-over-next 'meep-move-by-sexp-over-prev)))

;;;###autoload
(defun meep-move-by-sexp-over-next (arg)
  "Move next over the SEXP ARG times."
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
  "Move previous over the SEXP ARG times."
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
  "Move up & out of the expression.
Move forward when ARG is positive, otherwise backwards."
  (let ((pos-init (point))
        (pos-found (point))
        (bracket-chars (meep--jump-brackets-from-mode)))
    (save-excursion
      (cond
       ((meep--is-point-after-bracket-open bracket-chars)
        (forward-char -1))
       ((meep--is-point-before-bracket-close bracket-chars)
        (forward-char 1)))

      ;; TODO: only get opening brackets.
      (let ((keep-looking t))
        (while keep-looking
          (backward-up-list (abs arg) nil t)
          (when (or (null bracket-chars) (memq (char-after (point)) bracket-chars))
            (setq keep-looking nil))))

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
        (setq pos-found (point))))

    (unless (eq pos-init pos-found)
      (meep--with-mark-on-motion-maybe-set
        (goto-char pos-found)))))

;;;###autoload
(defun meep-move-by-sexp-out-prev (&optional arg)
  "Jump to the previous SEXP, jumping out of the current expression.
Step ARG times or 1 when default."
  (interactive "^p")
  (meep--move-by-sexp-out-impl (- arg)))

;;;###autoload
(defun meep-move-by-sexp-out-next (&optional arg)
  "Jump to the next SEXP, jumping into the next expression.
Step ARG times or 1 when default."
  (interactive "^p")
  (meep--move-by-sexp-out-impl arg))

;;;###autoload
(defun meep-move-matching-bracket-outer ()
  "Jump to the matching outer bracket.
When not at the bounds, jump the start (when enclosed in brackets).

Return non-nil when the point was moved."
  (interactive "^")
  (meep--with-mark-on-motion-maybe-set
    (let ((result nil)
          (interactive t)
          ;; It only makes sense to use an argument of 1
          ;; when jumping to matching items.
          (arg 1)
          (pos-orig (point))
          ;; Jump the start of the string, prevents failure to jump out of the string.
          (pos-outer (nth 8 (syntax-ppss))))
      (cond
       ((meep--is-point-before-bracket-open nil)
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (forward-sexp arg interactive)
                 (point))))
          (setq result t)
          (goto-char pos)))
       ((meep--is-point-after-bracket-close nil)
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (forward-sexp (- arg) interactive)
                 (point))))
          (setq result t)
          (goto-char pos))))
      ;; As a handy fallback, jump up to the opening parent parenthesis.
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
When not at the bounds, jump the start (when enclosed in brackets).

Return non-nil when the point was moved."
  (interactive "^")
  (meep--with-mark-on-motion-maybe-set
    (let ((result nil)
          (interactive t)
          ;; It only makes sense to use an argument of 1
          ;; when jumping to matching items.
          (arg 1)
          (pos-orig (point))
          ;; Jump the start of the string, prevents failure to jump out of the string.
          (pos-outer (nth 8 (syntax-ppss))))
      (cond
       ((meep--is-point-after-bracket-open nil)
        (let ((pos
               (save-excursion
                 (when pos-outer
                   (goto-char pos-outer))
                 (forward-char -1)
                 (forward-sexp arg interactive)
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
                 (forward-sexp (- arg) interactive)
                 (forward-char 1)
                 (point))))
          (setq result t)
          (goto-char pos))))
      ;; As a handy fallback, jump up to the opening parent parenthesis.
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
  "Return the outer bounds for the comments at point or nil when not found."
  (let ((state (syntax-ppss)))
    (unless (nth 4 state)
      ;; Don't step back onto the previous line as it causes
      ;; syntax with single line comments consider the blank
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
  (and (null (nth 4 state)) (nth 8 state)))

(defun meep--bounds-at-point-for-string-outer ()
  "Return the outer bounds for the string at point or nil when not found."
  (let ((pos-orig (point)))
    (when-let* ((start
                 (or (meep--syntax-state-is-string (syntax-ppss))
                     (and (not (bobp))
                          (meep--syntax-state-is-string
                           (save-excursion (syntax-ppss (1- pos-orig)))))
                     (and (not (eobp))
                          (meep--syntax-state-is-string
                           (save-excursion (syntax-ppss (1+ pos-orig))))))))
      (when (<= start pos-orig)
        (save-excursion
          (goto-char start)
          (forward-sexp)
          (when (<= pos-orig (point))
            (cons start (point))))))))

(defun meep--bounds-match-at-end-points (bounds beg-re end-re)
  "Return t if BOUNDS begins and ends with BEG-RE & END-RE."
  (save-excursion
    (cond
     ((and (or (null beg-re)
               (progn
                 (goto-char (car bounds))
                 (looking-at-p beg-re)))
           (or (null end-re)
               (progn
                 (goto-char (cdr bounds))
                 (save-match-data (looking-back beg-re (car bounds))))))
      t)
     (t
      nil))))

(defun meep--bounds-equal-at-end-points (bounds beg-str end-str)
  "Return t if BOUNDS begins and ends with BEG-STR & END-STR."
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

(defun meep--bounds-at-point-for-comment-inner-guess (bounds)
  "Contract BOUNDS based on the mode."
  (declare (important-return-value t))
  (let* ((result nil))
    ;; TODO: split out mode-specific settings into customizable variables.
    ;; Since it's not good to hard code modes into MEEP.
    ;; For now, do a "reasonable" job at supporting most popular languages.
    (cond
     ;; Defined in `cc-defs'.
     ((or
       ;; This looks to be the most generic way to check for a C-like mode.
       (bound-and-true-p c-buffer-is-cc-mode)
       ;; Other modes that support both C++ and C style comment blocks.
       (derived-mode-p
        (list 'dart-mode 'javascript-mode 'kotlin-mode 'php-mode 'scala-mode 'rust-mode)))
      (cond
       ((meep--bounds-equal-at-end-points bounds "/*" "*/")
        (setq result (cons (+ (car bounds) 2) (- (cdr bounds) 2))))
       ((meep--bounds-equal-at-end-points bounds "//" "")
        (setq result (cons (+ 2 (car bounds)) (cdr bounds))))))
     ;; C /* ... */ style only.
     ((derived-mode-p 'css-mode)
      (cond
       ((meep--bounds-equal-at-end-points bounds "/*" "*/")
        (setq result (cons (+ (car bounds) 2) (- (cdr bounds) 2))))))
     ;; C++ // ... style only.
     ((derived-mode-p 'go-mode)
      (cond
       ((meep--bounds-equal-at-end-points bounds "//" "")
        (setq result (cons (+ 2 (car bounds)) (cdr bounds))))))
     ((derived-mode-p 'lua-mode)
      (cond
       ((meep--bounds-equal-at-end-points bounds "--[[" "]]")
        (setq result (cons (+ (car bounds) 4) (- (cdr bounds) 2))))
       ((meep--bounds-equal-at-end-points bounds "--" "")
        (setq result (cons (+ 2 (car bounds)) (cdr bounds))))))
     ((derived-mode-p 'cmake-mode)
      (cond
       ;; Actual comment is #[[ ]] however: #[=[ ]=] (with arbitrary "=")
       ;; is allowed, rely on white space skipping to skip over these characters.
       ((meep--bounds-equal-at-end-points bounds "#[" "]")
        (setq result (cons (+ (car bounds) 3) (- (cdr bounds) 3))))))
     ((derived-mode-p 'haskell-mode)
      (cond
       ((meep--bounds-equal-at-end-points bounds "{-#" "#-}")
        (setq result (cons (+ (car bounds) 3) (- (cdr bounds) 3))))
       ((meep--bounds-equal-at-end-points bounds "{-" "-}")
        (setq result (cons (+ (car bounds) 2) (- (cdr bounds) 2))))))
     ((derived-mode-p (list 'html-mode 'xml-mode))
      (cond
       ((meep--bounds-equal-at-end-points bounds "<!--" "-->")
        (setq result (cons (+ (car bounds) 4) (- (cdr bounds) 3))))))
     ((derived-mode-p 'ruby-mode)
      (cond
       ((meep--bounds-equal-at-end-points bounds "=begin" "=end")
        (setq result (cons (+ 6 (car bounds)) (- (cdr bounds) 4))))))
     ((derived-mode-p 'pascal-mode)
      (cond
       ((meep--bounds-equal-at-end-points bounds "(*" "*)")
        (setq result (cons (+ (car bounds) 2) (- (cdr bounds) 2))))
       ((meep--bounds-equal-at-end-points bounds "{" "}")
        (setq result (cons (+ (car bounds) 2) (- (cdr bounds) 2)))))))
    result))

(defun meep--bounds-at-point-for-comment-inner ()
  "Return the inner bounds for the comments at point or nil when not found."
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
          (let ((bounds-inner (meep--bounds-at-point-for-comment-inner-guess (cons beg end))))
            (when bounds-inner
              (setq beg (car bounds-inner))
              (setq end (cdr bounds-inner)))))

        ;; Skip repeated chars: "/***** "
        ;; TODO: check if the characters are space (seems very unlikely).
        ;; but could mess with `meep-move-comment-skip-space'.
        (when meep-move-comment-skip-repeated
          (unless (eq beg (car bounds-outer))
            (save-excursion
              (goto-char beg)
              ;; TODO: char-before could be nil if at beginning of buffer.
              (let ((ch (char-to-string (char-before beg))))
                (skip-chars-forward ch end)
                (unless (eq (point) end)
                  (setq beg (point))))))
          (unless (eq end (cdr bounds-outer))
            (save-excursion
              (goto-char end)
              ;; TODO: char-after could be nil if at the end of the of buffer.
              (let ((ch (char-to-string (char-after end))))
                (skip-chars-backward ch beg)
                (unless (eq (point) beg)
                  (setq end (point)))))))

        (unless (and (eq beg (car bounds-outer)) (eq end (cdr bounds-outer)))
          (setq result (cons beg end))))

      (when meep-move-comment-skip-space
        (when result
          (let ((skip "[:blank:]"))
            (setq result (meep--bounds-contract-by-chars-non-empty result skip skip))))))

    (or result bounds-outer)))

(defun meep--bounds-at-point-for-string-inner ()
  "Return the inner bounds for the string at point or nil when not found."
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
When not at the bounds, jump the start (when in a string/comment).

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
        ;; As a handy fallback, jump up to the opening parent parenthesis.
        (when (eq pos-orig (point))
          (goto-char (car bounds))))
      result)))

;;;###autoload
(defun meep-move-matching-syntax-inner ()
  "Move to the inner matching sting/comment syntax.
When not at the bounds, jump the start (when in a string/comment).

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
        ;; As a handy fallback, jump up to the opening parent parenthesis.
        (when (eq pos-orig (point))
          (goto-char (car bounds))))
      result)))

;;;###autoload
(defun meep-move-matching-contextual-outer ()
  "Move to the matching character.
When not at the bounds, jump the start."
  (interactive "^")
  (or (meep-move-matching-syntax-outer) (meep-move-matching-bracket-outer)))

;;;###autoload
(defun meep-move-matching-contextual-inner ()
  "Move to the matching character.
When not at the bounds, jump the start."
  (interactive "^")
  (or (meep-move-matching-syntax-inner) (meep-move-matching-bracket-inner)))


;; ---------------------------------------------------------------------------
;; Motion: Find & Till

(defvar meep--move-find-last-char nil
  "The last character used to find.")

(defun meep--move-find-impl (n ch is-till)
  "Find/Till implementation.
N the number of times to find.
CH is the character to find.
IS-TILL when non-nil, search up until the character."
  (let* ((case-fold-search nil)
         (ch-str
          (cond
           ((eq ch 13)
            "\n")
           (t
            (char-to-string ch))))

         ;: Note that limiting could be optional.
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
       ((not end)
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

;; Avoids repeating the error message all over the place.
(defun meep--move-find-last-char-or-message ()
  "Return the last character or raise an error."
  (cond
   (meep--move-find-last-char
    meep--move-find-last-char)
   (t
    (message "No last character is set")
    nil)))

;;;###autoload
(defun meep-move-find-char-on-line-at-next (arg ch)
  "Find the next ARG char CH, read from mini-buffer."
  (interactive "^p\ncFind Next:")
  (meep--move-find-impl arg ch nil))

;;;###autoload
(defun meep-move-find-char-on-line-at-prev (arg ch)
  "Find the previous ARG char CH, read from mini-buffer."
  (interactive "^p\ncFind Prev:")
  (meep--move-find-impl (- arg) ch nil))

;;;###autoload
(defun meep-move-find-char-on-line-till-next (arg ch)
  "Find till the next ARG char CH, read from mini-buffer."
  (interactive "^p\ncTill Next:")
  (meep--move-find-impl arg ch t))

;;;###autoload
(defun meep-move-find-char-on-line-till-prev (arg ch)
  "Find till the previous ARG CH, char read from mini-buffer."
  (interactive "^p\ncTill Prev:")
  (meep--move-find-impl (- arg) ch t))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-at-next (arg)
  "Repeat find ARG chars forwards."
  (interactive "^p")
  (meep--move-find-impl arg (meep--move-find-last-char-or-message) nil))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-at-prev (arg)
  "Repeat find ARG chars backwards."
  (interactive "^p")
  (meep--move-find-impl (- arg) (meep--move-find-last-char-or-message) nil))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-till-next (arg)
  "Repeat find ARG chars forwards."
  (interactive "^p")
  (meep--move-find-impl arg (meep--move-find-last-char-or-message) t))

;;;###autoload
(defun meep-move-find-char-on-line-repeat-till-prev (arg)
  "Repeat find ARG chars backwards."
  (interactive "^p")
  (meep--move-find-impl (- arg) (meep--move-find-last-char-or-message) t))


;; ---------------------------------------------------------------------------
;; Motion: Bounds (Implementation)

(defun meep--bounds-contract-by-chars (bounds skip-beg skip-end)
  "Contract BOUNDS by SKIP-BEG & SKIP-END."
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
  "Contract BOUNDS by SKIP-BEG & SKIP-END."
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
  "Bounds of visual line (contract to INNER when true)."
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
  "Bounds of sentence (contract to INNER when true)."
  (let ((bounds (bounds-of-thing-at-point 'sentence)))
    (when bounds
      (when inner
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
          (setcdr bounds (point)))))
    bounds))

(defun meep--bounds-of-paragraph (inner)
  "Bounds of paragraph (contract to INNER when true)."
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (when bounds
      (when inner
        (let ((skip "[:blank:]\r\n"))
          (setq bounds (meep--bounds-contract-by-chars bounds skip skip)))))
    bounds))

(defun meep--move-to-bounds-endpoint (bounds n)
  "Move to the start/end of BOUNDS (start when N is negative)."
  ;; Note that it's important to always set the mark because unlike a typical motion,
  ;; we want to be able to use `meep-exchange-point-and-mark-motion' even if this
  ;; move-to-bounds action happens not to move the point, see: #8.
  (meep--with-mark-on-motion-always-set
    (cond
     ((< n 0)
      (goto-char (car bounds)))
     (t
      (goto-char (cdr bounds))))))

(defun meep--move-to-bounds-of-thing (thing n)
  "Implement move to bounds of THING (start when N is negative)."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds
      (meep--move-to-bounds-endpoint bounds n))))


;; ---------------------------------------------------------------------------
;; Motion: Bounds

;;;###autoload
(defun meep-move-to-bounds-of-sentence (arg &optional inner)
  "Move to the sentences start/end (start when ARG is negative).
INNER to move to inner bound."
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
  "Move to the inner sentences start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-sentence arg t))

;;;###autoload
(defun meep-move-to-bounds-of-paragraph (arg &optional inner)
  "Move to the paragraph start/end (start when ARG is negative).
INNER to move to inner bound."
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
INNER to move to inner bound."
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
INNER to move to inner bound."
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
(defun meep-move-to-bounds-of-defun (arg &rest inner)
  "Move to the function start/end (start when ARG is negative).
INNER to move to inner bound."
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
INNER to move to inner bound."
  (interactive "^p")
  (let ((bounds (cons (pos-bol) (pos-eol))))
    (cond
     (bounds
      (when inner
        (let ((skip "[:blank:]"))
          (setq bounds (meep--bounds-contract-by-chars bounds skip skip))))
      (meep--move-to-bounds-endpoint bounds arg))
     (t
      (message "Not found: bounds of line")
      nil))))

;;;###autoload
(defun meep-move-to-bounds-of-line-inner (arg)
  "Move to the inner line start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-line arg t))

;;;###autoload
(defun meep-move-to-bounds-of-visual-line (arg &optional inner)
  "Move to the visual-line start/end (start when ARG is negative).
INNER to move to inner bound."
  (interactive "^p")
  (let ((bounds (meep--bounds-of-visual-line inner)))
    ;; No need to check for nil.
    (meep--move-to-bounds-endpoint bounds arg)))
;;;###autoload
(defun meep-move-to-bounds-of-visual-line-inner (arg)
  "Move to the inner visual-line start/end (start when ARG is negative)."
  (interactive "^p")
  (meep-move-to-bounds-of-visual-line arg t))

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
    (?\. meep-move-to-bounds-of-sentence-inner "sentence inner")
    (?> meep-move-to-bounds-of-sentence "sentence"))
  "List of commands for bounds movement. Each element is (key function description)."
  :type
  '(repeat
    (list
     :tag
     "Command"
     (character :tag "Key")
     (function :tag "Function")
     (string :tag "Description"))))

(defun meep--move-bounds-of-thing-impl (n)
  "Initiate a bounds motion, forward when N is positive.
When INNER is non-nil move to the outer bounds."
  (let ((km nil)
        (info-text
         (mapcar
          (lambda (cmd) (format "%s: %s" (string (nth 0 cmd)) (nth 2 cmd))) meep-bounds-commands)))
    (setq km (make-sparse-keymap))
    (when (< n 0)
      (setq prefix-arg -1))
    (dolist (cmd (reverse meep-bounds-commands))
      (keymap-set km (string (nth 0 cmd)) (nth 1 cmd)))
    (set-transient-map km
                       nil nil
                       (concat
                        (format "Jump to the %s, of" (or (and (< n 0) "beginning") "end"))
                        ": %k or any other to exit\n"
                        (mapconcat #'identity info-text ", ")))))

;;;###autoload
(defun meep-move-to-bounds-of-thing-beginning (arg)
  "Move to inner bounds of thing (begging).
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
  "Enable the active region."
  (interactive)
  (unless (region-active-p)
    ;; This may have been set, clear it if it was.
    (setq meep-state-region-elem nil)
    ;; Begin selecting (set the mark to the points location).
    ;; Use `meep-exchange-point-and-mark' to activation the region with the old mark.
    (set-mark (point))
    (activate-mark t)))

;;;###autoload
(defun meep-region-disable ()
  "Disable the active region.

The mark is not moved, the region can be restored
via `meep-exchange-point-and-mark'."
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
(defun meep-exchange-point-and-mark ()
  "Exchange the point and mark, activating the region."
  (interactive)
  ;; This will activate the selection if it's not already selected,
  ;; it allows re-selecting pasted text for example.
  (exchange-point-and-mark))

(defun meep--last-motion-calc-whole-mark-pos ()
  "When a partial motion command has been made."
  (let ((local-last-command (meep--last-command))
        (local-last-prefix-arg (meep--last-prefix-arg))
        (local-mrk (mark))
        (new-mrk nil)
        (prefix "exchange-point-and-mark-motion"))
    (cond
     ((null local-mrk)
      (message "%s: failed, no mark found, the mark is expected to be set by: %S"
               prefix
               local-last-command)
      nil)
     ((null local-last-command)
      (message "%s: failed, no last-command found" prefix)
      nil)
     ((not (symbolp local-last-command))
      (message "%s: failed, the last-command must be a symbol, not %S"
               prefix
               (type-of local-last-command))
      nil)
     ((eq local-last-command 'meep-exchange-point-and-mark-motion)
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
(defun meep-exchange-point-and-mark-motion ()
  "Exchange the point and mark, activating the region."
  (interactive)
  (let ((last-motion-info (meep--last-motion-calc-whole-mark-pos)))
    (cond
     (last-motion-info
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

- When the region on a single line:
  The text after point implies the selection.
- When a line-wise region is used:
  The same number of lines after the point is used (ignoring line length).
- When a rectangle-wise region is used:
  The text after & lines below are used to create the implied selection."
  :type 'boolean)

(defun meep--range-list-as-marker-list (ranges)
  "Create a list of markers from RANGES of integer ranges."
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

When IS-LINE-WISE is non-nil, the secondary selection represents whole lines,
this impacts `meep-region-swap-imply-region' causing the implied region to extend
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
                  (user-error "Rectangle line count mismatch for implied region (%d & %d)"
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
      (user-error "Rectangle line count mismatch (%d & %d)" len-a len-b))

    ;; We _could_ subtract one region from another - to prevent overlap,
    ;; or handle overlap as part of the swapping logic.
    ;; Raise an error as this seems like enough of corner case.
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

          ;; Unlikely, but may as well skip redundant.
          (unless (string-equal text-a text-b)
            (meep--replace-in-region text-b (car range-a) (cdr range-a))
            (meep--replace-in-region text-a (car range-b) (cdr range-b)))

          ;; Reached the end, ensure the mark & secondary selection are updated.
          (unless line-ranges-a

            (cond
             (is-swap
              (setq secondary-end-next (marker-position (cdr range-a)))
              (setq region-end-next (marker-position (cdr range-b))))
             (t
              (setq secondary-end-next (marker-position (cdr range-b)))
              (setq region-end-next (marker-position (cdr range-a))))))

          ;; Queue makers to be cleared.
          (set-marker (car range-a) nil)
          (set-marker (cdr range-a) nil)
          (set-marker (car range-b) nil)
          (set-marker (cdr range-b) nil))))

    ;; Only move the end for both region & secondary selection.
    (cond
     (is-forward
      (goto-char region-end-next))
     (t
      (meep--set-marker region-end-next)))

    (move-overlay
     mouse-secondary-overlay (overlay-start mouse-secondary-overlay) secondary-end-next)))

;;;###autoload
(defun meep-region-swap ()
  "Swap the contents of the primary & secondary region.

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
    ;; Ensure the region is line based (even if not active).
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
Consecutive

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

    (let ((is-forward (eq (point) (region-end)))
          (beg (region-beginning))
          (end (region-end))

          ;; Local functions.
          (is-line-empty-fn
           (lambda ()
             (save-excursion
               (beginning-of-line)
               (looking-at-p "[[:blank:]]*$")))))

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

            ;; If the first line is empty, skip all empty space.
            (when (funcall is-line-empty-fn)
              (line-move 1 t)
              (while (not (funcall is-line-empty-fn))
                (line-move 1 t)))

            (let ((x (point)))
              (while (not (funcall is-line-empty-fn))
                (setq x (point))
                (line-move 1 t))
              (goto-char x)
              (goto-char (pos-eol)))

            ;; Ensure we always step over the last newline.
            ;; This is (among other reasons)
            ;; so it's possible to select a line and cut it.
            ;; It also has some minor added benefits.
            ;; - The cursor doesn't scroll of the RHS of the screen
            ;;   for long lines.
            ;; - Moving the cursor up-down can stick to column zero.
            (unless (funcall is-line-empty-fn)
              (forward-char 1)))

           (t
            (forward-char -1)

            ;; If the first line is empty, skip all empty space.
            (when (funcall is-line-empty-fn)
              (forward-char -1)
              (while (not (funcall is-line-empty-fn))
                (forward-char -1)))

            (let ((x (point)))
              (while (not (funcall is-line-empty-fn))
                (setq x (point))
                (forward-char -1))
              (goto-char x)
              (goto-char (pos-bol)))

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
    (goto-char (1+ (pos-eol)))
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
;; but stop symmetrical extending once the first syntax mismatch if found.
(defvar-local meep--region-syntax-asym nil)

;;;###autoload
(defun meep-region-syntax-expand ()
  "Expand on matching syntax table elements."
  (interactive)

  (when meep--region-syntax-asym
    (let ((local-last-command (meep--last-command)))
      (unless (memq
               local-last-command (list 'meep-region-syntax-expand 'meep-region-syntax-contract))
        (setq meep--region-syntax-asym nil))))

  (let ((syn-as-str-fn (lambda (syn) (char-to-string (syntax-class-to-char (syntax-class syn))))))
    (cond
     ((region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (is-forward (eq end (point)))
             (beg-ok
              (and (or (null meep--region-syntax-asym) (not is-forward)) (/= beg (point-min))))
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
         ((and (member beg-syn-str (list "(" ")")) (member end-syn-str (list "(" ")")))
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
              (skip-syntax-backward beg-syn-str)
              (setq beg-next (point))))
          (when do-end
            (save-excursion
              (goto-char end)
              (skip-syntax-forward end-syn-str)
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
            (meep--set-marker end-next)
            (progn
              (activate-mark t)
              (setq deactivate-mark nil)))

           (t
            (goto-char end-next)
            (meep--set-marker beg-next)
            (progn
              (activate-mark t)
              (setq deactivate-mark nil)))))

        ;; Don't attempt symmetry in future.
        (unless meep--region-syntax-asym
          (unless (and do-beg do-end)
            (setq meep--region-syntax-asym t)))))

     (t
      (let* ((syn
              (or (syntax-after (point))
                  ;; Needed in case the point is at the end of the buffer.
                  (syntax-after (max (point-min) (1- (point)))))))

        (cond
         ((null syn)
          (message "No syntax around the point (empty buffer?)")
          nil)
         (t
          (let ((syn-str (and syn (funcall syn-as-str-fn syn))))
            (let ((beg-next
                   (save-excursion
                     (skip-syntax-backward syn-str)
                     (point)))
                  (end-next
                   (save-excursion
                     (skip-syntax-forward syn-str)
                     (point))))
              (meep--set-marker beg-next)
              (goto-char end-next)
              (progn
                (activate-mark t)
                (setq deactivate-mark nil)))))))))))


;;;###autoload
(defun meep-region-syntax-contract ()
  "Contract matching syntax table."
  (interactive)
  (unless (region-active-p)
    (user-error "No region to expand"))

  (when meep--region-syntax-asym
    (let ((local-last-command (meep--last-command)))
      (unless (memq
               local-last-command (list 'meep-region-syntax-expand 'meep-region-syntax-contract))
        (setq meep--region-syntax-asym nil))))

  (let ((syn-as-str-fn (lambda (syn) (char-to-string (syntax-class-to-char (syntax-class syn))))))
    (let* ((beg (region-beginning))
           (end (region-end))
           (is-forward (eq end (point)))
           (beg-ok
            (and (or (null meep--region-syntax-asym) (not is-forward)) (/= beg (point-min))))
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
       ((and (member beg-syn-str (list "(" ")")) (member end-syn-str (list "(" ")")))
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
            (skip-syntax-forward beg-syn-str)
            (setq beg-next (point))))
        (when do-end
          (save-excursion
            (goto-char end)
            (skip-syntax-backward end-syn-str)
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
          (deactivate-mark t))

         ((eq beg (point))
          (goto-char beg-next)
          (meep--set-marker end-next)
          (progn
            (activate-mark t)
            (setq deactivate-mark nil)))

         (t
          (goto-char end-next)
          (meep--set-marker beg-next)
          (progn
            (activate-mark t)
            (setq deactivate-mark nil)))))

      ;; Don't attempt symmetry in future.
      (unless meep--region-syntax-asym
        (unless (and do-beg do-end)
          (setq meep--region-syntax-asym t))))))


;; ---------------------------------------------------------------------------
;; Command: Repeat N

;; Only ever use this when the last command was numeric.
(defvar meep--numeric-last-command nil)
(defvar meep--numeric-last-prefix-arg nil)

(defun meep--last-command ()
  "A version of `last-command' that isn't masked by numeric arguments."
  (cond
   ;; The symbol check is needed as keys may be bound to a lambda,
   ;; in that case checking for the symbol raises an error.
   ((and (symbolp last-command) (meep-command-is-digit-argument last-command))
    meep--numeric-last-command)
   (t
    last-command)))

(defun meep--last-prefix-arg ()
  "A version of `last-prefix-arg' that isn't masked by numeric arguments."
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
  ;; Copied from `digit-command'
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
            ;; Ensures an action can be adjusted without setting the mark.
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
  "Begin defining a macro."
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
  "Jump to the register, may jump to a location or call a macro ARG times."
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
;; ISearch Wrapper
;;
;; Support searching in both directions as well as
;; searching based on the active region.

;; Wrapped search.
(defcustom meep-isearch-activate-mark t
  "ISearch activates the mark (transient).
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
  "Search forward a REGEXP."
  (interactive)
  (add-hook 'isearch-mode-end-hook #'meep--isearch-done-hook 0 t)
  (call-interactively #'isearch-forward-regexp))

;;;###autoload
(defun meep-isearch-regexp-prev ()
  "Search backward a REGEXP."
  (interactive)
  (add-hook 'isearch-mode-end-hook #'meep--isearch-done-hook 0 t)
  (call-interactively #'isearch-backward-regexp))

(defun meep--isearch-repeat-impl (dir)
  "Repeat search in direction DIR."
  ;; Re-display can flicker.
  (let ((inhibit-redisplay t)
        ;; Opinionated, but ISEARCH is not that usable without these.
        (isearch-wrap-pause 'no-ding)
        (isearch-repeat-on-direction-change t)
        (had-region (region-active-p)))

    (cond
     ((< dir 0)
      (isearch-repeat-backward (- dir)))
     (t
      (isearch-repeat-forward dir)))

    (meep--isearch-handle-done had-region)))

;;;###autoload
(defun meep-isearch-repeat-next (arg)
  "Repeat ISEARCH forwards ARG times."
  (interactive "p")
  (meep--isearch-repeat-impl arg))

;;;###autoload
(defun meep-isearch-repeat-prev (arg)
  "Repeat ISEARCH backwards ARG times."
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
  "Extract regex from a TEXT-BOUNDS purpose of searching."

  (let ((text (buffer-substring-no-properties (car text-bounds) (cdr text-bounds)))
        (beg nil)
        (end nil)
        (beg-test-list (list "\\_<" "\\<" "\\b"))
        (end-test-list (list "\\_>" "\\>" "\\b")))

    ;; NOTE: exactly how to do this isn't clear, looking-at commands work well-enough.
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
      (push text regexp-search-ring)
      ;; Inline `isearch-yank-string' because it expects non regex text,
      ;; however this text is already quoted.
      (progn
        (setq isearch-yank-flag t)
        (isearch-process-search-string text (mapconcat #'isearch-text-char-description text ""))))

    (isearch-exit)

    (meep--isearch-handle-done had-region)))

;;;###autoload
(defun meep-isearch-at-point-next (arg)
  "Search forwards for the symbol or region at the current point.
Repeat the search ARG times."
  (interactive "p")
  (meep--isearch-at-point-impl arg))

;;;###autoload
(defun meep-isearch-at-point-prev (arg)
  "Search backwards for the symbol or region at the current point.
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
      (line-move-to-column (min (pos-eol) goal-column))))
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

        (line-move-to-column (min (pos-eol) column))
        (cond
         ((consp temporary-goal-column)
          (setcar temporary-goal-column column))
         (t
          (setq temporary-goal-column column))))))
   (t ;; Set a new temporary column.
    (let ((column (current-column)))
      (prog1 (funcall fn)
        (line-move-to-column (min (pos-eol) column))
        (setq temporary-goal-column column))))))

(defmacro meep--with-respect-goal-column (&rest body)
  "Execute BODY, maintaining the goal column."
  `(meep--respect-goal-column-impl (lambda () ,@body)))

(defun meep--delete-from-motion-fn (fn)
  "Kill based on a motion from calling FN."
  (let ((pos-orig (point))
        (pos-next
         (save-excursion
           (funcall fn)
           (point))))
    (delete-region pos-orig pos-next)))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-symbol-next (arg)
  "Kill the symbol forwards ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn (lambda () (forward-thing 'symbol arg))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-symbol-prev (arg)
  "Kill the symbol backwards ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn (lambda () (forward-thing 'symbol (- arg)))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-next (arg)
  "Kill the syntax-spans forwards ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn (lambda () (meep--move-same-syntax-impl arg t (cons nil nil) nil))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-prev (arg)
  "Kill the syntax-spans backwards ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn
   (lambda () (meep--move-same-syntax-impl (- arg) t (cons nil nil) nil))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-or-symbol-next (arg)
  "Kill the syntax-spans or symbols forwards ARG times."
  (interactive "*p")
  (meep--delete-from-motion-fn
   (lambda () (meep--move-same-syntax-impl arg t (cons nil nil) 'symbol))))

;; NOTE: this is mainly useful for binding in insert mode.
;;;###autoload
(defun meep-delete-same-syntax-or-symbol-prev (arg)
  "Kill the syntax-spans or symbols backwards ARG times."
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
;; Character level delete which has it's own kill-ring.
;; This can be useful for quickly relocating characters.
;;
;; Note that this is only accumulated on successive calls.

(defvar meep-delete-char-ring nil
  "Deleted characters.
Used by `meep-delete-char-ring-next', `meep-delete-char-ring-prev' &
`meep-delete-char-ring-yank'.")

(defun meep--delete-char-ring-maybe-clear ()
  "Clear the delete character ring as needed."
  ;; Only accumulate successive calls,
  ;; otherwise this would either grow indefinitely or need to be "managed".
  (unless (memq last-command (list 'meep-delete-char-ring-next 'meep-delete-char-ring-prev))
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
    (message "Delete char ring empty"))))

;;;###autoload
(defun meep-delete-char-ring-yank (arg)
  "Yank from the delete character ring ARG times."
  (interactive "*p")
  (meep--delete-char-ring-yank-impl arg nil))

;;;###autoload
(defun meep-delete-char-ring-yank-no-pop (arg)
  "Yank from the delete character ring ARG times.

Leave the char-ring unmodified afterwards."
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
        (funcall replace-in-range-fn (point) (1+ (point)))))))))


;;;###autoload
(defun meep-char-insert (ch arg)
  "Read a character CH and insert it or replace the active region.
Inset ARG times."
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
;; Text Editing: Surround Insert/Delete

;; TODO: can emacs do this itself?
;; Maybe make a public variable.
(defvar meep--char-surround-pairs (list (cons ?\( ?\)) (cons ?\[ ?\]) (cons ?{ ?}) (cons ?< ?>))
  "Bracket pairs.")

(defun meep--char-pair-find (ch)
  "Return the matching character for CH or nil."
  (let ((pairs meep--char-surround-pairs)
        (result nil))
    (while pairs
      (let ((ch-pair (pop pairs)))
        (cond
         ((eq ch (car ch-pair))
          (setq result (cdr ch-pair))
          ;; Break.
          (setq pairs nil))
         ((eq ch (cdr ch-pair))
          (setq result (car ch-pair))
          ;; Break.
          (setq pairs nil)))))
    result))

(defun meep--char-surround-insert-impl (ch arg line-wise)
  "Surround the region by CH ARG times.
When LINE-WISE is non-nil, surround each line otherwise use region bounds."
  ;; Sanitize numeric prefix.
  (when (< arg 0)
    (setq arg (abs arg)))

  (meep--char-is-ok-or-error "Surround" ch)

  (let* ((buffer-len-old (- (point-max) (point-min)))
         (ch-end (or (meep--char-pair-find ch) ch))

         (surround-in-range-fn
          `(lambda (beg end n)
             ;; Make the values global.
             (let ((ch-prefix (make-string n ,ch))
                   (ch-suffix (make-string n ,ch-end)))
               (save-excursion
                 (goto-char end)
                 (insert ch-suffix)
                 (goto-char beg)
                 (insert ch-prefix))
               (+ (length ch-prefix) (length ch-suffix)))))

         (surround-in-range-from-columns-fn
          `(lambda (col-beg col-end n)
             ;; Make the values global.
             (let ((ch-prefix (make-string n ,ch))
                   (ch-suffix (make-string n ,ch-end))
                   (beg nil)
                   (end nil)
                   (col-beg-found nil)
                   (col-end-found nil)
                   (result 0))

               (save-excursion
                 (setq col-beg-found (move-to-column col-beg))
                 (setq beg (point))
                 (setq col-end-found (move-to-column col-end))
                 (setq end (point))

                 ;; The region doesn't intersect this line at all, skip it.
                 (unless (eq col-beg-found col-end-found)
                   (goto-char end)
                   (insert ch-suffix)
                   (goto-char beg)
                   (insert ch-prefix)

                   (meep--incf result (+ (length ch-prefix) (length ch-suffix)))))

               result))))

    (cond
     ((bound-and-true-p rectangle-mark-mode)
      (apply-on-rectangle
       ;; Make the values global.
       `(lambda (col-beg col-end)
          (funcall ,surround-in-range-from-columns-fn col-beg col-end ,arg))
       (region-beginning) (region-end)))
     (t
      ;; Support surrounding by "implied" region.
      (meep--mark-on-motion-maybe-activate)
      (cond
       ((region-active-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (cond
           ;; Surround each line.
           (line-wise
            (save-excursion
              (goto-char beg)
              (while (< (point) end)
                (let ((end-iter (min end (pos-eol))))
                  (when (< (point) end-iter)
                    (let ((n-add (funcall surround-in-range-fn (point) end-iter arg)))
                      (meep--incf end n-add)
                      (meep--incf end-iter n-add)))
                  (cond
                   ((eq end-iter end)
                    ;; Break.
                    (goto-char end))
                   (t
                    (goto-char (1+ end-iter))
                    (skip-chars-forward "\n\r" end)))))))
           (t
            (funcall surround-in-range-fn beg end arg)))))
       (t
        (cond
         (line-wise
          (funcall surround-in-range-fn (pos-bol) (pos-eol) arg))
         (t
          (funcall surround-in-range-fn (point) (point) arg)))))))

    (let ((buffer-len-new (- (point-max) (point-min))))
      (unless (eq buffer-len-old buffer-len-new)
        ;; A small nicety, if the point is on the beginning,
        ;; keep it "inside" the surrounding characters.
        ;; Only the left hand side needs updating.
        (let* ((m (mark-marker))
               (p-pos (point))
               (m-pos (or (and m (marker-position m)) p-pos))
               (update-pos (<= p-pos m-pos))
               (update-mrk (and m (>= p-pos m-pos))))
          (when update-pos
            (goto-char (+ (point) arg)))
          (when update-mrk
            (set-marker m (+ (marker-position m) arg))))))))

;;;###autoload
(defun meep-char-surround-insert (ch arg)
  "Read a character CH and surround the selection with it.
Inset ARG times.

When there is no active region, surround the current point."
  (interactive "*cSurround Char:\np")
  (meep--char-surround-insert-impl ch arg nil))

;;;###autoload
(defun meep-char-surround-insert-lines (ch arg)
  "Read a character CH and surround the selected lines with it.
Inset ARG times.

When multiple lines are are in the active region,
surround each line individually.
When there is no active region, surround the current line."
  (interactive "*cSurround Lines by Char:\np")
  (meep--char-surround-insert-impl ch arg t))


;; ---------------------------------------------------------------------------
;; Text Editing: Join Lines
;;
;; Line joining with support for left-trimming code-comments,
;; so this may be used to conveniently joining lines in code.
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
;;    /* Example. Block. Next line. */

(defun meep--join-maybe-skip-comment-prefix (limit)
  "Skip forward over comment chars and any following white-space.
Don't skip past LIMIT."
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
               ;; Less trouble then checking all the derived modes.
               (bound-and-true-p c-block-comment-prefix))
      ;; White space has already been skipped, so trim it here.
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
      (save-match-data
        (when (re-search-forward comment-start-skip limit t)
          (skip-chars-forward "[:blank:]" limit))))))

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
       ;; we only need to check if the first non white-space is a:
       ;; - comment start 11.
       ;; - comment generic 14.
       ((memq (car (syntax-after bol-ws-next)) (list 11 14))
        (setq result t))))
    result))

(defun meep--join-range (bol eol use-comment-strip)
  "Join new lines between BOL & EOL.
When USE-COMMENT-STRIP is non-til, strip comments."
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
  "Join newline in the region from BEG to END.
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
        (while (and (not
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
        (while (and (not
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
      (message "Join line beginning: no following line found"))
    changed))


;; ---------------------------------------------------------------------------
;; Text Editing: Tab Wrapper

;;;###autoload
(defun meep-indent-rigidly ()
  "Indent the active region or the current line."
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
;; Inserts after the cursor OR on the opposite end of the region.
;;;###autoload
(defun meep-insert-append ()
  "Enter insert mode."
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
  "Enter insert mode where insert mode was last exited."
  (interactive)
  (let ((pos-last-insert
         (let ((reg-val (get-register meep-state-insert-register)))
           (unless reg-val
             (user-error "No register found"))
           (unless (markerp reg-val)
             (user-error "No register found - not a marker"))
           (marker-position reg-val))))

    (goto-char pos-last-insert)
    (meep--insert-impl)))

(defun meep--insert-overwrite-disable-on-exit ()
  "A callback to disable overwrite mode."
  (overwrite-mode -1)
  ;; Don't check if it's valid since this hook can only be installed if the symbol is found.
  (let ((hook-sym (bray-state-get-hook-exit meep-state-insert)))
    (remove-hook hook-sym #'meep--insert-overwrite-disable-on-exit t)))

;;;###autoload
(defun meep-insert-overwrite ()
  "Enter insert mode & enable `overwrite-mode' while inserting."
  (interactive)
  (meep-insert)
  (let ((hook-sym (bray-state-get-hook-exit meep-state-insert)))
    (cond
     (hook-sym
      (overwrite-mode 1)
      (add-hook hook-sym #'meep--insert-overwrite-disable-on-exit 0 t))
     (t
      (message "No exit hook found for state: %S" meep-state-insert)))))

;;;###autoload
(defun meep-insert-change ()
  "Change the region, entering insert mode.
The region may be implied, see `meep-command-is-mark-set-on-motion-any'."
  (interactive "*")
  (cond
   ((bound-and-true-p rectangle-mark-mode)
    ;; Sort of odd but this is how emacs supports changing a region.
    (call-interactively #'string-rectangle))

   (t
    ;; Avoid `meep--mark-on-motion-maybe-activate' because it actually activates.
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
  ;; NOTE: there is two modes for this action, the behavior is as follows:
  ;; - Without an active region the result is:
  ;;   - Delete the line back until the indentation level,
  ;;   - Inter insert mode.
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
  ;;     Where the cursor is always moved to the following next.
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
    (call-interactively #'string-rectangle))

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

;;;###autoload
(defun meep-insert-into-last ()
  "Insert text into last insert point."
  (interactive "*")
  (let ((beg nil)
        (end nil)
        (pos-last-insert
         (let ((reg-val (get-register meep-state-insert-register)))
           (unless reg-val
             (user-error "No register found"))
           (unless (markerp reg-val)
             (user-error "No register found - not a marker"))
           (marker-position reg-val))))

    (cond
     ((region-active-p)
      (setq beg (region-beginning))
      (setq end (region-end)))
     (t
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (when bounds
          (setq beg (car bounds))
          (setq end (cdr bounds))))))

    (let ((text (and beg end (buffer-substring-no-properties beg end))))
      (goto-char pos-last-insert)
      (meep-insert)
      (when text
        (insert text)))))


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
  "Move the line indentation start and switch to INSERT state."
  (interactive "*")
  (back-to-indentation)
  (bray-state-set meep-state-insert))

;;;###autoload
(defun meep-insert-line-end ()
  "Move the line end and switch to INSERT state."
  (interactive "*")
  (end-of-line)
  (bray-state-set meep-state-insert))


;; ---------------------------------------------------------------------------
;; Clipboard: System Only
;;
;; These commands only wrap the "systems" clipboard,
;; without mixing the kill-ring or primary clipboard - for predictable results.

(defun meep--clipboard-only-cut-or-copy-impl (beg end do-cut)
  "Copy or cut to system clipboard.
Arguments BEG & END define the region.
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
    (let ((text (buffer-substring-no-properties beg end)))
      (when do-cut
        (delete-region beg end))

      (let ((select-enable-clipboard t)
            (select-enable-primary nil))
        (funcall interprogram-cut-function text))
      (setq deactivate-mark t)))))

;;;###autoload
(defun meep-clipboard-only-copy ()
  "Copy the region using the clipboard-only."
  (interactive)
  (meep--clipboard-only-cut-or-copy-impl (region-beginning) (region-end) nil))

;;;###autoload
(defun meep-clipboard-only-cut ()
  "Cut the region using the clipboard-only."
  (interactive "*")
  (meep--clipboard-only-cut-or-copy-impl (region-beginning) (region-end) t))

;;;###autoload
(defun meep-clipboard-only-cut-line ()
  "Cut the whole line using the clipboard-only."
  (interactive "*")
  ;; Note that this command writes to the system clipboard.
  ;; (kill-whole-line)
  (meep--with-respect-goal-column
   (meep--clipboard-only-cut-or-copy-impl (pos-bol) (min (1+ (pos-eol)) (point-max)) t)))

(defun meep--clipboard-only-yank-impl ()
  "Yank-replace from clipboard-only."
  (let ((text (funcall interprogram-paste-function)))
    ;; This is strange that emacs cannot access it's own selection.
    ;; Copy sets this value, could investigate this further.
    (unless text
      (setq text gui--last-selected-text-clipboard))

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
  "Yank from the clipboard-only, replacing the region (indenting the content)."
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
  "Yank from the clipboard-only, replacing the region (as lines)."
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
;; So line-wise kill & yank can be used to operate on lines without the need
;; to place the point at the beginning of the line.
;;
;; If you wish to override this behavior, you may activate the region with an empty range,
;; since an active region always defines the range.
;;
;; Note that rect-wise regions are also stored in the kill-ring and paste from the top-left.

;; Currently only used to differentiate each kind regions (line-wise & rect-wise).
(defun meep--yank-handler-line-wise (&rest args)
  "Line wise wrapper for ARGS."
  (apply #'insert args))
(defun meep--yank-handler-rect-wise (&rest args)
  "Rectangle wise wrapper for ARGS."
  (apply #'insert args))
(defconst meep--yank-handler-from-region-type-alist
  (list
   (cons 'line-wise 'meep--yank-handler-line-wise)
   (cons 'rect-wise 'meep--yank-handler-rect-wise)))

(defun meep--yank-handler-from-region-type (region-type)
  "Return the yank-handler from the REGION-TYPE or ni."
  (cdr (assq region-type meep--yank-handler-from-region-type-alist)))

(defun meep--yank-handler-to-region-type (yank-handler)
  "Return region type from the YANK-HANDLER or nil."
  (car (rassq yank-handler meep--yank-handler-from-region-type-alist)))

(defun meep--wrap-current-kill (n &optional do-not-move)
  "Wrap `current-kill', only ever use the kill ring.
Forward N & DO-NOT-MOVE."
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
        (cond
         ((bound-and-true-p rectangle-mark-mode)
          (deactivate-mark t)
          (delete-rectangle (region-beginning) (region-end)))
         (t
          (let ((bounds (cons (region-beginning) (region-end))))
            ;; Important for the point to be at the start.
            ;; So pasting replaces this region.
            (when (< (mark) (point))
              (let ((pos-orig (point)))
                (goto-char (mark))
                (meep--set-marker pos-orig)))

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
  "Handle interactive part of yanking, interpret raw ARG.
Forward DO-NOT-MOVE & ROTATE-AS-STACK."
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
  "An equivalent to `kill-region' that respects MEEP clipboard settings.
Arguments BEG & END define the region.
When DO-CUT is non-nil, delete the region.

When REGION-TYPE is non-nil, store the region type in the kill ring,
this may be used when yanking."
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
  (let ((region-type (meep--state-region-type)))
    (meep--clipboard-killring-cut-or-copy (region-beginning) (region-end) region-type t)))

;;;###autoload
(defun meep-clipboard-killring-copy ()
  "Add the current region to the `kill-ring'.
The region need not be active."
  (interactive)
  (let ((region-type (meep--state-region-type)))
    (meep--clipboard-killring-cut-or-copy (region-beginning) (region-end) region-type nil)))

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
  "Yank from the ARG'th item from the `kill-ring' which is rotated.

Rotating the kill ring means that you may kill multiple items,
then conveniently yank those items afterwards."
  (interactive "*P")
  (meep--clipboard-killring-yank-impl-interactive arg nil t))

;;;###autoload
(defun meep-clipboard-killring-yank (arg)
  "Yank from the ARG'th item from the `kill-ring'.
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
  "Clipboard to use for the register-clipboard.

Used by `meep-clipboard-register-map'.")

(defun meep-clipboard-register-actions ()
  "Set the pre-defined register to use for `meep-clipboard-register-*' commands.

Uses the `meep-clipboard-register-map' key-map."
  (interactive)
  (setq meep--clipboard-register-current (register-read-with-preview "Clipboard register: "))
  (set-transient-map meep-clipboard-register-map
                     nil ; Don't keep the keymap it active.
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
    (meep--clipboard-register-yank-impl reg)))


;; ---------------------------------------------------------------------------
;; Keypad Mode
;;
;; Support entering a sequence of keys without the need to hold modifiers, see:
;; `keypad mode <https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#keypad>`__.

;; NOTE: Based on MEOW's keypad mode
;;;###autoload
(defun meep-keypad ()
  "Begin entering a key sequence."
  (interactive)
  ;; Functions & constants.
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
          ;; Build a sequence of keys, note that this is stored in reverse order
          ;; for conveniently adding to the start.
          ;; Each element may contain multiple keys - to ensure both
          ;; <delete> and DEL are properly detected.
          (keyseq (list (list "C-")))
          (was-space nil)
          ;; TODO: check if this can be automatically calculated.
          (char-map
           (list
            (cons "<backspace>" "BS")
            (cons "<delete>" "DEL")
            (cons "<escape>" "ESC")
            (cons "<linefeed>" "LFD")
            (cons "<return>" "RET")
            (cons "<space>" "SPC")
            (cons "<tab>" "TAB"))))

      (while (not found)
        (let ((maybe-complete nil))
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
                   (concat "]"))))

                (ch-str-list nil))

            (let ((ch-str (single-key-description ch))
                  (ch-str-other nil))

              (or (when-let* ((ch-cell (assoc ch-str char-map)))
                    (setq ch-str-other (cdr ch-cell))
                    t)
                  (when-let* ((ch-cell (rassoc ch-str char-map)))
                    (setq ch-str-other (car ch-cell))
                    t))

              (when ch-str-other
                (push ch-str-other ch-str-list))
              (push ch-str ch-str-list))

            (cond
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
                (setq maybe-complete t))))))

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
               ((or (and bind (symbolp bind)) (interpreted-function-p bind))
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

                (execute-kbd-macro kbd-keyseq))
               ((consp bind)
                ;; A key-map, keep looking.
                nil)
               ((null bind)
                (user-error "Keypad [%s] unknown!" keyseq-str-default))
               (t
                ;; Should never happen, but this is early development so it might.
                (user-error "Keypad [%s] unknown type: %S !"
                            keyseq-str-default
                            (type-of bind)))))))))))


;; ---------------------------------------------------------------------------
;; Command Advice
;;
;; Use this advice for commands you wish to use as "motions".
;; This causes them to set the mark before motion (unless overridden when repeating).
;;
;; To support repeating and selecting whole objects (via ``meep-exchange-point-and-mark-motion``),
;; these commands should also accept an integer argument, representing the number of
;; times the motion is made, reversing when negative.
;; This is typically indicated using ``(interactive "p")``.

(defun meep--command-mark-on-motion-advice (old-fn &rest args)
  "Advice to conditionally set the mark on motion (call OLD-FN with ARGS).

Note that this only changes behavior when MEEP is used
\(when `meep-enabled-p' succeeds)."
  ;; Check if MEEP is enabled, so this doesn't change behavior when the same operations
  ;; are used elsewhere (mini-buffer or similar).
  (cond
   ((meep-enabled-p)
    (meep--with-mark-on-motion-maybe-set
      (apply old-fn args)))
   (t
    (apply old-fn args))))

;;;###autoload
(defun meep-command-mark-on-motion-advice-add (cmd)
  "Add advice to CMD, so it sets the mark-on-motion.
This can be explicitly overridden (when repeating).
Use `meep-command-mark-on-motion-advice-remove' to remove the advice."
  (advice-add cmd :around #'meep--command-mark-on-motion-advice)
  (meep-command-prop-set cmd :mark-on-motion t))

;;;###autoload
(defun meep-command-mark-on-motion-advice-remove (cmd)
  "Remove advice added to CMD by `meep-command-mark-on-motion-advice-remove'."
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
  "Get CMD property for PROP or nil."
  (let* ((sym 'meep)
         (plist (get cmd sym)))
    (and plist (plist-get plist prop))))

;;;###autoload
(defun meep-command-prop-remove (cmd prop)
  "Remove CMD property for PROP, remove the `meep' property if it's empty."
  (let* ((sym 'meep)
         (plist (get cmd sym)))
    (when plist
      (unless (setq plist (meep--plist-remove plist prop))
        (setplist cmd (meep--plist-remove (symbol-plist cmd) sym))))))

;; Setup values...
(dolist (cmd
         (list
          ;; These don't set mark but allow marking.
          'meep-move-char-prev 'meep-move-char-next
          ;; Use reverse as an adjustment.
          'meep-exchange-point-and-mark 'meep-exchange-point-and-mark-motion))
  (meep-command-prop-set cmd :mark-on-motion 'adjust))

(dolist (cmd (list 'meep-exchange-point-and-mark 'meep-exchange-point-and-mark-motion))
  (meep-command-prop-set cmd :mark-activate t))

(dolist (cmd (list 'meep-digit-argument-repeat))
  (meep-command-prop-set cmd :digit-repeat t))

(dolist (cmd
         (list
          'meep-clipboard-killring-cut-line
          'meep-clipboard-only-cut-line
          'meep-move-line-next
          'meep-move-line-prev
          'next-line
          'previous-line))
  (meep-command-prop-set cmd :respect-temporary-goal-column t))

;; Currently only repeat-fu uses this.
(dolist (cmd
         (list
          'meep-char-replace
          'meep-char-insert
          'meep-delete-char-next
          'meep-delete-char-prev
          'meep-delete-char-ring-next
          'meep-delete-char-ring-prev
          'meep-delete-char-ring-yank))
  (meep-command-prop-set cmd :mark-on-motion-exclude t))

;; The inactive mark should be used, but the action should not be repeated.
(dolist (cmd
         (list
          ;; Only this is used for interactive ISEARCH.
          'isearch-exit
          ;; `'meep-isearch-regexp-prev'
          ;; `'meep-isearch-regexp-next'

          'meep-isearch-at-point-prev
          'meep-isearch-at-point-next
          'meep-isearch-repeat-prev
          'meep-isearch-repeat-next))
  (meep-command-prop-set cmd :mark-on-motion-no-repeat t))

(dolist (cmd
         (list
          'meep-move-by-sexp-any-next
          'meep-move-by-sexp-any-prev
          'meep-move-by-sexp-out-next
          'meep-move-by-sexp-out-prev
          'meep-move-by-sexp-over-next
          'meep-move-by-sexp-over-prev
          'meep-move-find-char-on-line-at-next
          'meep-move-find-char-on-line-at-prev
          'meep-move-find-char-on-line-repeat-at-next
          'meep-move-find-char-on-line-repeat-at-prev
          'meep-move-find-char-on-line-repeat-till-next
          'meep-move-find-char-on-line-repeat-till-prev
          'meep-move-find-char-on-line-till-next
          'meep-move-find-char-on-line-till-prev
          'meep-move-same-syntax-or-symbol-next
          'meep-move-same-syntax-or-symbol-prev
          'meep-move-same-syntax-and-space-next
          'meep-move-same-syntax-and-space-next-end
          'meep-move-same-syntax-and-space-prev
          'meep-move-same-syntax-next
          'meep-move-same-syntax-prev
          'meep-move-line-beginning
          'meep-move-line-end
          'meep-move-line-next
          'meep-move-line-non-space-beginning
          'meep-move-line-non-space-end
          'meep-move-line-prev
          'meep-move-matching-bracket-inner
          'meep-move-matching-bracket-outer
          'meep-move-matching-contextual-inner
          'meep-move-matching-contextual-outer
          'meep-move-matching-syntax-inner
          'meep-move-matching-syntax-outer
          'meep-move-paragraph-next
          'meep-move-paragraph-prev
          'meep-move-sentence-next
          'meep-move-sentence-prev
          'meep-move-symbol-next
          'meep-move-symbol-next-end
          'meep-move-symbol-prev
          'meep-move-to-bounds-of-comment-inner
          'meep-move-to-bounds-of-defun-inner
          'meep-move-to-bounds-of-line-inner
          'meep-move-to-bounds-of-paragraph-inner
          'meep-move-to-bounds-of-string-inner
          'meep-move-to-bounds-of-visual-line-inner
          'meep-move-word-next
          'meep-move-word-next-end
          'meep-move-word-prev

          'meep-move-to-bounds-of-paragraph
          'meep-move-to-bounds-of-comment
          'meep-move-to-bounds-of-string
          'meep-move-to-bounds-of-line
          'meep-move-to-bounds-of-visual-line
          'meep-move-to-bounds-of-defun))
  (meep-command-prop-set cmd :mark-on-motion t))

(dolist (cmd-pair
         (list
          (cons 'meep-move-line-next 'next-line)
          (cons 'meep-move-line-prev 'previous-line)
          (cons 'meep-move-line-end 'move-end-of-line)
          (cons 'meep-move-line-beginning 'move-beginning-of-line)))
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
  "Check if CMD is any motion."
  (or (meep-command-is-mark-set-on-motion cmd)
      (meep-command-is-mark-set-on-motion-adjust cmd)
      (meep-command-is-mark-set-on-motion-no-repeat cmd)))
;;;###autoload
(defun meep-command-is-mark-activate (cmd)
  "Return t if CMD doesn't use mark on motion (without active)."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :mark-activate)))
;;;###autoload
(defun meep-command-is-mark-on-motion-exclude (cmd)
  "Return t if CMD doesn't use mark on motion (without active)."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :mark-on-motion-exclude)))
;;;###autoload
(defun meep-command-is-mark-respect-temporary-goal-column (cmd)
  "Return t if CMD doesn't use mark on motion (without active)."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :respect-temporary-goal-column)))
;;;###autoload
(defun meep-command-is-digit-argument (cmd)
  "Return t if CMD is a numeric command."
  (declare (important-return-value t))
  (eq t (meep-command-prop-get cmd :digit-repeat)))

;; ---------------------------------------------------------------------------
;; Public API
;;
;; Public non interactive functions.

;;;###autoload
(defun meep-enabled-p ()
  "Return non-nil if MEEP is enabled."
  (bound-and-true-p bray-mode))

;;;###autoload
(defun meep-bootstrap-once ()
  "Run this to initialize MEEP.

This may be use for `use-package' commands, to defer loading MEEP until it's needed."
  (ignore))

(provide 'meep)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; meep.el ends here
