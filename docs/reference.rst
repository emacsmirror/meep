
##############
MEEP Reference
##############


.. BEGIN VARIABLES

Custom Variables
----------------

``meep-mark-set-on-motion``: ``t``
   Motion sets the mark.

``meep-state-insert-register``: ``94``
   The register set when leaving insert mode.

   Used by ``meep-insert-last`` which will enter insert mode at this location.

``meep-move-comment-skip-space``: ``t``
   When navigating comment bounds, skip leading/trailing space.

``meep-move-comment-skip-repeated``: ``t``
   When navigating comment bounds, skip repeated characters.

``meep-region-swap-imply-region``: ``t``
   Imply the region from the length of the secondary region.

   - When the region on a single line:
     The text after point implies the selection.
   - When a line-wise region is used:
     The same number of lines after the point is used (ignoring line length).
   - When a rectangle-wise region is used:
     The text after & lines below are used to create the implied selection.

``meep-isearch-activate-mark``: ``t``
   ISearch activates the mark (transient).
   So motion drops the selection.

   Useful for pasting while stepping over search results.


Other Variables
---------------

``meep-state-region-elem``: ``nil``
   Supported values are symbols nil or 'line-wise.

   Note that line-wise navigation is not enforced,
   this is a hint that commands may use.

``meep-mark-set-on-motion-override``: ``nil``
   This constant exists so it’s possible to let-bind the value to t.

   Used so a motion can be repeated without setting the mark.

``meep-move-by-sexp-over-depth``: ``nil``
   The target depth when moving over S-expressions.
   Used to maintain the depth even when moving over causes
   navigation to move to outer scope.

   Only used between successive
   ``meep-move-by-sexp-over-next`` & ``meep-move-by-sexp-over-prev`` calls.

``meep-delete-char-ring``: ``nil``
   Deleted characters.
   Used by ``meep-delete-char-ring-next``, ``meep-delete-char-ring-prev`` &
   ``meep-delete-char-ring-yank``.

``meep-clipboard-register-map``: ``(keymap)``
   Clipboard to use for the register-clipboard.

   Used by ``meep-clipboard-register-map``.


Commands
--------

Motion: Symbol/Word
^^^^^^^^^^^^^^^^^^^

Command properties:
commands may have a `meep' property, this is expected to be a P-list of properties.

:mark-on-motion
   - t: Mark on motion.
   - 'adjust: Adjust the previous motion.

     This is used so a motion can be adjusted,
     without breaking the chain of commands used to repeat an action.
     So it's possible to perform a motion & any number of adjustments before an edit-command.

     When repeating the motion, adjustments and edit will all be repeated.
     Single character motion commands take advantage of this.

   - nil: don't mark on motion (same as missing).

:mark-on-motion-no-repeat
   - t: These motions that should not be repeated such as search.
     (used by repeat-fu).
:digit-repeat
   - t: The command is a digit command.

     This command can repeat other commands multiple times.

``(meep-move-symbol-prev ARG)``
   Move point to the beginning of the previous symbol, ARG times.

``(meep-move-symbol-prev-end ARG)``
   Move to the end of the previous symbol, ARG times.

``(meep-move-symbol-next-end ARG)``
   Move to the end of the next symbol, ARG times.

``(meep-move-symbol-next ARG)``
   Move point to the beginning next symbol, ARG times.

``(meep-move-word-prev ARG)``
   Move point to the beginning of the previous word, ARG times.

``(meep-move-word-next-end ARG)``
   Move to the end of the next word ARG times.

``(meep-move-word-prev-end ARG)``
   Move to the end of the previous word ARG times.

``(meep-move-word-next ARG)``
   Move point to the beginning of the next word, ARG times.

Motion: Same Syntax
^^^^^^^^^^^^^^^^^^^

``(meep-move-same-syntax-prev ARG)``
   Move back a syntax-spans ARG times.

``(meep-move-same-syntax-next ARG)``
   Move to the end of the next word ARG times.

Motion: Same Syntax or Symbol
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Skips over the same syntax or entire symbols.

``(meep-move-same-syntax-or-symbol-prev ARG)``
   Move back a syntax-spans or symbols ARG times.

``(meep-move-same-syntax-or-symbol-next ARG)``
   Move forward a syntax-spans or symbols ARG times.

Motion: Same Syntax & Space
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Skips over the same syntax with changes to behavior for surrounding space,
where space at the bounds of text is skipped over, matching
how this is handled for skipping words & symbols.

``(meep-move-same-syntax-and-space-prev ARG)``
   Move back a syntax-and-space, ARG times.

``(meep-move-same-syntax-and-space-next ARG)``
   Move to the end of the next syntax-and-space, ARG times.

``(meep-move-same-syntax-and-space-next-end ARG)``
   Move to the beginning of the next syntax-and-space, ARG times.

Motion: Line
^^^^^^^^^^^^

``(meep-move-line-beginning ARG)``
   Move to the beginning of the current line end.
   Moves to the end when ARG is negative.

``(meep-move-line-end ARG)``
   Move to the end of the current line end.
   Moves to the beginning when ARG is negative.

``(meep-move-line-non-space-beginning ARG)``
   Move the the beginning of the line, ignoring end of line white-spaces.
   A negative ARG moves to the end.

``(meep-move-line-non-space-end ARG)``
   Move the the end of the line, ignoring end of line white-spaces.
   A negative ARG moves to the beginning.

``(meep-move-line-prev ARG)``
   Move to the previous line ARG times.

``(meep-move-line-next ARG)``
   Move to the next line ARG times.

Motion: Character
^^^^^^^^^^^^^^^^^

``(meep-move-char-prev ARG)``
   Move to the previous character ARG times.

``(meep-move-char-next ARG)``
   Move to the next character ARG times.

Motion: Paragraph
^^^^^^^^^^^^^^^^^

``(meep-move-paragraph-prev ARG)``
   Move backward paragraphs ARG times.

``(meep-move-paragraph-next ARG)``
   Move forward paragraphs ARG times.

Motion: Sentence
^^^^^^^^^^^^^^^^

``(meep-move-sentence-prev ARG)``
   Move backward sentences, ARG times.

``(meep-move-sentence-next ARG)``
   Move forward sentences, ARG times.

Motion: S-expressions
^^^^^^^^^^^^^^^^^^^^^

``(meep-move-by-sexp-any-next ARG)``
   Jump to the next SEXP.
   Step ARG times or 1 when default.

``(meep-move-by-sexp-any-prev ARG)``
   Jump to the previous SEXP.
   Step ARG times or 1 when default.

``(meep-move-by-sexp-over-next ARG)``
   Move next over the SEXP ARG times.

``(meep-move-by-sexp-over-prev ARG)``
   Move previous over the SEXP ARG times.

``(meep-move-by-sexp-out-prev &optional ARG)``
   Jump to the previous SEXP, jumping out of the current expression.
   Step ARG times or 1 when default.

``(meep-move-by-sexp-out-next &optional ARG)``
   Jump to the next SEXP, jumping into the next expression.
   Step ARG times or 1 when default.

``(meep-move-matching-bracket-outer)``
   Jump to the matching outer bracket.
   When not at the bounds, jump the start (when enclosed in brackets).

   Return non-nil when the point was moved.

Motion: Matching Characters
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Jump to the opposite character - bracket, quote or comment bounds
depending on the command.

When no matching character is found,
jump to the start of the surrounding characters (if found).

``(meep-move-matching-bracket-inner)``
   Jump to the matching inner bracket.
   When not at the bounds, jump the start (when enclosed in brackets).

   Return non-nil when the point was moved.

``(meep-move-matching-syntax-outer)``
   Move to the outer matching string/comment syntax.
   When not at the bounds, jump the start (when in a string/comment).

   Return nil if no matching syntax was found.

``(meep-move-matching-syntax-inner)``
   Move to the inner matching sting/comment syntax.
   When not at the bounds, jump the start (when in a string/comment).

   Return nil if no matching syntax was found.

``(meep-move-matching-contextual-outer)``
   Move to the matching character.
   When not at the bounds, jump the start.

``(meep-move-matching-contextual-inner)``
   Move to the matching character.
   When not at the bounds, jump the start.

Motion: Find & Till
^^^^^^^^^^^^^^^^^^^

``(meep-move-find-char-on-line-at-next ARG CH)``
   Find the next ARG char CH, read from mini-buffer.

``(meep-move-find-char-on-line-at-prev ARG CH)``
   Find the previous ARG char CH, read from mini-buffer.

``(meep-move-find-char-on-line-till-next ARG CH)``
   Find till the next ARG char CH, read from mini-buffer.

``(meep-move-find-char-on-line-till-prev ARG CH)``
   Find till the previous ARG CH, char read from mini-buffer.

``(meep-move-find-char-on-line-repeat-at-next ARG)``
   Repeat find ARG chars forwards.

``(meep-move-find-char-on-line-repeat-at-prev ARG)``
   Repeat find ARG chars backwards.

``(meep-move-find-char-on-line-repeat-till-next ARG)``
   Repeat find ARG chars forwards.

``(meep-move-find-char-on-line-repeat-till-prev ARG)``
   Repeat find ARG chars backwards.

Motion: Bounds
^^^^^^^^^^^^^^

``(meep-move-to-bounds-of-sentence ARG &optional INNER)``
   Move to the sentences start/end (start when ARG is negative).
   INNER to move to inner bound.

``(meep-move-to-bounds-of-sentence-inner ARG)``
   Move to the inner sentences start/end (start when ARG is negative).

``(meep-move-to-bounds-of-paragraph ARG &optional INNER)``
   Move to the paragraph start/end (start when ARG is negative).
   INNER to move to inner bound.

``(meep-move-to-bounds-of-paragraph-inner ARG)``
   Move to the inner paragraph start/end (start when ARG is negative).

``(meep-move-to-bounds-of-comment ARG &optional INNER)``
   Move to the comment start/end (start when ARG is negative).
   INNER to move to inner bound.

``(meep-move-to-bounds-of-comment-inner ARG)``
   Move to the comment inner start/end (start when ARG is negative).

``(meep-move-to-bounds-of-string ARG &optional INNER)``
   Move to the string start/end (start when ARG is negative).
   INNER to move to inner bound.

``(meep-move-to-bounds-of-string-inner ARG)``
   Move to the string inner start/end (start when ARG is negative).

``(meep-move-to-bounds-of-defun ARG &rest INNER)``
   Move to the function start/end (start when ARG is negative).
   INNER to move to inner bound.

``(meep-move-to-bounds-of-defun-inner ARG)``
   Move to the inner function start/end (start when ARG is negative).

``(meep-move-to-bounds-of-line ARG &optional INNER)``
   Move to the line start/end (start when ARG is negative).
   INNER to move to inner bound.

``(meep-move-to-bounds-of-line-inner ARG)``
   Move to the inner line start/end (start when ARG is negative).

``(meep-move-to-bounds-of-visual-line ARG &optional INNER)``
   Move to the visual-line start/end (start when ARG is negative).
   INNER to move to inner bound.

``(meep-move-to-bounds-of-visual-line-inner ARG)``
   Move to the inner visual-line start/end (start when ARG is negative).

``(meep-move-to-bounds-of-thing-beginning ARG)``
   Move to inner bounds of thing (begging).
   Move to the end with a negative ARG.

``(meep-move-to-bounds-of-thing-end ARG)``
   Move to inner bounds of thing (end).
   Move to the beginning with a negative ARG.

Selection/Region: Primitive
^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-region-enable)``
   Enable the active region.

``(meep-region-disable)``
   Disable the active region.

   The mark is not moved, the region can be restored
   via ``meep-exchange-point-and-mark``.

``(meep-region-toggle)``
   Toggle the active region.
   When the region is transient (where motion would clear it),
   this operation makes it stay active, running again clears it.

``(meep-exchange-point-and-mark)``
   Exchange the point and mark, activating the region.

``(meep-exchange-point-and-mark-motion)``
   Exchange the point and mark, activating the region.

Selection/Region: Secondary Selection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-region-swap)``
   Swap the contents of the primary & secondary region.

   When ``meep-region-swap-imply-region`` is non-nil,
   only the secondary region needs to be set.

``(meep-region-to-secondary-selection)``
   Create a secondary selection from the current region.

Selection/Region: Line Selection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-region-expand-to-line-bounds)``
   Expand the region to the line bounds.
   Consecutive

   ``meep-state-region-elem`` is set to \='line-wise which commands may
   use to maintain line-based selection.

Selection/Region: Expand/Contract
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Expand/contract the regions.
Initially expanding is performed in both directions until
a syntax mismatch is encountered, then expanding is only performed at the point.

This allows for expanding across surrounding symmetrical characters which can be useful.

``(meep-region-syntax-expand)``
   Expand on matching syntax table elements.

``(meep-region-syntax-contract)``
   Contract matching syntax table.

Command: Repeat N
^^^^^^^^^^^^^^^^^

``(meep-digit-argument-repeat)``
   Repeat the last command multiple times.

   This must be bound to keys 0..9 or the minus key.

Keyboard Macro Access
^^^^^^^^^^^^^^^^^^^^^

This provides access to macros where keys can be
conveniently assigned to macros (VIM style).

``(meep-register-kmacro-start-or-end)``
   Begin defining a macro.

``(meep-register-jump-to)``
   Jump to the register, this may call a macro or jump to a location.

ISearch Wrapper
^^^^^^^^^^^^^^^

Support searching in both directions as well as
searching based on the active region.

``(meep-isearch-regexp-next)``
   Search forward a REGEXP.

``(meep-isearch-regexp-prev)``
   Search backward a REGEXP.

``(meep-isearch-repeat-next)``
   Repeat ISEARCH forwards.

``(meep-isearch-repeat-prev)``
   Repeat ISEARCH backwards.

``(meep-isearch-at-point-next)``
   Search forwards for the symbol or region at the current point.

``(meep-isearch-at-point-prev)``
   Search backwards for the symbol or region at the current point.

Text Editing: Delete
^^^^^^^^^^^^^^^^^^^^

``(meep-delete-symbol-next ARG)``
   Kill the symbol forwards ARG times.

``(meep-delete-symbol-prev ARG)``
   Kill the symbol backwards ARG times.

``(meep-delete-same-syntax-next ARG)``
   Kill the syntax-spans forwards ARG times.

``(meep-delete-same-syntax-prev ARG)``
   Kill the syntax-spans backwards ARG times.

``(meep-delete-same-syntax-or-symbol-next ARG)``
   Kill the syntax-spans or symbols forwards ARG times.

``(meep-delete-same-syntax-or-symbol-prev ARG)``
   Kill the syntax-spans or symbols backwards ARG times.

Text Editing: Character Delete/Backspace
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-delete-char-next ARG)``
   Delete the next character ARG times.
   This deletion is not sent to the ``kill-ring``.

``(meep-delete-char-prev ARG)``
   Delete the previous character ARG times.
   This deletion is not sent to the ``kill-ring``.

Text Editing: Character Delete/Backspace (Ring)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Character level delete which has it's own kill-ring.
This can be useful for quickly relocating characters.

Note that this is only accumulated on successive calls.

``(meep-delete-char-ring-next ARG)``
   Delete the next character ARG times.
   This deletion is sent to the ``meep-delete-char-ring``.

``(meep-delete-char-ring-prev ARG)``
   Delete the previous character ARG times.
   This deletion is sent to the ``meep-delete-char-ring``.

``(meep-delete-char-ring-yank ARG)``
   Yank from the delete character ring ARG times.

Text Editing: Character Operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-char-replace CH)``
   Read a character CH and replace the selection with it.

``(meep-char-insert CH ARG)``
   Read a character CH and insert it or replace the active region.
   Inset ARG times.

Text Editing: Surround Insert/Delete
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-char-surround-insert CH ARG)``
   Read a character CH and surround the selection with it.
   Inset ARG times.

   When there is no active region, surround the current point.

``(meep-char-surround-insert-lines CH ARG)``
   Read a character CH and surround the selected lines with it.
   Inset ARG times.

   When multiple lines are are in the active region,
   surround each line individually.
   When there is no active region, surround the current line.

Text Editing: Join Lines
^^^^^^^^^^^^^^^^^^^^^^^^

Line joining with support for left-trimming code-comments,
so this may be used to conveniently joining lines in code.

For an example of languages using ``#`` prefixed comments (Python or Shell):

.. code-block:: python

   # Example block.
   # Next line.

Joined at the first line removes the leading ``#``:

.. code-block:: python

   # Example block. Next line.

And for C-family languages:

.. code-block:: c

   /* Example block.
    * next line. */

Joined at the first line removes the leading ``*``:

.. code-block:: c

   /* Example. Block. Next line. */

``(meep-join-line-next ARG)``
   Join the next line to this one ARG times.

``(meep-join-line-prev ARG)``
   Join the previous line to this one ARG times.

Text Editing: Tab Wrapper
^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-indent-rigidly)``
   Indent the active region or the current line.

State: Insert
^^^^^^^^^^^^^

``(meep-insert)``
   Enter insert mode.

``(meep-insert-append)``
   Enter insert mode.

``(meep-insert-at-last)``
   Enter insert mode where insert mode was last exited.

``(meep-insert-overwrite)``
   Enter insert mode & enable ``overwrite-mode`` while inserting.

``(meep-insert-change)``
   Change the region, entering insert mode.
   The region may be implied, see ``meep-command-is-mark-set-on-motion-any``.

``(meep-insert-change-lines)``
   Change the region, entering insert mode.
   The region may be implied, see ``meep-command-is-mark-set-on-motion-any``.

``(meep-insert-into-last)``
   Insert text into last insert point.

``(meep-insert-open-above)``
   Open a newline above and switch to INSERT state.

``(meep-insert-open-below)``
   Open a newline below and switch to INSERT state.

``(meep-insert-line-beginning)``
   Move the line indentation start and switch to INSERT state.

``(meep-insert-line-end)``
   Move the line end and switch to INSERT state.

Clipboard: System Only
^^^^^^^^^^^^^^^^^^^^^^

These commands only wrap the "systems" clipboard,
without mixing the kill-ring or primary clipboard - for predictable results.

``(meep-clipboard-only-copy)``
   Copy the region using the clipboard-only.

``(meep-clipboard-only-cut)``
   Cut the region using the clipboard-only.

``(meep-clipboard-only-cut-line)``
   Cut the whole line using the clipboard-only.

``(meep-clipboard-only-yank-with-indent)``
   Yank from the clipboard-only, replacing the region (indenting the content).

``(meep-clipboard-only-yank)``
   Yank from the clipboard-only, replacing the region (as lines).

Clipboard: Kill Ring
^^^^^^^^^^^^^^^^^^^^

These commands wrap the kill-ring, without mixing the system clipboard.

Note that line-wise cut/copy is stored in the kill-ring.
Yanking (pasting) a line-wise region yanks from the line beginning.

So line-wise kill & yank can be used to operate on lines without the need
to place the point at the beginning of the line.

If you wish to override this behavior, you may activate the region with an empty range,
since an active region always defines the range.

Note that rect-wise regions are also stored in the kill-ring and paste from the top-left.

``(meep-clipboard-killring-cut)``
   Kill the current region.
   The region need not be active.

``(meep-clipboard-killring-copy)``
   Add the current region to the ``kill-ring``.
   The region need not be active.

``(meep-clipboard-killring-cut-line)``
   Kill the whole line.

``(meep-clipboard-killring-copy-line)``
   Copy the whole line to the kill ring.

``(meep-clipboard-killring-yank-pop-stack ARG)``
   Yank from the ARG'th item from the ``kill-ring`` which is rotated.

   Rotating the kill ring means that you may kill multiple items,
   then conveniently yank those items afterwards.

``(meep-clipboard-killring-yank ARG)``
   Yank from the ARG'th item from the ``kill-ring``.
   The region is replaced (when active).

Clipboard: Register
^^^^^^^^^^^^^^^^^^^

``(meep-clipboard-register-actions)``
   Set the pre-defined register to use for ``meep-clipboard-register-*`` commands.

   Uses the ``meep-clipboard-register-map`` key-map.

``(meep-clipboard-register-copy)``
   Copy to pre-defined register.

``(meep-clipboard-register-cut)``
   Cut to pre-defined register.

``(meep-clipboard-register-yank)``
   Yank from pre-defined register.

``(meep-clipboard-register-yank-lines)``
   Yank from pre-defined register as lines.

Keypad Mode
^^^^^^^^^^^

Support entering a sequence of keys without the need to hold modifiers, see:
`keypad mode <https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#keypad>`__.

``(meep-keypad)``
   Begin entering a key sequence.

.. END VARIABLES


.. BEGIN COMMANDS


.. END COMMANDS
