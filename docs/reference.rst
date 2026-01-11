
##############
MEEP Reference
##############


.. BEGIN VARIABLES

Custom Variables
----------------

``meep-mark-set-on-motion``: ``t``
   When non-nil, motion commands set the mark.

``meep-state-insert-register``: ``94``
   The register set when leaving insert mode.

   Used by ``meep-insert-at-last`` which will enter insert mode at this location.

``meep-move-comment-skip-space``: ``t``
   When navigating comment bounds, skip leading/trailing space.

``meep-move-comment-skip-repeated``: ``t``
   When navigating comment bounds, skip repeated characters.

``meep-symmetrical-chars``: ``(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">"))``
   List of character matches.

   Used for ``meep-region-mark-bounds-of-char-inner`` and
   ``meep-region-mark-bounds-of-char-outer``.

``meep-match-bounds-of-char-contextual``: ``(("\"" . "\"") ("'" . "'") ("`" . "`") ("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">") ("“" . "”") ("‘" . "’"))``
   List of boundary string matches used for automatically marking bounds.

   While this is typically used for brackets and quotes,
   multi-character pairs are also supported.

   Used for ``meep-region-mark-bounds-of-char-inner-contextual`` and
   ``meep-region-mark-bounds-of-char-outer-contextual``.

``meep-bounds-commands``: ``((112 meep-move-to-bounds-of-paragraph-inner "paragraph inner") (80 meep-move-to-bounds-of-paragraph "paragraphs") (99 meep-move-to-bounds-of-comment-inner "comment inner") (67 meep-move-to-bounds-of-comment "comment") (115 meep-move-to-bounds-of-string-inner "string inner") (83 meep-move-to-bounds-of-string "string") (108 meep-move-to-bounds-of-line-inner "line inner") (76 meep-move-to-bounds-of-line "line") (86 meep-move-to-bounds-of-visual-line-inner "visual line inner") (118 meep-move-to-bounds-of-visual-line "visual line") (100 meep-move-to-bounds-of-defun-inner "defun inner") (68 meep-move-to-bounds-of-defun "defun") (46 meep-move-to-bounds-of-sentence-inner "sentence inner") (62 meep-move-to-bounds-of-sentence "sentence"))``
   List of commands for bounds movement.
   Each element is (key function description).

``meep-region-swap-imply-region``: ``t``
   Imply the region from the length of the secondary region.

   - When the region is on a single line:
     The text after point implies the selection.
   - When a line-wise region is used:
     The same number of lines after point is used (ignoring line length).
   - When a rectangle-wise region is used:
     The text after and lines below are used to create the implied selection.

``meep-isearch-activate-mark``: ``t``
   ISEARCH activates the mark (transient).
   So motion drops the selection.

   Useful for pasting while stepping over search results.


Other Variables
---------------

``meep-state-region-elem``: ``nil``
   Supported values are nil or ``line-wise``.

   Note that line-wise navigation is not enforced;
   this is a hint that commands may use.

``meep-mark-adjust``: ``nil``
   The previous position for commands that don’t set mark-on-motion.

   This must be set by commands that pass the:
   ``meep-command-is-mark-set-on-motion-adjust`` test.

``meep-mark-set-on-motion-override``: ``nil``
   Override variable for suppressing mark-on-motion.

   When let-bound to t, motions can be repeated without setting the mark.
   Must never be set directly.

``meep-move-by-sexp-over-depth``: ``nil``
   The target depth when moving over S-expressions.
   Used to maintain the depth even when the motion causes
   navigation to move to an outer scope.

   Only used between successive
   ``meep-move-by-sexp-over-next`` and ``meep-move-by-sexp-over-prev`` calls.

``meep-delete-char-ring``: ``nil``
   Ring of deleted characters.
   Used by ``meep-delete-char-ring-next``, ``meep-delete-char-ring-prev``,
   and ``meep-delete-char-ring-yank``.

``meep-clipboard-register-map``: ``(keymap)``
   Key-map for register clipboard actions.

   Used by ``meep-clipboard-register-actions``.


Commands
--------

Motion: Symbol/Word
^^^^^^^^^^^^^^^^^^^

Command properties:
Commands may have a `meep' property which is expected to be a PLIST of properties.

:mark-on-motion
   - t: Mark on motion.
   - 'adjust: Adjust the previous motion.

     This is used so a motion can be adjusted,
     without breaking the chain of commands used to repeat an action.
     So it's possible to perform a motion and any number of adjustments before an edit-command.

     When repeating the motion, adjustments and edit will all be repeated.
     Single character motion commands take advantage of this.

   - nil: don't mark on motion (same as missing).

:mark-on-motion-no-repeat
   - t: Motions that should not be repeated, such as search.
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
   Move point to the beginning of the next symbol, ARG times.

``(meep-move-word-prev ARG)``
   Move point to the beginning of the previous word, ARG times.

``(meep-move-word-next-end ARG)``
   Move to the end of the next word, ARG times.

``(meep-move-word-prev-end ARG)``
   Move to the end of the previous word, ARG times.

``(meep-move-word-next ARG)``
   Move point to the beginning of the next word, ARG times.

Motion: Same Syntax
^^^^^^^^^^^^^^^^^^^

``(meep-move-same-syntax-prev ARG)``
   Move back over characters with the same syntax class, ARG times.

``(meep-move-same-syntax-next ARG)``
   Move forward over characters with the same syntax class, ARG times.

Motion: Same Syntax or Symbol
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Skip over the same syntax or entire symbols.

``(meep-move-same-syntax-or-symbol-prev ARG)``
   Move back over characters with the same syntax class or symbols, ARG times.

``(meep-move-same-syntax-or-symbol-next ARG)``
   Move forward over characters with the same syntax class or symbols, ARG times.

Motion: Same Syntax and Space
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Skip over the same syntax with changes to behavior for surrounding space,
where space at the bounds of text is skipped over, matching
how this is handled for skipping words and symbols.

``(meep-move-same-syntax-and-space-prev ARG)``
   Move back a syntax-and-space, ARG times.

``(meep-move-same-syntax-and-space-next ARG)``
   Move to the end of the next syntax-and-space, ARG times.

``(meep-move-same-syntax-and-space-next-end ARG)``
   Move to the end of the next syntax-and-space, ARG times.

Motion: Line
^^^^^^^^^^^^

``(meep-move-line-beginning ARG)``
   Move to the beginning of the current line.
   Move to the end when ARG is negative.

``(meep-move-line-end ARG)``
   Move to the end of the current line.
   Move to the beginning when ARG is negative.

``(meep-move-line-non-space-beginning ARG)``
   Move to the first non-blank character of the line.
   A negative ARG moves to the end.

``(meep-move-line-non-space-end ARG)``
   Move to the end of the line, ignoring trailing whitespace.
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
   Jump to the next S-expression, ARG times.

``(meep-move-by-sexp-any-prev ARG)``
   Jump to the previous S-expression, ARG times.

``(meep-move-by-sexp-over-next ARG)``
   Move to the next S-expression at the same depth, ARG times.

``(meep-move-by-sexp-over-prev ARG)``
   Move to the previous S-expression at the same depth, ARG times.

``(meep-move-by-sexp-out-prev &optional ARG)``
   Jump out of the current S-expression to the opening bracket, ARG times.

``(meep-move-by-sexp-out-next &optional ARG)``
   Jump out of the current S-expression to the closing bracket, ARG times.

``(meep-move-matching-bracket-outer)``
   Jump to the matching outer bracket.
   When not at the bounds, jump to the start (when enclosed in brackets).

   Return non-nil when point was moved.

Motion: Matching Characters
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Jump to the opposite character - bracket, quote or comment bounds
depending on the command.

When no matching character is found,
jump to the start of the surrounding characters (if found).

``(meep-move-matching-bracket-inner)``
   Jump to the matching inner bracket.
   When not at the bounds, jump to the start (when enclosed in brackets).

   Return non-nil when point was moved.

``(meep-move-matching-syntax-outer)``
   Move to the outer matching string/comment syntax.
   When not at the bounds, jump to the start (when in a string/comment).

   Return nil if no matching syntax was found.

``(meep-move-matching-syntax-inner)``
   Move to the inner matching string/comment syntax.
   When not at the bounds, jump to the start (when in a string/comment).

   Return nil if no matching syntax was found.

``(meep-move-matching-contextual-outer)``
   Move to the matching character.
   When not at the bounds, jump to the start.

``(meep-move-matching-contextual-inner)``
   Move to the matching character.
   When not at the bounds, jump to the start.

Motion: Find and Till
^^^^^^^^^^^^^^^^^^^^^

``(meep-move-find-char-on-line-at-next ARG CH)``
   Find the next ARG char CH, read from minibuffer.

``(meep-move-find-char-on-line-at-prev ARG CH)``
   Find the previous ARG char CH, read from minibuffer.

``(meep-move-find-char-on-line-till-next ARG CH)``
   Find till the next char CH, ARG times.

``(meep-move-find-char-on-line-till-prev ARG CH)``
   Find till the previous char CH, ARG times.

``(meep-move-find-char-on-line-repeat-at-next ARG)``
   Repeat find ARG times forwards.

``(meep-move-find-char-on-line-repeat-at-prev ARG)``
   Repeat find ARG times backwards.

``(meep-move-find-char-on-line-repeat-till-next ARG)``
   Repeat find-till ARG times forwards.

``(meep-move-find-char-on-line-repeat-till-prev ARG)``
   Repeat find-till ARG times backwards.

Region Mark: Bounds in Character
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Support convenient marking of a region in character bounds.
This works by prompting for a character which is then scanned in both directions,
marking the region in the bounds when it is found.

Notes:

- Both inner/outer commands are available,
  in case you wish to manipulate the region including/excluding the characters.
- Entering bracket characters uses matching brackets.
  This is customizable with the ``meep-symmetrical-chars`` variable.
- Entering an opening ``(`` bracket marks the region inside: ``( ... )``.
- Entering a closing ``)`` bracket marks the region inside: ``) ... (``.
- A "contextual" version of this function has been implemented which marks the nearest region.
  This is customizable with the ``meep-match-bounds-of-char-contextual`` variable.

``(meep-region-mark-bounds-of-char-inner CH ARG)``
   Mark the inner bounds of CH, ARG times.
   A negative ARG positions point at the end of the region.

   Note that pressing Return instead of a character performs a contextual mark,
   finding the closest pair, see: ``meep-match-bounds-of-char-contextual``.

``(meep-region-mark-bounds-of-char-outer CH ARG)``
   Mark the outer bounds of CH, ARG times.
   A negative ARG positions point at the end of the region.

   Note that pressing Return instead of a character performs a contextual mark,
   finding the closest pair, see: ``meep-match-bounds-of-char-contextual``.

``(meep-region-mark-bounds-of-char-contextual-inner ARG)``
   Mark the inner bounds of the nearest character pairs, ARG times.
   A negative ARG positions point at the end of the region.

   Character pairs are detected using: ``meep-match-bounds-of-char-contextual``.

``(meep-region-mark-bounds-of-char-contextual-outer ARG)``
   Mark the outer bounds of the nearest boundary pairs, ARG times.
   A negative ARG positions point at the end of the region.

   Bounds are detected using: ``meep-match-bounds-of-char-contextual``.

Motion: Bounds
^^^^^^^^^^^^^^

``(meep-move-to-bounds-of-sentence ARG &optional INNER)``
   Move to the sentence start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.

``(meep-move-to-bounds-of-sentence-inner ARG)``
   Move to the inner sentence start/end (start when ARG is negative).

``(meep-move-to-bounds-of-paragraph ARG &optional INNER)``
   Move to the paragraph start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.

``(meep-move-to-bounds-of-paragraph-inner ARG)``
   Move to the inner paragraph start/end (start when ARG is negative).

``(meep-move-to-bounds-of-comment ARG &optional INNER)``
   Move to the comment start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.

``(meep-move-to-bounds-of-comment-inner ARG)``
   Move to the comment inner start/end (start when ARG is negative).

``(meep-move-to-bounds-of-string ARG &optional INNER)``
   Move to the string start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.

``(meep-move-to-bounds-of-string-inner ARG)``
   Move to the string inner start/end (start when ARG is negative).

``(meep-move-to-bounds-of-defun ARG &optional INNER)``
   Move to the function start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.

``(meep-move-to-bounds-of-defun-inner ARG)``
   Move to the inner function start/end (start when ARG is negative).

``(meep-move-to-bounds-of-line ARG &optional INNER)``
   Move to the line start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.

``(meep-move-to-bounds-of-line-inner ARG)``
   Move to the inner line start/end (start when ARG is negative).

``(meep-move-to-bounds-of-visual-line ARG &optional INNER)``
   Move to the visual-line start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.

``(meep-move-to-bounds-of-visual-line-inner ARG)``
   Move to the inner visual-line start/end (start when ARG is negative).

``(meep-move-to-bounds-of-thing-beginning ARG)``
   Move to inner bounds of thing (beginning).
   Move to the end with a negative ARG.

``(meep-move-to-bounds-of-thing-end ARG)``
   Move to inner bounds of thing (end).
   Move to the beginning with a negative ARG.

Selection/Region: Primitive
^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-region-enable)``
   Enable the active region.

   The mark is moved to point to begin a new selection.
   If you wish to activate the region between the existing point and mark see:
   ``meep-region-activate-and-reverse`` and ``meep-region-activate-or-reverse``.

``(meep-region-activate-or-reverse)``
   Activate the region without moving the mark.

   Otherwise exchange point and mark when the region is already active.
   See: ``meep-region-activate-and-reverse``.

``(meep-region-disable)``
   Disable the active region.

   The mark is not moved, the region can be restored
   via ``meep-region-activate-or-reverse`` or ``meep-region-activate-and-reverse``.

``(meep-region-toggle)``
   Toggle the active region.

   When the region is transient (where motion would clear it),
   this operation makes it stay active, running again clears it.

``(meep-region-activate-and-reverse)``
   Exchange point and mark, activating the region.

   To first activate the region without exchanging point and mark:
   See: ``meep-region-activate-or-reverse``.

   Note that this wraps Emacs built-in: ``exchange-point-and-mark``.

``(meep-region-activate-and-reverse-motion)``
   Exchange point and mark, activating the region.

Selection/Region: Secondary Selection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-region-swap)``
   Swap the contents of the primary and secondary region.

   When ``meep-region-swap-imply-region`` is non-nil,
   only the secondary region needs to be set.

``(meep-region-to-secondary-selection)``
   Create a secondary selection from the current region.

Selection/Region: Line Selection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-region-expand-to-line-bounds)``
   Expand the region to the line bounds.

   ``meep-state-region-elem`` is set to \='line-wise which commands may
   use to maintain line-based selection.

Selection/Region: Expand/Contract
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Expand/contract the regions.
Initially expanding is performed in both directions until
a syntax mismatch is encountered, then expanding is only performed at the point.

This allows for expanding across surrounding symmetrical characters which can be useful.

``(meep-region-syntax-expand ARG)``
   Expand on matching syntax table elements ARG times.

   When there is no active region, activate and expand the region.
   This can be used to quickly mark symbols or blocks of contiguous syntax,
   including blank-space.

``(meep-region-syntax-contract ARG)``
   Contract matching syntax table ARG times.

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
   Start or stop recording a keyboard macro to a register.

``(meep-register-jump-to ARG)``
   Jump to a register or execute a macro stored in a register, ARG times.

ISEARCH Wrapper
^^^^^^^^^^^^^^^

Support searching in both directions as well as
searching based on the active region.

``(meep-isearch-regexp-next)``
   Search forward for a regexp.

``(meep-isearch-regexp-prev)``
   Search backward for a regexp.

``(meep-isearch-repeat-next ARG)``
   Repeat ISEARCH forwards ARG times.

``(meep-isearch-repeat-prev ARG)``
   Repeat ISEARCH backwards ARG times.

``(meep-isearch-at-point-next ARG)``
   Search forwards for the symbol or region at point.
   Repeat the search ARG times.

``(meep-isearch-at-point-prev ARG)``
   Search backwards for the symbol or region at point.
   Repeat the search ARG times.

Text Editing: Delete
^^^^^^^^^^^^^^^^^^^^

``(meep-delete-symbol-next ARG)``
   Delete the symbol forwards ARG times.

``(meep-delete-symbol-prev ARG)``
   Delete the symbol backwards ARG times.

``(meep-delete-same-syntax-next ARG)``
   Delete characters with the same syntax class forwards, ARG times.

``(meep-delete-same-syntax-prev ARG)``
   Delete characters with the same syntax class backwards, ARG times.

``(meep-delete-same-syntax-or-symbol-next ARG)``
   Delete characters with the same syntax class or symbols forwards, ARG times.

``(meep-delete-same-syntax-or-symbol-prev ARG)``
   Delete characters with the same syntax class or symbols backwards, ARG times.

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

Character level delete which has its own kill-ring.
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

``(meep-delete-char-ring-yank-no-pop ARG)``
   Yank from the delete character ring ARG times.

   Leave the char-ring unmodified afterwards.

Text Editing: Character Operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-char-replace CH)``
   Read a character CH and replace the selection with it.

``(meep-char-insert CH ARG)``
   Read a character CH and insert it or replace the active region.
   Insert ARG times.

Text Editing: Surround Insert/Delete
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-char-surround-insert CH ARG)``
   Read a character CH and surround the selection with it.
   Insert ARG times.

   When there is no active region, surround the current point.

``(meep-char-surround-insert-lines CH ARG)``
   Read a character CH and surround the selected lines with it.
   Insert ARG times.

   When multiple lines are in the active region,
   surround each line individually.
   When there is no active region, surround the current line.

Text Editing: Join Lines
^^^^^^^^^^^^^^^^^^^^^^^^

Line joining with support for left-trimming code-comments,
so this may be used to conveniently join lines in code.

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

   /* Example block. next line. */

``(meep-join-line-next ARG)``
   Join the next line to this one ARG times.

``(meep-join-line-prev ARG)``
   Join the previous line to this one ARG times.

Text Editing: Shrink Space
^^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-space-shrink-contextual)``
   Remove blank space contextually.
   - When on a blank line, remove surrounding blank lines.
   - When on a blank character remove multiple blank characters.
   - Otherwise, when over a paragraph, trim the bounds to a single blank line.

   Return non-nil when a change was made.

Text Editing: Transpose
^^^^^^^^^^^^^^^^^^^^^^^

``(meep-transpose ARG)``
   Transpose the previous motion ARG times.
   This can be used to transpose words if the previous motion was over words.
   Transposing lines and characters is also supported.

Text Editing: Tab Wrapper
^^^^^^^^^^^^^^^^^^^^^^^^^

``(meep-indent-rigidly)``
   Indent the active region or the current line.

State: Insert
^^^^^^^^^^^^^

``(meep-insert)``
   Enter insert mode.

``(meep-insert-append)``
   Enter insert mode after the cursor, or at the opposite end of the region.

``(meep-insert-at-last)``
   Enter insert mode at the position it was last exited.

``(meep-insert-overwrite)``
   Enter insert mode and enable ``overwrite-mode`` while inserting.

``(meep-insert-change)``
   Change the region, entering insert mode.
   The region may be implied, see ``meep-command-is-mark-set-on-motion-any``.

``(meep-insert-change-lines)``
   Change the region, entering insert mode.
   The region may be implied, see ``meep-command-is-mark-set-on-motion-any``.

``(meep-insert-into-last-copy)``
   Insert text into last insert point (copying it).

   When there is no active region, the symbol at point is used.

``(meep-insert-into-last-move)``
   Insert text into last insert point (moving it).

   When there is no active region, the symbol at point is used.

``(meep-insert-open-above)``
   Open a newline above and switch to INSERT state.

``(meep-insert-open-below)``
   Open a newline below and switch to INSERT state.

``(meep-insert-line-beginning)``
   Move to the line indentation start and switch to INSERT state.

``(meep-insert-line-end)``
   Move to the line end and switch to INSERT state.

Clipboard: System Only
^^^^^^^^^^^^^^^^^^^^^^

These commands only wrap the system's clipboard,
without mixing the kill-ring or primary clipboard - for predictable results.

``(meep-clipboard-only-copy)``
   Copy the region to the system clipboard.

``(meep-clipboard-only-cut)``
   Cut the region to the system clipboard.

``(meep-clipboard-only-cut-line)``
   Cut the whole line to the system clipboard.

``(meep-clipboard-only-yank-with-indent)``
   Yank from the system clipboard, replacing the region (indenting the content).

``(meep-clipboard-only-yank)``
   Yank from the system clipboard, replacing the region.

Clipboard: Kill Ring
^^^^^^^^^^^^^^^^^^^^

These commands wrap the kill-ring, without mixing the system clipboard.

Note that line-wise cut/copy is stored in the kill-ring.
Yanking (pasting) a line-wise region yanks from the line beginning.

So line-wise kill and yank can be used to operate on lines without the need
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
   Yank the ARG'th item from the ``kill-ring``, rotating it.

   Rotating the kill ring means you may kill multiple items,
   then conveniently yank those items afterwards.

``(meep-clipboard-killring-yank ARG)``
   Yank the ARG'th item from the ``kill-ring``.
   The region is replaced (when active).

Clipboard: Register
^^^^^^^^^^^^^^^^^^^

``(meep-clipboard-register-actions)``
   Set the pre-defined register to use for ``meep-clipboard-register-*`` commands.

   Use the ``meep-clipboard-register-map`` key-map.

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

Old Commands
^^^^^^^^^^^^

These will eventually be removed.
Report the new commands which should be used so users can upgrade.

``(meep-exchange-point-and-mark-motion)``
   Report that ``meep-region-activate-and-reverse-motion`` must be used instead.

``(meep-exchange-point-and-mark)``
   Report that ``meep-region-activate-and-reverse`` must be used instead.

.. END VARIABLES


.. BEGIN COMMANDS


.. END COMMANDS
