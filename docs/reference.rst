
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

``meep-repeat-fu-replay``: ``t``
   When non-nil, use real insert mode for rectangle editing.
   Instead of ``string-rectangle``, enter insert mode on the first line
   and record keystrokes.  On exit, replay the recorded keystrokes
   on all other lines in the rectangle.

   Requires ``repeat-fu-mode`` to be active; falls back to
   ``string-rectangle`` when it is not.

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

``meep-bounds-for-inner-comment``: ``nil``
   Spec for contracting outer comment bounds to inner.

   When non-nil, the value is a 2-element list ‘(FUNC ARGS)’.  Given
   outer comment bounds BOUNDS, the inner bounds are computed as

     (funcall FUNC ARGS BOUNDS)

   which should return a cons ‘(BEG . END)’ or nil if BOUNDS cannot
   be contracted.

   Defaults to nil; the spec is then read from the preset for the
   current ``major-mode`` (see the bundled ‘meep-preset-MODE.el’
   files).  An explicit user setting - buffer-local or global -
   takes precedence over the preset.

``meep-list-item-bounds``: ``nil``
   Spec for the ``list-item`` text object: bracket lists and their separators.

   The value is a list of entries, each of the form

     ((OPEN . CLOSE) SEPARATORS)

   OPEN and CLOSE are the bracket characters delimiting a list, and SEPARATORS is
   the list of that list’s separator strings, or t to split on runs of whitespace
   (e.g. Lisp, where items are space-separated):

     (((?\( . ?\)) (",")) ((?\{ . ?\}) (";")))
     (((?\( . ?\)) t))   ; whitespace-separated, e.g. Lisp

   Each bracket type defines its own separators, so different brackets may separate
   differently - in C/C++ ‘()’ splits on ‘,’ while ‘{}’ splits on ‘;’.  A bracket
   may appear in more than one entry to give a fallback chain: entries are tried in
   order and the first whose separators occur in the list wins, e.g. C/C++ braces
   holding either statements (‘;’) or an initializer (‘,’).

   Nesting is read from the buffer’s syntax tree, so a separator inside any deeper
   bracket, string or comment never splits a list item.  Each OPEN and CLOSE must
   therefore be a single character with paren syntax in the current mode (possibly
   via a syntax-table text property, as CC Mode does for C++ template ‘<’ / ‘>’).

   When nil, falls back to the preset for the current ``major-mode``, then to a spec
   generated from ``meep-symmetrical-chars`` (see ``meep--list-item-bounds-default``).

``meep-syntax-backend``: ``nil``
   Backend for locating the bracket pair enclosing point.

   - nil: choose automatically per buffer - ``syntax`` in ``prog-mode`` derivatives
     (whose syntax tables are reliable), ``text`` elsewhere (so prose and markup
     delimiters keep working).
   - ``text``: scan the buffer text.  Matches any configured delimiter, including
     multi-character markup, but counts brackets inside strings and comments.
   - ``syntax``: read the syntax tree (``syntax-ppss``).  Ignores brackets inside
     strings and comments and nests correctly, but applies only to single-character
     paren-syntax brackets - every other delimiter always scans text regardless of
     this setting (same-delimiter quotes and markup never consult it; multi-character
     and non-paren pairs fall back to ``text``).  Auto-detected quote and markup pairs
     are dropped from surround recognition, so surround-delete inside a top-level
     string with no enclosing bracket is a no-op (use ``text``, or configure the quote
     in ``meep-surround-pairs``, to delete a string’s own quotes).

   Affects the surround delete / replace verbs and the mark-bounds-of-char motions.

``meep-bounds-commands``: ``((112 meep-move-to-bounds-of-paragraph-inner "paragraph inner") (80 meep-move-to-bounds-of-paragraph "paragraphs") (99 meep-move-to-bounds-of-comment-inner "comment inner") (67 meep-move-to-bounds-of-comment "comment") (115 meep-move-to-bounds-of-string-inner "string inner") (83 meep-move-to-bounds-of-string "string") (108 meep-move-to-bounds-of-line-inner "line inner") (76 meep-move-to-bounds-of-line "line") (86 meep-move-to-bounds-of-visual-line-inner "visual line inner") (118 meep-move-to-bounds-of-visual-line "visual line") (100 meep-move-to-bounds-of-defun-inner "defun inner") (68 meep-move-to-bounds-of-defun "defun") (105 meep-move-to-bounds-of-list-item-inner "list item inner") (73 meep-move-to-bounds-of-list-item "list item") (46 meep-move-to-bounds-of-sentence-inner "sentence inner") (62 meep-move-to-bounds-of-sentence "sentence"))``
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

``meep-surround-alist``: ``((42 . bold) (47 . italic) (96 . code) (126 . strike))``
   Alist mapping a surround key to a semantic symbol.

   Each entry is ‘(KEY . SYMBOL)’ where KEY is the character read after the
   surround dispatch key and SYMBOL names the markup intent (e.g. ``bold``).  The
   symbol is resolved to concrete delimiters via ``meep-surround-pairs``, typically
   populated per ``major-mode`` by a preset.

   This layer is shared across modes so a key means the same intent everywhere,
   with the mode supplying the syntax.  A key absent from this alist is taken
   literally as a delimiter (paired via ``meep-symmetrical-chars``).

``meep-surround-pairs``: ``nil``
   Alist mapping a surround SYMBOL to its delimiter pair.

   Each entry is ‘(SYMBOL . SPEC)’ where SYMBOL matches a value in
   ``meep-surround-alist`` and SPEC is either:

     (OPEN . CLOSE)   two delimiter strings (single or multi-character), or
     FUNCTION         a function of no arguments returning such a cons
		      (used for prompted delimiters such as tags).

   When nil, falls back to the preset for the current ``major-mode``.  A mode that
   does not define a symbol simply has no surround for that key, rather than
   wrapping with the wrong characters.

   To add your own kind, bind a key to a new symbol in ``meep-surround-alist`` (e.g.
   ‘(?h . heading)’), then map that symbol to its pair here for each mode - in a
   ‘meep-preset-MODE.el’, a mode hook, or as a global default.  The symbol is
   arbitrary; nothing is special about the built-in ``bold`` / ``italic`` / ``code`` /
   ``strike``.

   A pair declared here is also recognized by delete and replace, even a
   single-character bracket the syntax table does not mark as one (e.g. ‘<’ ‘>’) -
   unlike the generic fall-back, which drops such brackets to avoid mistaking
   operators for them, see ``meep--surround-recognition-pairs``.

``meep-surround-mark-result``: ``nil``
   When non-nil, surround delete and replace mark the affected content.
   Point is left just inside the opening delimiter and the mark just inside the
   closing delimiter - without activating the region - so the operated-on content
   spans point..mark and can be re-selected (e.g. with ``meep-region-activate``).
   When nil, point and the mark are left where they were before the command.

   Applies to the region / point verbs only.  The line-wise and rectangle verbs
   operate on multiple disjoint spans with no single span to mark, so they are
   unaffected by this option.


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

``meep-text-object-alist``: ``((word ``:bounds-fn`` meep--bounds-of-word ``:bounds-step-fn`` meep--bounds-step-word ``:no-inner`` t) (symbol ``:bounds-fn`` meep--bounds-of-symbol ``:bounds-step-fn`` meep--bounds-step-symbol ``:no-inner`` t) (sentence ``:bounds-fn`` meep--bounds-of-sentence ``:bounds-step-fn`` meep--bounds-step-sentence) (paragraph ``:bounds-fn`` meep--bounds-of-paragraph ``:bounds-step-fn`` meep--bounds-step-paragraph) (comment ``:bounds-fn`` meep--bounds-of-comment ``:bounds-step-fn`` meep--bounds-step-comment) (comment-block ``:bounds-fn`` meep--bounds-of-comment-block ``:bounds-step-fn`` meep--bounds-step-comment-block) (string ``:bounds-fn`` meep--bounds-of-string ``:bounds-step-fn`` meep--bounds-step-string) (defun ``:bounds-fn`` meep--bounds-of-defun ``:bounds-step-fn`` meep--bounds-step-defun) (line ``:bounds-fn`` meep--bounds-of-line ``:bounds-step-fn`` meep--bounds-step-line) (visual-line ``:bounds-fn`` meep--bounds-of-visual-line ``:bounds-step-fn`` meep--bounds-step-visual-line) (list-item ``:bounds-fn`` meep--bounds-of-list-item ``:bounds-step-fn`` meep--bounds-step-list-item))``
   Alist mapping a text-object KIND to a plist of operations.
   Plist keys:
     ``:bounds-fn`` (INNER) -> BOUNDS
       Pure query: return ‘(BEG . END)’ of the object at point, or nil.
     ``:bounds-step-fn`` (INNER STEP AT-START) -> (LANDING-POS . REMAINING)
       Pure query (does not move point): scan STEP objects forward (backward
       if negative) and return the precise landing plus the signed count of
       steps that could not be advanced.
       LANDING-POS is the AT-START side (start when non-nil, end otherwise)
       of the target object’s INNER bounds;
       the motion dispatcher uses it directly without further lookup.
       Implementations may skip the bounds-normalization step when the
       underlying primitive’s natural landing already matches AT-START.
     ``:no-inner`` (optional)
       When non-nil, the kind has no meaningful inner variant (its ``:bounds-fn``
       ignores INNER, e.g. word, symbol).  Consumers (e.g. the mark commands)
       may generate only an ``-outer`` variant and skip the ``-inner`` one.
   Extension packages may extend this alist to register new kinds.

``meep-delete-char-ring``: ``nil``
   Ring of deleted characters.
   Used by ``meep-delete-char-ring-next``, ``meep-delete-char-ring-prev``,
   and ``meep-delete-char-ring-yank``.

``meep-clipboard-register-map``: ``(keymap)``
   Key-map for register clipboard actions.

   Used by ``meep-clipboard-register-actions``.

``meep-preset-variables``: ``(meep-bounds-for-inner-comment meep-match-bounds-of-char-contextual meep-list-item-bounds meep-surround-pairs)``
   Variables that meep presets are allowed to set.

   A bundled ‘meep-preset-MODE.el’ must restrict the keys of its
   returned alist to symbols in this list.  The contract is checked
   by the test suite, not enforced at runtime.


Commands
--------

Region Mark Commands (optional)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These commands must be explicitly loaded via::

   (require 'meep-region-mark)

``(meep-region-mark-line-outer)``
   Mark outer line at point.

``(meep-region-mark-sentence-outer)``
   Mark outer sentence at point.

``(meep-region-mark-list-item-outer)``
   Mark outer list-item at point.

``(meep-region-mark-list-item-inner)``
   Mark inner list-item at point.

``(meep-region-mark-comment-outer)``
   Mark outer comment at point.

``(meep-region-mark-visual-line-outer)``
   Mark outer visual-line at point.

``(meep-region-mark-symbol-outer)``
   Mark outer symbol at point.

``(meep-region-mark-defun-inner)``
   Mark inner defun at point.

``(meep-region-mark-string-inner)``
   Mark inner string at point.

``(meep-region-mark-line-inner)``
   Mark inner line at point.

``(meep-region-mark-sentence-inner)``
   Mark inner sentence at point.

``(meep-region-mark-comment-inner)``
   Mark inner comment at point.

``(meep-region-mark-string-outer)``
   Mark outer string at point.

``(meep-region-mark-comment-block-outer)``
   Mark outer comment-block at point.

``(meep-region-mark-word-outer)``
   Mark outer word at point.

``(meep-region-mark-paragraph-outer)``
   Mark outer paragraph at point.

``(meep-region-mark-visual-line-inner)``
   Mark inner visual-line at point.

``(meep-region-mark-paragraph-inner)``
   Mark inner paragraph at point.

``(meep-region-mark-comment-block-inner)``
   Mark inner comment-block at point.

``(meep-region-mark-defun-outer)``
   Mark outer defun at point.

Motion: Symbol/Word
^^^^^^^^^^^^^^^^^^^

Command properties:
Commands may have a ``meep`` property which is expected to be a PLIST of properties.

``:mark-on-motion``
   - t: Mark on motion.
   - 'adjust: Adjust the previous motion.

     This is used so a motion can be adjusted,
     without breaking the chain of commands used to repeat an action.
     So it's possible to perform a motion and any number of adjustments before an edit-command.

     When repeating the motion, adjustments and edit will all be repeated.
     Single character motion commands take advantage of this.

   - nil: don't mark on motion (same as missing).

``:mark-on-motion-no-repeat``
   - t: Motions that should not be repeated, such as search.
     (used by repeat-fu).
``:digit-repeat``
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

Motion: List Item
^^^^^^^^^^^^^^^^^

Useful for navigating over function arguments,
but can be used for stepping over other kinds of list-items.

Especially useful for transposing arguments as it properly handles
multi-line arguments, arguments mixed with comments and arguments that
themselves contain lists/function calls.

``(meep-move-list-item-prev ARG)``
   Move point to the beginning of the (previous) list item, ARG times.

``(meep-move-list-item-next-end ARG)``
   Move to the end of the current list item, ARG times.

``(meep-move-list-item-prev-end ARG)``
   Move to the end of the previous list item, ARG times.

``(meep-move-list-item-next ARG)``
   Move point to the beginning of the next list item, ARG times.

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
   Move to the end of the line, ignoring trailing blank-space.
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

``(meep-move-to-bounds-of-list-item ARG &optional INNER)``
   Move to the list item start/end (start when ARG is negative).
   When INNER is non-nil, move to the inner bound.
   Recognized brackets and separators come from ``meep-list-item-bounds``.

``(meep-move-to-bounds-of-list-item-inner ARG)``
   Move to the inner list item start/end (start when ARG is negative).

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

``(meep-region-enable-rectangle)``
   Enable rectangle mark mode.

``(meep-region-toggle-rectangle)``
   Toggle rectangle mark mode.

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
   Return non-nil on success.

``(meep-isearch-repeat-prev ARG)``
   Repeat ISEARCH backwards ARG times.
   Return non-nil on success.

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

Text Editing: Surround
^^^^^^^^^^^^^^^^^^^^^^

Surround is just commands the user binds directly - separate add / replace /
delete verbs, plus their line-wise variants - the same as the rest of the
keymap.  Each verb reads at most one further key, the delimiter; only
``meep-surround-delete-at-point`` (and its line-wise variant) reads none.

That delimiter read is a real keymap, built at run-time from the buffer's
``meep-surround-pairs`` (see ``meep--surround-make-delimiter-map``) and installed
transiently (``meep--surround-set-keymap``), so it always reflects the current
buffer and the echo leads with the verb's prompt.  Every key in the map routes
to the verb's named event command (see ``meep--surround-event-command``), which
resolves the invoking key with ``meep--surround-from-event``; the verb lives in
the binding, so one command resolves any delimiter (alias, literal,
multi-character, or ``RET`` to prompt).  The one map serves a key-bound verb and a
direct ``M-x`` call alike - there is no prefix keymap to dispatch through.

Resolving the delimiter from the event - rather than capturing it in a closure -
is what keeps each a *named* command, so ``repeat-fu`` can record and replay the
gesture.  The dispatch commands below rely on this and do not restate it.

By-type surround is not a separate dispatch: it is the same verbs bound directly.
``meep-surround-replace-by-type`` replaces and ``meep-surround-delete`` deletes, each
reading a source delimiter that names which pair *type* to act on (the nearest
pair of that type, skipping closer pairs of other types).  The default binds them
under a plain ``s t`` / ``s T`` prefix.

``(meep-surround-add)``
   Read a delimiter, then surround the region with it.
   Reads the delimiter through a run-time keymap of the buffer's ``meep-surround-pairs``
   installed transiently, whether called by key or ``M-x``.  A numeric prefix is the
   repeat count; the target is the active or implied region, see
   ``meep--region-or-mark-bounds``.

``(meep-surround-add-lines)``
   Read a delimiter, then surround each line's content with it.
   The line-wise variant of ``meep-surround-add``.

``(meep-surround-add-event ARG)``
   Surround the region with the delimiter named by the invoking key, ARG times.
   A direct shortcut for ``meep-surround-add`` followed by that delimiter: bind a
   delimiter key to this and it wraps with that delimiter immediately, with no
   separate delimiter read.  The delimiter is ``last-command-event``, resolved by
   ``meep--surround-from-event``; the target is the active or implied region, as for
   ``meep-surround-add``.

``(meep-surround-add-lines-event ARG)``
   Surround each line's content with the delimiter named by the invoking key.
   Wrap ARG times.  The line-wise variant of ``meep-surround-add-event``.

``(meep-surround-replace-event ARG)``
   Replace the surrounding pair with the delimiter named by the invoking key.
   Replace ARG times (the nesting depth).  The replace counterpart of
   ``meep-surround-add-event``: the delimiter is ``last-command-event``, resolved by
   ``meep--surround-from-event``.  Routed to by the run-time delimiter keymap, and
   bindable directly to a delimiter key.

``(meep-surround-replace-lines-event ARG)``
   Replace each line's surrounding pair with the delimiter named by the invoking key.
   The line-wise variant of ``meep-surround-replace-event``.

``(meep-surround-delete-event ARG)``
   Delete the surrounding pair of the type named by the invoking key, ARG times.
   The by-type delete counterpart of ``meep-surround-add-event``: the delimiter is
   ``last-command-event``, resolved by ``meep--surround-from-event``, and names which
   pair type to strip - a closer pair of another type is skipped.  To strip the
   nearest pair of any type with no read, use ``meep-surround-delete-at-point``.

``(meep-surround-delete-lines-event ARG)``
   Delete each line's surrounding pair of the type named by the invoking key.
   The line-wise variant of ``meep-surround-delete-event``.

``(meep-surround-replace)``
   Read a delimiter, then replace the surrounding pair to it.
   A numeric prefix is the nesting depth.  Reads the delimiter through a run-time
   keymap of the buffer's ``meep-surround-pairs`` installed transiently, whether called
   by key or ``M-x``.

``(meep-surround-replace-lines)``
   Read a delimiter, then replace each line's surrounding pair to it.
   The line-wise variant of ``meep-surround-replace``.

``(meep-surround-delete)``
   Read a delimiter, then delete the surrounding pair of that type.
   Reading a delimiter parallels ``meep-surround-replace``; here it names which pair
   type to strip, so only a surrounding pair of that type is removed - a closer pair
   of another type is skipped, as in the by-type surround flow (``s t t``).  A numeric
   prefix takes the Nth enclosing pair of the type.  To strip the nearest pair of any
   type with no prompt, use ``meep-surround-delete-at-point``.

``(meep-surround-delete-lines)``
   Read a delimiter, then delete each line's surrounding pair of that type.
   A numeric prefix strips that many nested layers per line.  The line-wise variant
   of ``meep-surround-delete``.

``(meep-surround-delete-at-point ARG)``
   Delete the delimiters surrounding the region or point, ARG times.
   No delimiter is read - the surrounding pair is found contextually (innermost when
   ARG is 1, the ARG-th enclosing pair otherwise).  To choose which delimiter type to
   strip, use ``meep-surround-delete``.

``(meep-surround-delete-lines-at-point ARG)``
   Delete each line's outermost surrounding delimiters, ARG times.
   The line-wise variant of ``meep-surround-delete-at-point``.
   When a region is active, every line it spans is processed; lines without a
   surrounding pair are skipped.  ARG peels that many nested layers per line.

``(meep-surround-region-activate-event ARG)``
   Select the content of the surrounding pair of the type named by the invoking key.
   The region-activate counterpart of ``meep-surround-delete-event``: the delimiter is
   ``last-command-event``, resolved by ``meep--surround-from-event``, and names which pair
   type to select - a closer pair of another type is skipped.  ARG is the nesting
   depth.  To select the nearest pair of any type with no read, use
   ``meep-surround-region-activate-at-point``.

``(meep-surround-region-activate)``
   Read a delimiter, then activate the region inside the surrounding pair of that type.
   Reading a delimiter parallels ``meep-surround-delete``; here it names which pair type
   to select, so the region is activated over the content of the nearest surrounding
   pair of that type - a closer pair of another type is skipped.  A numeric prefix
   takes the Nth enclosing pair.  To select the nearest pair of any type with no
   prompt, use ``meep-surround-region-activate-at-point``.  Never modifies the buffer.

``(meep-surround-region-activate-at-point ARG)``
   Activate the region inside the delimiters surrounding the region or point, ARG times.
   No delimiter is read - the surrounding pair is found contextually (innermost when
   ARG is 1, the ARG-th enclosing pair otherwise).  To choose which delimiter type to
   select, use ``meep-surround-region-activate``.

``(meep-surround-replace-by-type-picked-event ARG)``
   Replace the picked by-type source pair with the destination named by the invoking key.
   The destination step of ``meep-surround-replace-by-type-pick``, replacing ARG times.
   Bound only in that command's transient map; reads the stashed source from
   ``meep--surround-replace-by-type-picked-pair``.

``(meep-surround-replace-by-type-picked-lines-event ARG)``
   Replace each line's picked by-type source pair with the destination key.
   The line-wise variant of ``meep-surround-replace-by-type-picked-event``.

``(meep-surround-replace-by-type-event ARG)``
   Replace a surrounding pair of one type with another, both named by the key sequence.
   The source - the pair type to act on - is the key pressed just before this one, the
   destination is the invoking key; replace ARG times (the nesting depth).  Routed to
   by the by-type replace source map (``meep--surround-replace-by-type-map``); the
   keymap-traversed gesture stays a single named command.

``(meep-surround-replace-by-type-lines-event ARG)``
   Replace each line's surrounding pair of one type with another, named by the key sequence.
   The line-wise variant of ``meep-surround-replace-by-type-event``.

``(meep-surround-replace-by-type-pick)``
   Pick a typed source delimiter, then read a destination and replace that pair.
   The region / point variant; see ``meep--surround-replace-by-type-pick-impl``.

``(meep-surround-replace-by-type-lines-pick)``
   Pick a typed source delimiter, then replace each line's surrounding pair of that type.
   The line-wise variant of ``meep-surround-replace-by-type-pick``.

``(meep-surround-replace-by-type)``
   Read a source delimiter, then a destination, and replace that surrounding pair.
   The source names which pair type to replace - the nearest enclosing pair of that
   delimiter, skipping closer pairs of other types - and the destination is the new
   pair.  A numeric prefix is the nesting depth.  Installs the source delimiter map
   transiently, whether called by key (the default binds ``s t g`` / ``s T g``) or ``M-x``.

``(meep-surround-replace-by-type-lines)``
   Read a source delimiter and destination, replacing each line's surrounding pair of that type.
   The line-wise variant of ``meep-surround-replace-by-type``.

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

   You may wish to bind this TAB, so pressing TAB twice re-indents.

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

Rectangle Edit with Repeat-FU Replay
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

.. END VARIABLES
