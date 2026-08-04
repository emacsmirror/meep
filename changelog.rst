
##########
Change Log
##########

- In development

  - A zero repeat of ``meep-clipboard-killring-yank-pop-stack`` drops the
    shown item from the kill-ring (a zero prefix argument drops without
    pasting).
  - Rectangle edit: support real insert mode with replay.
    Keystrokes entered on one line are recorded and replayed on remaining lines,
    fused into a single undo step. Falls back to ``string-rectangle`` when ``repeat-fu-mode`` is not active.
  - Add ``sexp-elem`` movement commands for navigating between S-expression elements
    (e.g. function arguments) within bracket pairs.
  - Implement character bounds region-marking (``meep-region-mark-bounds-of-char-*``),
    with support for nested brackets.
  - Add contextual blank space removal (``meep-space-shrink-contextual``).
  - Add ``meep-region-activate-or-reverse``.
  - Add ``meep-region-exchange-secondary-selection``, exchanging the region and the
    secondary selection (including the rectangle or line-wise region type).
  - Add ``meep-secondary-selection-jump-to``, jumping to the end of the
    secondary selection which held the mark when it was made,
    repeating the command or an even numeric argument jumps to the opposite end.
  - Support ``which-key`` for the keypad.
  - Integrate rectangle cut/copy/paste with the kill-ring.
  - Line-wise kill-ring regions now use line-wise yank.
  - Region expand/contract supports a numeric argument.
  - Expanding the region without an active region now expands symmetrically.

- Version 0.0.1 (2025-09-07)

  Initial release.
