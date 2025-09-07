
################################################
MEEP Comparison with Other Model Editing Systems
################################################

Since adopting a modal-editing system can be a significant undertaking:
this page aims to differentiate MEEP from other modal editing systems available for Emacs.

*While this document compares MEEP with other systems,
I would like to acknowledge that those systems are much more mature.

Furthermore, they have their strengths.
The intent of this page is to help users know if MEEP is something they might wish use,
not to diminish the work of others.*


Comparison with Evil Mode
=========================

**Similarities:**

- Most actions take a similar number of key-strokes.
  it's possible to change a word, change a *whole* word, change 3 whole words... etc.

- Persistent Selection.

  In both cases selection is explicitly enabled and disabled,
  the selection (visual) state can have it's own key-map.

- "repeat" functionality.

  MEEP's repeat is similar to evil-modes where it's possible to repeat an insertion or a change
  which may be made up of multiple key-strokes.

**Differences:**

- Order of Motion / Action

  With VIM the motion is performed after a key to perform the action.
  With MEEP the motion is performed first, see the overview for details.

- Key-map Layout

  VIM uses mnemonics for key-binding locations (I for Insert, C for change .. etc),
  where as MEEP uses the physical location on the keyboard.

  This allows key binding locations to be optimized for:

  - Distribute actions between the left and right hand.
  - Move frequently used actions to the home row.
  - Order sequential actions to take advantage of "finger roll".

  - Bindings for insert mode.

    VIM uses many keys to enter insert mode
    (``i``, ``a``, ``s``, ``o``) all enter insert mode in slightly different ways.
    By comparison, MEEP uses:

    - A key to enter insert mode.
    - A "leader" key with insertions placed based on ``HJKL`` directions.


Comparison with Meow
====================

In many ways, MEEP is quite similar to Meow.

**Similarities:**

- Similar goal & approach:
  To provide a modal editing system that fits in with Emacs built-in behavior
  instead of attempting to apply more rigid control over behavior.

- The user is expected to set their own key-map,
  with suggested default provided.

- Support for a Key-Pad to support key binding sequences more easily.

- The default key-map mainly uses alphanumeric keys with Shift,
  none of the bindings use Control or Alt, allowing Emacs default bindings to co-exist.

**Differences:**

- Persistent Selection

  In Meow holding shift with a motion selects text and moving the cursor can drop the selection.
  With MEEP selection is explicitly enabled & disabled.

  This has the implications that Shift can be used to map additional actions,
  and motions without Shift held don't "drop" the selection.

- Macros/Repeating Actions

  In Meow beacon-mode to apply an action in multiple places
  (similar to multiple cursors).

  By comparison, MEEP relies on repeating the last edit,
  where more involved multi-step edits require regular macro record & replay.

- Visual Feedback

  MEEP shows overlays after cursor motion,
  to indicate the number that can be used to extend the motion.

  MEEP by comparison doesn't use any visual feedback
  *although it could be optionally supported*.

- Search

  Meow implements it's own "visit" functionality,
  MEEP uses ISearch.

- Key-bindings

  Meow makes use of re-mapping key-strokes to Emacs default bindings.
  This can cause problems if users have disabled or remapped those bindings.

  MEEP by comparison uses a key-map without depending on other bindings.

- Key-map Layout

  - Since Shift is not used for selection,
    holding shift can be used for other bindings.

  - MEEP doesn't use the SpaceBar in the default key-map.

    The SpaceBar can be used by the user as a leader key,
    as is common practice for VIM/EvilMode.
