
####################
MEEP Design Overview
####################


MEEP's Defining Characteristics
===============================

Simple Implementation
   Avoid layering additional behavior on top of Emacs.

Emacs Compatible
   MEEP defines a modal editing system that uses emacs with minimal additional "state".

   That is to say, MEEP works withing the constraints of what emacs supports
   without defining concepts the user/developer needs to take into account.

   This has the advantage that most emacs packages work without the need for additional
   compatibility logic.

   The disadvantage is that more sophisticated functionality is not possible.
   This is not an absolute rule, MEEP just errs on the side of minimalism where possible.

Selection
   There are two key characteristics with how MEEP handles selection.

   - The active-region (visible selection) is manually toggled.
   - Commands may use the region even when it's not active.

   *These aspects are key to how commands are composed to perform edits.*

Composing Commands
   MEEP uses ``[motion, optional-argument, action]``.
   Unlike VIM style ``[action, optional-argument, motion]``.

   This can be thought of as selecting the thing to operate on,
   then calling the command to act on the selection.
   Which is common for non-modal editors, the difference with MEEP is the details for how this is achieved.

   In VIM ``cw`` will enter insert mode to change the word.

   Performing this with a selection can be done but always requires an extra step. e.g.

   ``[select, forward-word, change]``.

   Since these kinds of operations are very common, requiring the extra selection step is inefficient.

   Instead, operations such as stepping to the next word, moving to the end of the line, etc.
   set the mark without activating the region.

   This makes ``[forward-word, change]`` work without additional steps.

   There are times you may wish to change many words, where VIM would use:
   ``c3w`` to change 3 words.

   For MEEP this, would typically be accomplished using: ``[forward-word, 2, change]``.
   This has the advantage that you may adjust the "region" before executing the command:
   ``[forward-word, 3, 3, change]`` ... which can be useful to refine the destination.

   In some cases you may prefer to enter the number first, especially for text editing commands
   in this case the "extended" key (a kind of leader-key) can be used.
   ``[extended, 12, command]`` will run the command with it's digit argument set to 12.

   If you wish to mix different motions - say 3 lines then 3 words,
   then the implicit region cannot be used - since each new motion sets the mark.
   In this case it's necessary to activate the region first.

   Note that left/right motion does **not** set the mark and therefor may be used for minor adjustments
   before applying the edit. e.g. ``[forward-word, left, change]`` to change all but the last letter of a word.

Extensible States
   States & state switching (normal/visual/insert/... etc) are part of the user configuration,
   so you can define new states as you wish.


Default Key-map
---------------

While you can always ignore the default key-map and use your own
the defaults have some impact on the overall design and intended use of MEEP.

If nothing else they may provide a starting point from which to design your own key-map.

So notes on the default key-map are worth mentioning.

Layout Independent
   This means the keys are used based on their physical location
   rather than mnemonics as many other systems use (``U`` for undo, ``W`` for word ... etc).

Use of Leader Keys
   From observing other systems,
   often times many keys are used for common operations such as entering insert-mode at different places
   or different kinds of search.

   For the default MEEP layout there is a "find" leader and the "extended" map is used for an "insert" leader,
   freeing up keys for other operations.

Finger Roll
   Operations take advantage of finger-roll where possible for keys which are frequently accessed in sequence.

   Where the little finger is used before the ring finger, before the middle finger, before the index finger -
   allowing a more natural/fluid motion.


Limitations
===========

This section acknowledges some of the limitations with the choices used in MEEP.

Simplicity
----------

Avoiding complicated solutions is somewhat limiting,
for example it largely rules out defining entirely new behaviors
that require changing more fundamental aspects of emacs.

To pick some concrete examples:

- A line based selection that more rigidly ensures the selection remains at line bounds.

  While this has the advantage that compatibility logic is much less likely to be needed in other packages,
  it does limit the possibilities somewhat.

- MEEP uses regular Emacs key-maps.

  This means it's not possible to conveniently define a key that only applies to a major mode
  that only applies when inserting, selecting etc (as ``evil-mode`` can do).

  This is an area that can be investigated, it may be possible to accomplish this using opt-in utilities
  that don't require additional complication for the common case.


Motion First
------------

While I've found "motion first" works well in practice,
it must be acknowledged that ordering ``[action, motion]`` (like VIM) *does* provide greater scope
for key-bindings since the second key can potentially be bound to keys would otherwise be an ``action`` itself.

By comparison, "motion first" must allow for either another motion **or** an action,
reducing the possibilities for binding additional keys.

In defiance of this behavior:

- Making some keys act as "leader" keys avoids similar actions using too many "top level"
  bindings and gives enough space for this not to be such a problem.

- Using motion first allows any motion to be used as a kind of "text-object",
  without the need for a formal text-object system.

- Performing the action after has the advantage that accidentally operating the wrong number of text objects
  doesn't need to be undone and repeated - which can occur when operating on a larger number of words/lines.

- Using motion first has the advantage that a motion can be adjusted
  (with single character motions for example) before the action runs.

  So it's possible for example to:

  - Use a motion to the line end.
  - Move left one characters (as an adjustment).
  - Change the region.

  Further, this action can be repeated multiple timed with a single key-stroke,
  since adjustments to a motion don't begin a new chain of commands.

These arguments aren't strong claims that justify "motion first" as being better though.
Your experience is likely to vary based on your own preferences and use cases.
