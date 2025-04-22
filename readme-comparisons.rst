
################################################
Meep Comparison with Other Model Editing Systems
################################################

To avoid the README becoming littered with opinions and justifications,
this has been split into it's own section.


Meep's Defining Characteristics
===============================

Lightweight & Minimalist
   Meep takes minimalism seriously.

   TODO: auto-load most/all functions.

Emacs Compatible
   Meep defines a modal editing system that uses emacs with minimal additional "state".

   That is to say, meep works withing the constraints of what emacs supports
   without defining concepts the user/developer needs to take into account.

   This has the advantage that most emacs packages work without the need for additional
   compatibility logic.

   The disadvantage is that more sophisticated functionality is not possible.
   This is not an absolute rule, meep just errs on the side of minimalism where possible.

Selection
   There are two key characteristics with how meep handles selection.

   - The active-region (visible selection) is manually toggled.
   - Commands may use the region even when it's not active.

   These these aspects are key to how commands are composed to perform edits.

Composing Commands
   Meep uses ``[noun, optional-adjective, verb]``.
   Unlike VIM style ``[verb, optional-adjective, noun]``.

   This can be thought of as selecting the thing to operate on,
   then calling the command to act on the selection.
   Which is common for non-modal editors, the difference with meep is the details for how this is achieved.

   In VIM ``cw`` will enter insert mode to change the word.

   Performing this with a selection can be done but always requires an extra step. e.g.

   ``[select, forward-word, change]``.

   Since these kinds of operations are very common, requiring the extra selection step is inefficient.

   Instead, operations such as stepping to the next word, moving to the end of the line, etc.
   set the mark without activating the region.

   This makes ``[forward-word, change]`` work without additional steps.

   There are times you may wish to change many words, where VIM would use:
   ``c3w`` to change 3 words.

   For meep this, would typically be accomplished using: ``[forward-word, 2, change]``.
   This has the advantage that you may adjust the "region" before executing the command:
   ``[forward-word, 3, 3, change]`` ... which can be useful to refine the destination.

   In some cases you may prefer to enter the number first, especially for text editing commands
   in this case the "extended" key (a kind of leader-key) can be used.
   ``[extended, 6, command]`` will run the command with it's digit argument set to 6.

   If you wish to mix different motions - say 3 lines then 3 words,
   then the implicit region cannot be used - since each new motion sets the mark.
   In this case it's necessary to enable the active region first.

   Note that left/right motion does **not** set the mark and therefor may be used for minor adjustments
   before applying the edit. e.g. ``[forward-word, left, change]`` to change all but the last letter of a word.


Key-map
-------

While you can always ignore the default key-map and use your own
the defaults have some impact on the overall design and intended use of meep.
If nothing else they are a jumping off point.

So notes on the key-map are worth a mention.

Layout Independent
   This means the keys are used based on their physical location
   rather than mnemonics as many other systems use (U for undo, W for word ... etc).

Use of Leader Keys
   From observing other systems,
   often times many keys are used for common operations such as entering insert-mode at different places
   or different kinds of search.

   For the default meep layout there is a "find" leader and the "extended" map is used for an "insert" leader,
   freeing up keys for other operations.

Finger Roll
   Operations take advantage of finger-roll where possible for keys which are frequently accessed in sequence.

   Where the little finger is used before the ring finger, before the middle finger, before the index finger -
   allowing a more natural/fluid motion.


Comparison with Evil Mode
=========================

WIP.


Key-map
-------

- Many keys used for entering insert mode, sometimes replacing text.

  - ``i`` Insert.
  - ``s`` Substitute char & insert.
  - ``o`` Open below & insert.
  - ``O`` Open above & insert.
  - ``S-i`` Insert BOL.
  - ``S-s`` Substitute line & insert.

- Many keys used for positioning the view.

  - ``S-m`` Middle of screen.
  - ``S-l`` Bottom of screen.
  - ``S-h`` Top of screen.
