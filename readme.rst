
##############
MEEP for Emacs
##############

MEEP is an experimental modal editing system for emacs.

Available via `melpa <https://melpa.org/#/meep>`__.


Motivation
==========

The main motivation for MEEP was to provide a modal editing environment
that can co-exist with Emacs default key bindings.

Support VIM-like Workflow
   Common operations which are possible in VIM should be possible in MEEP
   with a similar number of key-strokes.
Low Friction Integration
   Use of a modal editing shouldn't impose additional constraints (where possible)
   or complicate use of 3rd party packages.
Low Complexity
   Avoid "clever" solutions that often end up causing unforeseen complications.

For the most part MEEP is just a collection of commands that could be called independently of each other.


Other Pages
===========

- `Overview <https://codeberg.org/ideasman42/emacs-meep/src/branch/main/docs/overview.rst>`__:
  for extended information on the design & implementation.
- `Comparisons <https://codeberg.org/ideasman42/emacs-meep/src/branch/main/docs/comparisons.rst>`__:
  for the rationale and comparisons with other modal-editing systems.
- `Command Reference <https://codeberg.org/ideasman42/emacs-meep/src/branch/main/docs/reference.rst>`__:
  for a list of commands and options.
- `Default Keymap <https://codeberg.org/ideasman42/emacs-meep/raw/branch/main/init/default/init.el>`__:
  for reference and an example key-map.
- `Hacking <https://codeberg.org/ideasman42/emacs-meep/src/branch/main/docs/hacking.rst>`__:
  notes for working on the code.
- `Further Work <https://codeberg.org/ideasman42/emacs-meep/issues/1>`__:
  potential changes and improvements.


Features
========

- Persistent selection (as its own state with its own key-map).
- Repeat multiple commands (edits, insertions, changes, etc).
- Rectangle selection support (cut/paste/swap/replace/change/surround).
- Find character on line (VIM style).
- Record and replay macros (VIM style).
- Key-pad mode
  (matching Meow's `keypad mode <https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#keypad>`__).


How it Works
============

Instead of defining text-objects, most "motions" commands set the mark.
This means commands afterwards can operate on the *region*, without activating it.

At first this may seem counter intuitive (since the region isn't visible)
but it doesn't take long to get used to this.

Allowing for common actions such as change-word or cut-paragraph to be performed efficiently.


Trying it Out
=============

To try out MEEP without installing it, you may use the bundled example ``init.el`` file:

.. code-block:: shell

   ./emacs-run-local.sh


Default Key-map
---------------

A diagram for the default layout can be found in the example configuration, see:
`init.el <https://codeberg.org/ideasman42/emacs-meep/raw/branch/main/init/default/init.el>`__.


Installing
==========

While it is possible to install this package and access commands
a typical installation is expected to be integrated into your ``init.el`` file.

For reference see the minimal example:
`init.el <https://codeberg.org/ideasman42/emacs-meep/raw/branch/main/init/minimal/init.el>`__.

Which shows how MEEP commands can be used with Bray for a modal editing configuration.


Integration
===========

If you wish to use "motion" commands from other packages with MEEP.
A utility functions is ``meep-command-mark-on-motion-advice-add`` (as well as an associated remove function).

If you wish to repeat these motions by entering a number afterwards or selecting the whole item with
``meep-exchange-point-and-mark-motion``, then these commands must accept a numeric argument
(already a common convention).

Note that without any integration, motion command from other packages can still be used,
they just wont mark text to operate on.


Customization
=============

It's recommended to start out with one of the provided key-maps.

See the
`command reference <https://codeberg.org/ideasman42/emacs-meep/src/branch/main/docs/reference.rst>`__
for a list of custom options.


Dependencies
============

- `Bray <https://codeberg.org/ideasman42/emacs-bray>`__:
  a generic modal editing system.
- `Repeat-Fu <https://codeberg.org/ideasman42/emacs-repeat-fu>`__:
  while not a hard dependency, it's recommended.


Other Packages
==============

These packages work well with MEEP.

- `Avy <https://github.com/abo-abo/avy>`__:
  For jumping to text by characters.
- `Shift numbers <https://codeberg.org/ideasman42/emacs-shift-number>`__:
  for incrementing/decrementing numbers.
