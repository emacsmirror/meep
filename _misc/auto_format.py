#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

"""Format every tracked Emacs Lisp file in the repository with elisp-autofmt."""

import argparse
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

_MISC_DIR = Path(__file__).resolve().parent
_REPO_ROOT = _MISC_DIR.parent

# elisp-autofmt is checked out alongside the repository (matching the sibling
# load-paths `make check` uses); `ELISP_AUTOFMT_EL` overrides the location.
_ELISP_AUTOFMT_EL = Path(os.environ.get(
    "ELISP_AUTOFMT_EL",
    _REPO_ROOT.parent / "elisp-autofmt" / "elisp-autofmt.el",
))

# Format the visited buffer in-place, then save only when formatting actually
# changed the content.  `elisp-autofmt-buffer` marks the buffer modified even
# for a no-op, so compare the text rather than trusting `buffer-modified-p`.
# The saved path is printed so the caller can report what was reformatted.
_FORMAT_ELISP = """\
(let ((orig (buffer-string)))
  (setq elisp-autofmt-python-bin (getenv "PYTHON_BIN"))
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (setq buffer-undo-list t)
  (elisp-autofmt-buffer)
  (unless (string-equal orig (buffer-string))
    (save-buffer)
    (princ (buffer-file-name))
    (princ "\\n")))"""


def _elisp_files() -> list[Path]:
    """Return the Emacs Lisp files tracked in the repository."""
    result = subprocess.run(
        ["git", "ls-files", "-z", "*.el"],
        cwd=_REPO_ROOT,
        capture_output=True,
        check=True,
    )
    return [_REPO_ROOT / name for name in result.stdout.decode("utf-8").split("\0") if name]


def _format_file(emacs_bin: str, path: Path) -> tuple[Path, bool, str | None]:
    """Format *path* in Emacs; return ``(path, changed, error)``."""
    env = os.environ.copy()
    env["PYTHON_BIN"] = sys.executable
    result = subprocess.run(
        [
            emacs_bin,
            "--batch",
            # elisp-autofmt wraps at the buffer's `fill-column`, set per-file via
            # a local variable. Apply safe locals without prompting (batch cannot
            # query), so a sibling unsafe var does not sink the whole block.
            "--eval", "(setq enable-local-variables :safe)",
            "--load", str(_ELISP_AUTOFMT_EL),
            str(path),
            "--eval", _FORMAT_ELISP,
        ],
        capture_output=True,
        env=env,
    )
    if result.returncode != 0:
        return (path, False, result.stderr.decode("utf-8", "replace").strip())
    # A printed path signals the file was reformatted and saved.
    return (path, bool(result.stdout.strip()), None)


def main(argv: list[str] | None = None) -> int:
    """Format every tracked Emacs Lisp file, in parallel."""
    parser = argparse.ArgumentParser(
        description="Format every tracked Emacs Lisp file with elisp-autofmt.",
    )
    parser.add_argument(
        "-j", "--jobs",
        type=int,
        default=os.cpu_count() or 1,
        help="Number of files to format in parallel (default: %(default)s).",
    )
    parser.add_argument(
        "--emacs-bin",
        default=os.environ.get("EMACS_BIN", "emacs"),
        help="The command used to run Emacs (default: %(default)s).",
    )
    args = parser.parse_args(argv)

    if not _ELISP_AUTOFMT_EL.is_file():
        sys.stderr.write(
            "error: elisp-autofmt.el not found at {:s}\n"
            "Set ELISP_AUTOFMT_EL to its location.\n".format(str(_ELISP_AUTOFMT_EL)),
        )
        return 1

    files = _elisp_files()
    if not files:
        print("No Emacs Lisp files found.")
        return 0

    changed = 0
    errors = 0
    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        results = executor.map(lambda path: _format_file(args.emacs_bin, path), files)
        for path, was_changed, error in results:
            rel = path.relative_to(_REPO_ROOT)
            if error is not None:
                sys.stderr.write("error: {:s}\n{:s}\n".format(str(rel), error))
                errors += 1
            elif was_changed:
                print("formatted: {:s}".format(str(rel)))
                changed += 1

    print("Formatted {:d} file(s), {:d} unchanged, {:d} error(s).".format(
        changed, len(files) - changed - errors, errors,
    ))
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
