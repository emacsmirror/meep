#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

"""
Generate the Sphinx RST sources for the Meep manual.

Meep is an Emacs Lisp package, so there is no Python module to introspect;
this script lays out the build directory Sphinx consumes: a landing page, the
hand-written static assets, and a templated copy of ``conf.py``. The Makefile's
``doc`` target then runs ``sphinx-build`` over the result.

Based on patterns from IonWL's ``sphinx_doc_gen.py``.
"""

__all__ = (
    "main",
)

import shutil
import sys
from pathlib import Path

_SCRIPT_DIR = Path(__file__).parent

# `_SCRIPT_DIR` is `doc/manual`, so `parents[1]` is the repository root, where
# `meep.el` carries the version header used to template `conf.py`.
_REPO_ROOT = _SCRIPT_DIR.parents[1]
_MEEP_EL = _REPO_ROOT / "meep.el"

# Documentation pages, in the order they appear in the index toctree.
_DOC_PAGES = (
    "overview",
    "comparisons",
    "reference",
    "hacking",
)

# Hand-written pages, copied verbatim from `rst/<name>.rst` into the build
# directory so Sphinx treats each as a top-level document. `reference` is absent
# because it is generated (see `_generate_reference_rst`), not tracked.
_RST_DIR = _SCRIPT_DIR / "rst"
_COPY_PAGES = (
    "overview",
    "comparisons",
    "hacking",
)

# Hand-written static assets (CSS) copied into `build/static/`, which
# `conf.py` registers as `html_static_path`.
_STATIC_DIR = _SCRIPT_DIR / "static"

# The project's source repository, linked from the index sidebar.
_PROJECT_URL = "https://codeberg.org/ideasman42/emacs-meep"

# Hand-written Sphinx config, copied into the build directory and templated
# with `${MEEP_VERSION}` substituted from `meep.el`.
_CONF_PY = _SCRIPT_DIR / "conf.py"


class _State:  # pylint: disable=too-few-public-methods
    """Mutable flags shared across the documentation pass."""

    verbose: bool = False
    # When true, suppress progress output and print only errors/warnings.
    quiet: bool = False


def _title_string(text: str, heading_char: str, double: bool = False) -> str:
    """Generate an RST title with underline (and optional overline)."""
    filler = len(text) * heading_char
    if double:
        # pylint: disable-next=duplicate-string-formatting-argument
        return "{:s}\n{:s}\n{:s}\n\n".format(filler, text, filler)
    return "{:s}\n{:s}\n\n".format(text, filler)


def _copy_doc_pages(output_dir: Path) -> None:
    """
    Copy hand-written documentation pages into the build directory.

    Pages are standalone documents referenced from the index toctree, so they
    must exist as real files in the build directory alongside the generated
    ``index.rst``.
    """
    for name in _COPY_PAGES:
        src = _RST_DIR / "{:s}.rst".format(name)
        if not src.is_file():
            raise FileNotFoundError("doc page not found: {:s}".format(str(src)))
        (output_dir / "{:s}.rst".format(name)).write_text(
            src.read_text(encoding="utf-8"), encoding="utf-8",
        )
        if _State.verbose:
            print("Generated: {:s}".format(str(output_dir / "{:s}.rst".format(name))))


def _generate_reference_rst(output_dir: Path) -> None:
    """Generate ``reference.rst`` from Meep's doc-strings into the build directory.

    The reference is derived entirely from doc-strings, so it is a build
    artifact rather than a tracked source file. `readme_update` shells out to
    Emacs to extract them.
    """
    import sys  # pylint: disable=import-outside-toplevel

    sys.path.insert(0, str(_REPO_ROOT / "_misc"))
    import readme_update  # type: ignore[import-not-found]  # pylint: disable=import-outside-toplevel

    (output_dir / "reference.rst").write_text(
        readme_update.generate_reference_rst(), encoding="utf-8",
    )
    if _State.verbose:
        print("Generated: {:s}".format(str(output_dir / "reference.rst")))


def _generate_index_rst(output_dir: Path) -> Path:
    """Generate the index.rst landing page."""
    filepath = output_dir / "index.rst"
    with open(filepath, "w", encoding="utf-8") as f:
        f.write(_title_string("Meep Manual", "=", double=True))

        f.write(".. toctree::\n")
        f.write("   :maxdepth: 2\n")
        f.write("\n")
        for name in _DOC_PAGES:
            f.write("   {:s}\n".format(name))
        f.write("\n")

        # External link to the project's source repository.
        f.write(".. toctree::\n")
        f.write("   :caption: Links\n")
        f.write("\n")
        f.write("   Project Page <{:s}>\n".format(_PROJECT_URL))
        f.write("\n")
    return filepath


def _clean_generated_sources(output_dir: Path) -> None:
    """
    Remove previously generated sources so a renamed or dropped page cannot linger.

    This pass writes each page fresh but never deletes, so a stale ``<page>.rst``
    from an earlier run survives.  `sphinx-build -W` then promotes its "document
    isn't included in any toctree" warning to a fatal error.  Clear the generated
    sources up-front, keeping Sphinx's own ``_build`` directory so the doctree
    cache (and thus incremental builds) is preserved.
    """
    if not output_dir.is_dir():
        return
    for path in output_dir.iterdir():
        if path.name == "_build":
            continue
        if path.is_dir():
            shutil.rmtree(path)
        else:
            path.unlink()


def _copy_static_files(output_dir: Path) -> None:
    """
    Copy hand-written static assets into ``build/static/``.

    Copies the raw bytes rather than round-tripping through text: ``html_static_path``
    conventionally holds images (icons, logos) alongside CSS, and a text copy would
    fail on the first non-UTF-8 asset.
    """
    if not _STATIC_DIR.is_dir():
        return
    dest = output_dir / "static"
    dest.mkdir(parents=True, exist_ok=True)
    for src in sorted(_STATIC_DIR.iterdir()):
        if src.is_file():
            shutil.copyfile(src, dest / src.name)
            if _State.verbose:
                print("Generated: {:s}".format(str(dest / src.name)))


def _meep_version() -> str:
    """Read the ``;; Version:`` header from ``meep.el``."""
    prefix = ";; Version:"
    for line in _MEEP_EL.read_text(encoding="utf-8").splitlines():
        if line.startswith(prefix):
            return line[len(prefix):].strip()
    raise ValueError("no `{:s}` header found in {:s}".format(prefix, str(_MEEP_EL)))


def _install_conf_py(output_dir: Path) -> Path:
    """
    Copy ``conf.py`` into the build directory and substitute placeholders.

    The hand-written ``conf.py`` carries ``${MEEP_VERSION}`` placeholders that
    are filled in here via :class:`string.Template`. Each value is wrapped so
    it closes the surrounding quotes in the template and re-opens as a
    ``repr()`` literal, keeping arbitrary characters safe.
    """
    from string import Template  # pylint: disable=import-outside-toplevel

    def declare_in_quotes(value: str) -> str:
        return '" {!r} "'.format(value)

    substitutions = {
        "MEEP_VERSION": declare_in_quotes(_meep_version()),
    }

    filepath = output_dir / "conf.py"
    template = _CONF_PY.read_text(encoding="utf-8")
    filepath.write_text(Template(template).substitute(substitutions), encoding="utf-8")
    return filepath


def main(argv: list[str] | None = None) -> int:
    """Main entry point."""
    import argparse  # pylint: disable=import-outside-toplevel

    parser = argparse.ArgumentParser(
        description="Generate the Sphinx RST sources for the Meep manual.",
    )
    parser.add_argument(
        "-o", "--output", dest="output_dir",
        default=str(_SCRIPT_DIR / "build"),
        help="Output directory for generated RST files (default: %(default)s).",
    )
    verbosity = parser.add_mutually_exclusive_group()
    verbosity.add_argument(
        "-v", "--verbose", action="store_true",
        help="Show per-file progress.",
    )
    verbosity.add_argument(
        "-q", "--quiet", action="store_true",
        help="Suppress progress output, printing only errors and warnings.",
    )
    args = parser.parse_args(argv)

    _State.verbose = args.verbose
    _State.quiet = args.quiet
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # Drop stale sources from a prior run before regenerating.
    _clean_generated_sources(output_dir)

    # Copy hand-written documentation pages referenced by the index toctree.
    _copy_doc_pages(output_dir)

    # Generate the doc-string reference referenced by the index toctree.
    _generate_reference_rst(output_dir)

    # Generate index.rst.
    index_path = _generate_index_rst(output_dir)
    if _State.verbose:
        print("Generated: {:s}".format(str(index_path)))

    # Copy static assets (CSS) referenced by conf.py's html_static_path.
    _copy_static_files(output_dir)

    # Copy and template conf.py.
    conf_path = _install_conf_py(output_dir)
    if _State.verbose:
        print("Generated: {:s}".format(str(conf_path)))

    if not _State.quiet:
        print("Generated RST sources in: {:s}".format(str(output_dir)))
        print("Run 'sphinx-build -b html build build/_build/html' to build HTML documentation.")

    return 0


if __name__ == "__main__":
    sys.exit(main())
