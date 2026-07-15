# SPDX-License-Identifier: GPL-3.0-or-later

"""Sphinx configuration for the Meep manual."""

# Sphinx reads its configuration from lowercase module globals and expects a
# `copyright` global, so the default constant-naming and builtin-shadowing
# lints do not apply to this file.
# pylint: disable=invalid-name,redefined-builtin

from html import escape

from docutils import nodes  # type: ignore[import-untyped]
from docutils.parsers.rst import Directive  # type: ignore[import-untyped]
from sphinx.application import Sphinx

__all__ = (
    "has_module",
    "register_inkpot_style",
    "MEEP_VERSION",
    "project",
    "copyright",
    "author",
    "version",
    "release",
    "html_title",
    "extensions",
    "exclude_patterns",
    "html_theme",
    "html_static_path",
    "html_last_updated_fmt",
    "highlight_language",
    "highlight_options",
    "register_details_directive",
    "setup",
)


def has_module(module_name: str) -> bool:
    """Return whether *module_name* can be imported."""
    found = False
    try:
        __import__(module_name)
        found = True
    except ModuleNotFoundError as ex:
        if ex.name != module_name:
            raise ex
    return found


def register_inkpot_style() -> None:
    """Register the InkPot style under the Pygments name ``inkpot``.

    Pygments resolves style names via its ``STYLE_MAP``, whose entries point
    at a class inside a ``pygments.styles`` submodule. Since the style is
    defined here rather than in an installed module, inject a synthetic module
    so :func:`pygments.styles.get_style_by_name` (used by Sphinx and furo) can
    find it.
    """
    # Imported lazily so all InkPot wiring lives here; Pygments is untyped, so
    # mypy sees these names as `Any`.
    # pylint: disable=import-outside-toplevel
    import sys
    from types import ModuleType

    import pygments.styles  # type: ignore[import-untyped]
    from pygments.style import Style  # type: ignore[import-untyped]
    from pygments.token import (  # type: ignore[import-untyped]
        Comment,
        Error,
        Generic,
        Keyword,
        Name,
        Number,
        Operator,
        Other,
        Punctuation,
        String,
        Text,
        Whitespace,
    )

    # A Pygments style is a data-only class (no public methods), and its
    # `Style` base is untyped so mypy sees it as `Any`.
    class InkPotStyle(Style):  # type: ignore[misc]  # pylint: disable=too-few-public-methods
        """The InkPot Pygments style, a dark syntax-highlighting scheme."""

        background_color = "#1e1e27"
        default_style = ""
        styles = {
            Text: "#cfbfad",
            Other: "#cfbfad",
            Whitespace: "#434357",
            Comment: "#cd8b00",
            Comment.Preproc: "#409090",
            Comment.PreprocFile: "bg:#404040 #ffcd8b",
            # TODO/FIXME markers use the vim `Todo` highlight.
            Comment.Special: "bg:#d0a060 #303030 bold",

            Keyword: "#808bed",
            Keyword.Pseudo: "nobold",
            Keyword.Constant: "#ff8bff",
            Keyword.Type: "#ff8bff",
            Keyword.Namespace: "#409090",

            Operator: "#808bed",

            Punctuation: "#cfbfad",

            Name: "#cfbfad",
            Name.Attribute: "#cfbfad",
            Name.Builtin.Pseudo: "#ff8bff",
            Name.Builtin: "#ff8bff",
            Name.Class: "#ff8bff",
            Name.Constant: "#409090",
            Name.Decorator: "#409090",
            Name.Exception: "#ff8bff",
            Name.Function: "#ff8bff",
            Name.Label: "#808bed",
            Name.Namespace: "#409090",
            Name.Variable: "#cfbfad",

            String: "bg:#404040 #ffcd8b",
            String.Escape: "bg:#404040 #c080d0",
            String.Interpol: "bg:#404040 #c080d0",
            String.Doc: "bg:#1e1e27 #808bed",

            Number: "#f0ad6d",

            # These generic styles are from the emacs InkPot theme.
            Generic.Heading: "bold #000080",
            Generic.Subheading: "bold #800080",
            Generic.Deleted: "#A00000",
            Generic.Inserted: "#00A000",
            Generic.Error: "#FF0000",
            Generic.Emph: "italic",
            Generic.Strong: "bold",
            Generic.EmphStrong: "bold italic",
            Generic.Prompt: "bold #000080",
            Generic.Output: "#888",
            Generic.Traceback: "#04D",

            Error: "bg:#6e2e2e #ffffff",
        }

    module = ModuleType("pygments.styles.inkpot")
    setattr(module, "InkPotStyle", InkPotStyle)
    sys.modules["pygments.styles.inkpot"] = module
    pygments.styles.STYLE_MAP["inkpot"] = "inkpot::InkPotStyle"


# This is substituted when the file is copied to the build directory.
MEEP_VERSION = "${MEEP_VERSION}"

project = "Meep {:s} Manual".format(MEEP_VERSION)
copyright = "Meep Contributors"
author = "Meep Contributors"
version = MEEP_VERSION
release = MEEP_VERSION

# Shorter title for the browser tab and the furo sidebar brand.
html_title = "Meep Manual"

extensions: list[str] = []

exclude_patterns: list[str] = []

# The fallback to a built-in theme when `furo` is not found.
html_theme = "classic"

if has_module("furo"):
    html_theme = "furo"
    html_theme_options = {
        "light_css_variables": {
            "color-brand-primary": "#265787",
            "color-brand-content": "#265787",
        },
    }

    # Use the InkPot style for code highlighting in both furo color modes.
    register_inkpot_style()
    pygments_style = "inkpot"
    pygments_dark_style = "inkpot"

html_static_path = ["static"]

# Furo has no option to hide the right-hand "On this page" column or widen its
# fixed-width content, so a small CSS override does both (see static/custom.css).
if html_theme == "furo":
    html_css_files = ["custom.css"]

# Disable the default `last_updated` value, since it is the date of doc
# generation, not the one of the source commit.
html_last_updated_fmt = None

# Meep is an Emacs Lisp package, so default code blocks to Emacs Lisp.
highlight_language = "emacs-lisp"
# No need to detect encoding.
highlight_options = {"default": {"encoding": "utf-8"}}


def register_details_directive(app: Sphinx) -> None:
    """Register a `.. details:: Title` directive.

    Wraps content in an HTML <details> widget so verbose-but-uninteresting
    sections are foldable. Implemented via nodes.raw around the parsed content -
    this avoids needing a custom node class (Sphinx pickles the doctree for
    parallel/incremental builds and cannot reach classes defined in conf.py,
    which is exec-loaded). Non-HTML builders ignore raw nodes and render the
    content inline.
    """

    class DetailsDirective(Directive):  # type: ignore[misc]
        """A `.. details:: Title` directive rendering an HTML <details> block."""

        required_arguments = 0
        optional_arguments = 1
        final_argument_whitespace = True
        has_content = True

        def run(self) -> list[nodes.Node]:
            summary = self.arguments[0] if self.arguments else "Details"
            container = nodes.Element()
            self.state.nested_parse(self.content, self.content_offset, container)
            children = list(container.children)
            container.children = []
            for child in children:
                child.parent = None
            open_tag = nodes.raw(
                "",
                "<details><summary>{:s}</summary>".format(escape(summary)),
                format="html",
            )
            close_tag = nodes.raw("", "</details>", format="html")
            return [open_tag, *children, close_tag]

    app.add_directive("details", DetailsDirective)


def setup(app: Sphinx) -> None:
    """Register Meep's custom Sphinx directives."""
    register_details_directive(app)
