#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

import os
import re
import subprocess
import sys

BASE_DIR = os.path.join(os.path.dirname(__file__), "..")

ELISP_NAME = "meep.el"
EMACS_NAME = "emacs"


def patch_help_test(emacs_output: str) -> str:

    # Replace unicode quotes to double back-ticks.
    def key_replace_quote_to_rst(m: re.Match[str]) -> str:
        return "``{:s}``".format(m.group(2))

    # Allow for quoted properties with optional leading `:`.
    emacs_output = re.sub(
        "(\u2018)(:?[\\w\\-*]+)(\u2019)",
        key_replace_quote_to_rst, emacs_output,
    )

    # Allow for quoted properties `symbol'.
    # For some reason some of these are converted to unicode quotes but others not.
    emacs_output = re.sub(
        "(`)([\\w\\-*]+)(')",
        key_replace_quote_to_rst, emacs_output,
    )

    # Add double back-ticks to lisp keywords,
    # because they are used to represent "code".
    def key_replace_property(m: re.Match[str]) -> str:
        return "{:s}``{:s}``".format(m.group(1), m.group(2))

    emacs_output = re.sub(
        "([\\s\\(\\)])(:[\\w\\-*]+)\\b",
        key_replace_property, emacs_output,
    )

    # Replace lisp convention for single quote escaping
    # with a single quote.
    emacs_output = emacs_output.replace("\\\\='", "'")

    return emacs_output


def text_insert_into_bounds(
        data: str,
        data_insert: str,
        beg_comment: str,
        end_comment: str,
        error_when_missing: bool = True,
) -> str | None:
    beg_index = data.find(beg_comment)
    end_index = data.find(end_comment, beg_index)

    if beg_index == -1:
        if error_when_missing:
            print("Error: {!r} not found".format(beg_comment))
        return None
    if end_index == -1:
        if error_when_missing:
            print('Error: {!r} not found'.format(end_comment))
        return None

    beg_index += len(beg_comment) + 1

    return data[:beg_index] + data_insert + data[end_index:]


def readme_patch_docstrings_sections(data: str) -> str:
    with open(os.path.join(BASE_DIR, ELISP_NAME)) as fh:
        file_orig = fh.read()

    header_overline = ";; " + ("-" * 75) + "\n"

    lines = file_orig.splitlines(keepends=True)

    def first_symbol_after_comment(line_index: int) -> str:
        for i in range(line_index, len(lines)):
            if lines[i] == header_overline:
                return ""

            if lines[i].startswith("(defun "):
                sym = lines[i].split()[1]
            # elif lines[i].startswith("(defcustom "):
            #     sym = lines[i].split()[1]
            # Causes problems as these are documented elsewhere.
            # elif lines[i].startswith("(defvar "):
            #     sym = lines[i].split()[1]
            # elif lines[i].startswith("(defvar-local "):
            #     sym = lines[i].split()[1]
            elif lines[i].startswith("(defconst "):
                sym = lines[i].split()[1]
            else:
                continue

            if "--" not in sym:
                return sym
        return ""

    # while True:
    # header_index = file_orig.find(header, header_index)
    sections: list[tuple[str, str]] = []
    section_current: list[str] = []
    state = -1
    for line_index, l in enumerate(lines):
        if l == header_overline:
            # Some sections only document internal functionality.
            section_current.clear()
            state = 1
            continue
        if state == 1:
            if not l or not l.startswith(";;"):
                state = -1
                text_lines: list[str] = [
                    "\n" if x == ";;\n" else x[3:]
                    for x in section_current
                ]
                # Add title text.
                text_lines = [
                    text_lines[0],
                    ("^" * (len(text_lines[0]) - 1)) + "\n",
                    *text_lines[1:],
                ]

                text = "".join(text_lines)
                sections.append((first_symbol_after_comment(line_index), text))
                section_current = []
                continue
            section_current.append(l)

    # print(sections)

    # Now add the inline groupings.
    data_lines_old = data.splitlines(keepends=True)

    data_lines_new = []

    for l in data_lines_old:
        if l.startswith("``("):
            sym = l[3:].split()[0].rstrip(")`")
        elif l.startswith("``"):
            sym = l[3:].split()[0].rstrip("`")
        else:
            sym = ""

        if sym:
            for i, (sym_test, header) in enumerate(sections):
                if sym_test == sym:
                    data_lines_new.append(header)
                    data_lines_new.append("\n")
                    del sections[i]
                    break

        data_lines_new.append(l)

    return "".join(data_lines_new)


def readme_patch_docstrings(data: str) -> str | int:

    cmd = [
        EMACS_NAME,
        "--batch",
        "--load",
        os.path.join(BASE_DIR, ELISP_NAME),
        "--load",
        os.path.join(BASE_DIR, "_misc", "readme_update.el"),

        "--eval", (
            """(readme_update-printf \""""
            """\nCustom Variables"""
            """\n----------------\n\n")"""
        ),
        "--eval", (
            """(readme_update """
            """"^meep-[a-z]" 'var-custom """
            """(list 'meep-mode-hook))"""
        ),

        # Valid but these are for developers making their own back-ends.
        # Better not make the README too cryptic.

        "--eval", (
            """(readme_update-printf \""""
            """\nOther Variables"""
            """\n---------------\n\n")"""
        ),
        "--eval", (
            """(readme_update """
            """"^meep-[a-z]" 'var """
            """(list 'meep-mode-off-hook """
            """      'meep-mode-on-hook """
            """      'meep-mode-hook """
            """      'meep-mode-map))"""
        ),

        "--eval", (
            """(readme_update-printf \""""
            """\nCommands"""
            """\n--------\n\n")"""
        ),
        "--eval", (
            """(readme_update """
            """"^meep-[a-z]" 'fun-interactive """
            """(list 'meep-mode """
            """))"""
        ),
        # "--eval", (
        #         """(readme_update-printf \""""
        #         """\nFunctions"""
        #         """\n---------\n\n")"""
        # ),
        # "--eval", (
        #     """(readme_update """
        #     """"^meep-[a-z]" 'fun """
        #     """(list 'meep-command-test-skip"""
        #     """      'meep-command-test-skip-active"""
        #     """      'meep-command-test-skip-change"""
        #     """      'meep-command-test-skip-change"""
        #     """))"""
        # ),

    ]

    p = subprocess.run(
        cmd,
        stdout=subprocess.PIPE,
    )

    emacs_output = (
        p.stdout.decode('utf-8').rstrip() +
        '\n\n'
    )
    del p

    # strip trailing space
    emacs_output = re.sub(r'[ \t]+(\n|\Z)', r'\1', emacs_output)
    emacs_output = patch_help_test(emacs_output)

    # Insert "sections" into doc-strings.
    emacs_output = readme_patch_docstrings_sections(emacs_output)

    data_result = text_insert_into_bounds(data, emacs_output, '.. BEGIN VARIABLES', '.. END VARIABLES')
    if data_result is None:
        return 1

    return data_result


def readme_patch_commands(data: str) -> str | int:

    preset_prefix = "meep-preset-"

    presets = [
        f for f in os.listdir(BASE_DIR)
        if f.endswith(".el")
        if f.startswith(preset_prefix)
    ]
    presets.sort()

    emacs_output = ["\n\n"]

    for preset in presets:
        with open(os.path.join(BASE_DIR, preset), encoding="utf-8") as fh:
            preset_name = preset[len(preset_prefix):].removesuffix(".el")
            preset_data = fh.read()
            find_comment = ";;; Commentary:"
            beg = preset_data.find(find_comment)
            assert beg != -1
            beg = preset_data.find(";; ", beg + len(find_comment))
            assert beg != -1
            # Search for a blank line.
            end = preset_data.find("\n\n", beg)

            text = "\n".join([
                "" if (l == ";;") else ("  " + l[2:])
                for l in preset_data[beg:end].split("\n")
            ])

            emacs_output.append("``'" + preset_name + "``\n")

            emacs_output.append(text)
            emacs_output.append("\n\n")

    data_result = text_insert_into_bounds(data, "".join(emacs_output), '.. BEGIN COMMANDS', '.. END COMMANDS')
    if data_result is None:
        return 1

    return data_result


def main() -> int:
    # Try write reStructuredText directly!
    filepath = "docs/reference.rst"
    data: str | int = ""
    with open(filepath, "r", encoding="utf-8") as f:
        data = f.read()

    data = readme_patch_docstrings(data)
    if isinstance(data, int):
        return data

    data = readme_patch_commands(data)
    if isinstance(data, int):
        return data

    with open(filepath, "w", encoding="utf-8") as f:
        f.write(data)

    return 0


if __name__ == "__main__":
    sys.exit(main())
