# SPDX-License-Identifier: GPL-3.0-or-later

import subprocess
import os
import re
import sys


THIS_DIR = os.path.normpath(os.path.abspath(os.path.join(os.path.dirname(__file__))))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, ".."))

EMACS_BIN = os.environ.get("EMACS_BIN") or "emacs"

# Interactive prompts that appear in batch mode.
# These are printed by Emacs C code and cannot be suppressed via Lisp.
SUPPRESSED_PROMPTS = [
    "Find Next:",
    "Find Prev:",
    "Till Next:",
    "Till Prev:",
    "Mark inner char:",
    "Mark outer char:",
    "Insert Char:",
    "Replace Char:",
    "Surround Char:",
    "Surround Lines by Char:",
]

SUPPRESSED_PROMPT_PATTERNS = re.compile(
    "^(" + "|".join(re.escape(p) for p in SUPPRESSED_PROMPTS) + ")$"
)


def subprocess_call_filtered(cmd: list, env: dict | None = None) -> int:
    """Run command and filter out known interactive prompt noise from output."""
    process = subprocess.Popen(
        cmd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    was_suppressed = False
    for line in process.stdout:
        line_rstrip = line.rstrip()
        if SUPPRESSED_PROMPT_PATTERNS.match(line_rstrip):
            was_suppressed = True
        else:
            # Suppress empty lines after suppressed lines,
            # since it's common the messages have a trailing blank line.
            if not (was_suppressed and not line_rstrip):
                sys.stdout.write(line)
                sys.stdout.flush()
            was_suppressed = False

    return process.wait()


def run_meep_tests() -> int:
    env = os.environ.copy()
    env["MEEP_TEST_ENV"] = "1"
    cmd = [
        EMACS_BIN,
        "-batch",
        "--debug-init",
        "--init-directory", os.path.join(BASE_DIR, "init", "default"),
        "-l", os.path.join(BASE_DIR, "init", "default", "init.el"),
        "-l", os.path.join(THIS_DIR, "meep_tests.el"),
        "-f", "meep_tests-run-all"
    ]

    return subprocess_call_filtered(cmd, env=env)


def run_meep_tests_internal() -> int:
    cmd = [
        EMACS_BIN,
        "-batch",
        "-l", os.path.join("tests", "meep_tests_internal.el"),
        "-f", "ert-run-tests-batch-and-exit"
    ]
    return subprocess_call_filtered(cmd)


def main() -> int:
    exit_code = 0
    exit_code |= run_meep_tests()
    exit_code |= run_meep_tests_internal()
    return exit_code


if __name__ == "__main__":
    sys.exit(main())
