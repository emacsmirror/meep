# SPDX-License-Identifier: GPL-3.0-or-later

import subprocess
import os


THIS_DIR = os.path.normpath(os.path.abspath(os.path.join(os.path.dirname(__file__))))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, ".."))

EMACS_BIN = os.environ.get("EMACS_BIN") or "emacs"

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

    return subprocess.call(
        cmd,
        env=env,
    )


def run_meep_tests_internal() -> int:
    cmd = [
        EMACS_BIN,
        "-batch",
        "-l", os.path.join("tests", "meep_tests_internal.el"),
        "-f", "ert-run-tests-batch-and-exit"
    ]
    return subprocess.call(
        cmd,
    )

def main() -> int:
    exit_code = 0
    exit_code |= run_meep_tests()
    exit_code |= run_meep_tests_internal()
    return exit_code


if __name__ == "__main__":
    import sys
    sys.exit(main())
