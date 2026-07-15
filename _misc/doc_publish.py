#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

"""Publish generated HTML documentation to a Codeberg Pages ``pages`` branch.

Codeberg's git-pages server serves the tip of a branch named ``pages``.  This
builds a single orphan commit from the HTML output (minus the Sphinx build cache)
and force-pushes it to that branch, so the repository never accumulates
generated-HTML history.  A push only
goes live once the repository's Forgejo webhook notifies the git-pages server,
which is a one-time setup in the Codeberg web UI.
"""

__all__ = (
    "main",
)

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path


def _git_output(
    args: list[str],
    *,
    env: dict[str, str] | None = None,
    cwd: str | None = None,
) -> str:
    """Run a git command, returning its stripped stdout, exiting on failure."""
    result = subprocess.run(
        ["git", *args],
        capture_output=True,
        text=True,
        check=False,
        env=env,
        cwd=cwd,
    )
    if result.returncode != 0:
        sys.stderr.write(result.stderr)
        sys.exit("error: `git {:s}` failed".format(args[0]))
    return result.stdout.strip()


def main() -> None:
    """Force-push the built HTML tree to the remote ``pages`` branch."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--html-dir",
        required=True,
        help="Directory holding the built HTML (must contain index.html).",
    )
    parser.add_argument(
        "--remote",
        default="origin",
        help="Git remote to push the pages branch to (default: origin).",
    )
    parser.add_argument(
        "--branch",
        default="pages",
        help="Branch git-pages serves from (default: pages).",
    )
    parser.add_argument(
        "--message",
        default=None,
        help="Commit message for the published tree.",
    )
    args = parser.parse_args()

    html_dir = Path(args.html_dir).resolve()
    if not (html_dir / "index.html").is_file():
        sys.exit(
            "error: {:s} has no index.html - run `make doc` first".format(str(html_dir)),
        )

    # Resolve the absolute git directory so the plumbing commands below work
    # regardless of the working directory they are handed.
    git_dir = Path(_git_output(["rev-parse", "--absolute-git-dir"])).resolve()

    # Record the source revision so the published commit is traceable.
    source_rev = _git_output(["rev-parse", "--short", "HEAD"])
    message = args.message or "Publish documentation from {:s}".format(source_rev)

    with tempfile.TemporaryDirectory() as tmp:
        env = os.environ.copy()
        env["GIT_DIR"] = str(git_dir)
        env["GIT_WORK_TREE"] = str(html_dir)
        env["GIT_INDEX_FILE"] = os.path.join(tmp, "index")

        # Stage the whole HTML tree into a throwaway index. `--force` overrides
        # any ignore rule that would otherwise skip Sphinx's `_static` output.
        _git_output(["add", "--all", "--force", "."], env=env, cwd=str(html_dir))
        # Drop Sphinx build artifacts that are not part of the served site.
        _git_output(
            ["rm", "--cached", "-r", "--quiet", "--ignore-unmatch", ".doctrees", ".buildinfo"],
            env=env,
        )
        tree = _git_output(["write-tree"], env=env)
        # A parentless commit keeps the branch a single throwaway snapshot.
        commit = _git_output(["commit-tree", tree, "-m", message], env=env)

    # Replace the remote branch with the freshly built tree.
    _git_output(["push", "--force", args.remote, "{:s}:refs/heads/{:s}".format(commit, args.branch)])

    print("Published {:s} to {:s}/{:s}.".format(source_rev, args.remote, args.branch))


if __name__ == "__main__":
    main()
