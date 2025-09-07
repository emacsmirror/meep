#!/usr/bin/env bash
BASEDIR=$(realpath $(dirname "$0"))
exec emacs --init-directory "$BASEDIR/init/default" --debug-init "$@"
