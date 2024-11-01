#!/bin/sh

source pyenv-template-lib.sh

VENV_DIR="$HOME/code/ctf/Resources/rsactftool-pyenv"
PY_CMD="$VENV_DIR/RsaCtfTool/RsaCtfTool.py"

pyenv_run "$VENV_DIR" "$PY_CMD" "$@"

exit $?
