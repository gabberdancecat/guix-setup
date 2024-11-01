#!/bin/sh

# pyenv_run [venv_dir] [py_cmd] [args...]
pyenv_run() {
    # Path to the virtual environment and python run cmd
    VENV_DIR="$1"
    PY_CMD="$2"
    shift 2

    # check if venv exists at path
    [ -d "$VENV_DIR" ] || { echo "venv not found at $VENV_DIR"; exit 1; }

    # activate venv
    source "$VENV_DIR/bin/activate"

    # run
    python3 $PY_CMD "$@"

    # get exit code of run command
    EXIT_CODE=$?

    # deactivate venv
    deactivate

    # exit with same exit code as run command
    exit $EXIT_CODE
}
