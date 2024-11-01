#!/bin/sh

set -m  # Start a new process group

cleanup() { pkill -P $$; }  # kill all processes in this process group
die() { echo "Error: $*"; exit 1; }

trap cleanup EXIT  # Ensure cleanup on termination signals

# create vars
SCRIPT_DIR="$(dirname "$(realpath -s "$0")")"  # make sure init.scm is in same dir
SWAYER_INIT="$SCRIPT_DIR/init.scm"
SWAYER_LOG="/tmp/guile-swayer.log"
[ ! -d "$SCRIPT_DIR" ] && die "SCRIPT_DIR is invalid: $SCRIPT_DIR"
[ ! -f "$SWAYER_INIT" ] && die "SWAYER_INIT is invalid: $SWAYER_INIT"

# run guile-swayer (to screen: stderr only, to log: stdout and stderr)
guile "$SWAYER_INIT" >> "$SWAYER_LOG" 2>&1 | tee -a "$SWAYER_LOG" >&2 &

wait  # Wait for all background processes
