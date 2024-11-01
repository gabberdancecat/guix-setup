#!/bin/sh

### ./script.sh [INTERVAL(def:600)] [SOCKET(def:/tmp/qemu-monitor.sock)]

## functions

error() {
    echo "> Error: $@, exiting..." | tee -a "$LOG_FILE"
    cleanup
    exit 1
}

cleanup() {
    echo "> Cleaning up..." | tee -a "$LOG_FILE"
    rm -f "/tmp/qemu-snapshotter.lock"  # Remove the lock file
    exit 0  # Exit the script
}

## args

INTERVAL="${1:-600}" # def: 10mins

SOCKET="${2:-/tmp/qemu-monitor.sock}"

echo "> LOG: SOCKET: $SOCKET, INTERVAL: $INTERVAL, DATE: $DATE"

## Lockfiles
lock_file="/tmp/qemu-snapshotter.lock"
[ -f "$lock_file" ] && error "snapshot script is already running."
touch "$lock_file"
trap cleanup INT TERM EXIT

## main

LOG_FILE="/tmp/qemu-snapshot.log"
echo "> Starting snapshots..." | tee -a "$LOG_FILE"

while true; do
    [ -S "$SOCKET" ] || error "socket not found: $SOCKET"

    ID=$(shuf -i 10-99 -n 1)
    SNAPSHOT_NAME="snap_${ID}.qcow2"

    echo "savevm \"$SNAPSHOT_NAME\"" | \
	socat - UNIX-CONNECT:"$SOCKET" >> "$LOG_FILE" 2>&1 || {
	error "Failed to send savevm command to VM"
    }

    echo "> Snapshot created: $SNAPSHOT_NAME" | tee -a "$LOG_FILE"

    sleep "$INTERVAL"
done
