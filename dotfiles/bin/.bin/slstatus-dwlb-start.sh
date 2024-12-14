#!/bin/sh

##
## note: killing slstatus will kill the process being piped to 
##

## Variables

# enable for debugging
debug=1

# get current state of processes
slstatus_proc="$(  pgrep -f "^slstatus"		  )"
dwlbMain_proc="$(  pgrep -f "^dwlb$"		  )"
dwlbStdin_proc="$( pgrep -f "^dwlb -status-stdin" )"

## Functions

DEBUG() {
    if [ "$debug" = "1" ]; then
	echo "Debug: $@"
	echo "$@" >> /tmp/slstatus-dwlb-start-debug
    fi
}

kill_slstatus_and_maybe_dwlb_stdin() {
    pkill -f "slstatus"
    DEBUG 'kill_slstatus_and_maybe_dwlb_stdin (killing slstatus)'
}

kill_dwlb_stdin_only() {
    pkill -f "^dwlb -status-stdin"
    DEBUG 'kill_dwlb_stdin_only (killing dwlbstdin)'
}

cleanup_running() {
    DEBUG 'cleaning up slstatus and dwlbstdin if either running'
    if [ -n "$slstatus_proc" ]; then
	kill_slstatus_and_maybe_dwlb_stdin
    else
	if [ -n "$dwlbStdin_proc" ]; then
	    kill_dwlb_stdin_only
	fi
    fi
}

start_slstatus_and_dwlb_stdin() {
    DEBUG 'starting slstatus and dwlbstdin'
    slstatus -s | dwlb -status-stdin all &
}

main() {

    if [ -z "$dwlbMain_proc" ]; then
	echo "dwlb main process is not running"
	DEBUG 'dwlb not running'
	cleanup_running
	exit 1
    fi

    if [ -z "$slstatus_proc" ] || [ -z "$dwlbStdin_proc" ]; then
	DEBUG 'neither slstatus nor dwlbstdin are running, start slstatus and dwlbstdin'
	cleanup_running
	start_slstatus_and_dwlb_stdin
	exit 0
    fi
}

main "$@"
