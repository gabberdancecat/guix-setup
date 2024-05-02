#!/bin/sh

# uvcvideo

LOAD_CMD="sudo modprobe uvcvideo"
UNLOAD_CMD="sudo modprobe -r uvcvideo"

# return 1 if loaded, 0 if not
module_is_loaded() {
    lsmod | grep -q uvcvideo
    echo $((! $?)) # flip 1 / 0
}

load_fn() {
    echo "Loading..."
    # echo "command: \"$LOAD_CMD\""
    $LOAD_CMD
}

unload_fn() {
    echo "Unloading..."
    # echo "command: \"$UNLOAD_CMD\""
    $UNLOAD_CMD
}

toggle_load_state() {
    if [ $(module_is_loaded) = 1 ]; then
	unload_fn
    else
	load_fn
    fi
}

print_load_state() {
    if [ $(module_is_loaded) = 1 ]; then
	echo "STATE: loaded"
    else
	echo "STATE: unloaded"
    fi
}

main() {
    print_load_state
    toggle_load_state
    print_load_state
}

main
