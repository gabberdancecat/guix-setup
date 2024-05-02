#!/bin/sh

# Ctrl+Shift to focus

# DEF_ASPECT_RATIO=800x600
DEF_ASPECT_RATIO=1600x800

###############

params="$@"

# [ $# = 0 ] && echo "Error, enter WM (e.g. stumpwm)." && exit 1
if [ $# = 0 ]; then
    echo "No WM to start received."
    echo "Start default, stumpwm? (Y/n)"
    read in
    if [ "$in" = "y" ] || [ "$in" = "" ]; then
	params="stumpwm"
    else
	echo "exiting..." && exit 1;
    fi
fi

###############

trap 'killall stumpwm ; killall Xephyr' EXIT

# Xephyr -br -ac -noreset -screen $DEF_ASPECT_RATIO :1 &
Xephyr -br -ac -noreset -resizeable -parent -screen :1 &

DISPLAY=:1 $params

# 
