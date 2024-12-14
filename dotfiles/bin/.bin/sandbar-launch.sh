#!/bin/sh

# sandbar
pkill -f '^sandbar'
$HOME/.config/river/bar

# sandbar status
pkill -f '^sh.*\\.config/river/status'
$HOME/.config/river/status
