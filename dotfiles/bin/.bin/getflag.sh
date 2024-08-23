#!/bin/sh

[ $# -le 0 ] && echo "Requires one argument for name of flag, exiting..." && exit 1
piped="$(cat)"
name="$1"

echo "$piped" | grep -oE "$name{.*}" --color=none
