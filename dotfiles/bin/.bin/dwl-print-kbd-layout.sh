#!/bin/sh

value="$(cat /tmp/dwl-keymap)"

if [ "$value" == "English (programmer Dvorak)" ]; then
    output="dvp"
elif [ "$value" == "English (US)" ]; then
    output="qwerty"
else
    output="$value"
fi

printf "$output"
