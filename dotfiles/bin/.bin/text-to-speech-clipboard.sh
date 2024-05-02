#!/bin/sh

TMP_FILE=$(mktemp)
trap 'rm -- "$TMP_FILE"' EXIT

# debug
echo "$TMP_FILE"

xclip -selection clipboard -o > $TMP_FILE

espeak-ng -f $TMP_FILE
