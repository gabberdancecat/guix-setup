#!/bin/sh

TMP_FILE=$(mktemp)
trap 'rm -- "$TMP_FILE"' EXIT

# debug
echo "$TMP_FILE"

DESKTOP_ENV="$(echo $XDG_SESSION_TYPE)"
if [ $DESKTOP_ENV = "wayland" ]; then
    PASTE_CMD="wl-paste"
elif [ $DESKTOP_ENV = "x11" ]; then
    PASTE_CMD="xclip -selection clipboard -o"
else
    echo "XDG_SESSION_TYPE???" ; exit 1 ;
fi

$PASTE_CMD > $TMP_FILE

espeak-ng -f $TMP_FILE
