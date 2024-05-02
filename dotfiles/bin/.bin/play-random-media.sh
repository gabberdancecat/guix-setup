#!/bin/sh

# Notes:
# - use Enter to go to next song
# - use < / > to go prev/next song
# - use Up/Down to seek by 60 seconds

main() {
    [ $# = 0 ] && { echo "not enough args" ; exit 1 ; }

    mpv --shuffle=yes --term-status-msg='Playing: ${?media-title:${media-title}}${!media-title:No file}' $1
}

main "$@"
