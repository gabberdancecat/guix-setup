#!/bin/sh

args="$@"
  
yt-dlp -F $args | less

echo "Continue? (Y/n)"
read in
[ "$in" = ""  ] && in="y"
[ "$in" = "n" ] && exit 0

yt-dlp --embed-thumbnail --format 140 -o "~/Music/downloads/%(title)s.%(ext)s" -v $args
