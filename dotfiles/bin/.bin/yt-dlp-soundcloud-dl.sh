#!/usr/bin/env sh

##
## 
## For downloading whole playlist, 
## --download-archive ~/Music/yt-dlp/<playlist>-downloaded.txt --no-overwrites
##

## Generic functions

die() { echo "Error: $@, exiting..." ; exit 1; }

usage() {
    cat <<EOF
Usage: $(basename $0) [url] optional: [path] [proceed_all] [extra yt-dlp args]
EOF
}

## Process args

[ "$#" -lt 1 ] && usage && exit 0

url="$1"
path="${2:-.}"
proceed_all="${3:-0}"
extra_args_for_dlp="$4"

base_args="--embed-thumbnail --embed-metadata -o %(uploader)s_-_%(title)s.%(ext)s -P $path"
raw_fmt_args="-f raw -x --audio-format m4a"
non_raw_fmt_args="-f best"

## Get available formats

formats_raw="$(yt-dlp -F "$url" | sed '/\[.*\].*:.*/d')"
echo -e "> Available formats:\n${formats_raw}\n"

download_available="$(echo "$formats_raw" | awk '$1=="download" ')"
wav_available="$(echo "$formats_raw" | awk '$2=="wav" ')"
[ "$download_available" = "" ] && [ "$wav_available" != "" ] &&
    die "no 'download' but has 'wav'???"
[ "$download_available" != "" ] && [ "$wav_available" = "" ] &&
    die "has 'download' but no 'wav'???"

if [ "$wav_available" = "" ]; then
    echo -e "> Downloading default format...\n"
    wav_available=0
    extra_args="$non_raw_fmt_args"
else
    echo -e "> A WAV is available. Using extra args ${raw_fmt_args}...\n"
    wav_available=1
    extra_args="$raw_fmt_args"
fi

## run command

command="yt-dlp $base_args $extra_args $url"

echo -e "> Command to run:\n${command}\n"

output_dir="`pwd`/$path"
echo -e "> Output directory?: $output_dir\n"

if [ "$proceed_all" != 1 ]; then
    echo "> Proceed? (ret/C-c)"
    read in
fi

$command

echo "> Finished!"
