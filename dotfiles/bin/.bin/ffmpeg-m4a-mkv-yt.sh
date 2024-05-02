#!/bin/sh

## Command to run optimized for mkv video output!
## TODO: support path to file as argument (currently expects input to be in same dir)

## Important functions ##

die() { echo "Error!" ; exit 1; }

print_usage() {
    cat <<EOF
Usage: $(basename $0) [ARGUMENTS]

EXAMPLES:
 $(basename $0) -i [INPUT]
 $(basename $0) -i [INPUT] -o [OUTPUT]

 If OUTPUT is not specified, create output file at current location with
 same name but different file extension.

EOF
}

## Process Args ##

[ "$#" -eq 0 ] && print_usage

# while getopts ":i:o:h" opt; do
#     case $opt in
# 	i)
# 	    INPUT_PATH="$OPTARG" ;;
# 	o)
# 	    OUTPUT_PATH="$OPTARG" ;;
# 	# No args required
# 	h)
# 	    print_usage ;;
# 	*)
# 	    die "Invalid argument option, run with -h to view available" ;;
#     esac
# done
#
# if [ -z $INPUT_PATH ]
# then die "No input file specified, use -i to specify path" ;;
# fi
#
#






# Need at least 1 argument
[ $# -eq 0 ] && { echo -e "\nUsage: $0 INPUT (opt: OUTPUT)\n" ; exit 1; }

ARG1="$1"
ARG2="$2"

# INPUT="$(readlink -f \"$ARG1\")" # get full path from input
INPUT="$1"
INPUT_EXT="${INPUT##*.}"
OUTPUT="${2-${INPUT%.*}.mkv}"
OUTPUT_EXT="${OUTPUT##*.}"

# # Error if input not m4a
# [ $INPUT_EXT != "m4a" ] && { echo -e "\nError: must be an m4a file format\n" ; exit 1; }

# Warn if output file is not m4a
[ $INPUT_EXT != "m4a" ] && { echo -e "\nWarning: this script is optimized for making an mkv video file.\n"; }


# warning too many params
all_args=("$@")
[ $# -gt 2 ] && { echo "Warning: $# inputs provided, ignoring: ${all_args[@]:2}"; }

echo
echo "Input: $INPUT"
echo "Output: $OUTPUT"

tmp_dir=$(mktemp -d -t ffmpreg-is-real-XXXX) || die
trap "test -d $tmp_dir && rm -fr $tmp_dir" EXIT || die

tmp_cover="$tmp_dir/tmp_cover.png"
tmp_video="$tmp_dir/tmp_video.$OUTPUT_EXT"

## Command to run:

# Variation 1 (original method, slow, found better solutions...):
#  extract cover, create black video with audio, add cover as overlay.
IFS='' read -r -d '' CMD1 <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die

# create black video with input audio
ffmpeg \
    -f lavfi -i color=c=black:s=1920x1080:r=5 \
    -i "$INPUT" \
    -shortest -preset ultrafast \
    -crf 0 \
    -c:a copy \
    $tmp_video || die

# add cover as overlay to video
ffmpeg \
    -i $tmp_video \
    -i $tmp_cover \
    -filter_complex \
    "[1:v][0:v]scale2ref=w=oh*mdar:h=ih[img][v];
     [v][img]overlay=(main_w-w)/2:(main_h-h)/2" \
    -crf 18 -tune stillimage -preset ultrafast \
    -c:a copy \
    "$OUTPUT" || die
EOF

# Variation 2 (Optimal? fast, good video quality):
#  extract cover, create video with audio and cover with resizing.
IFS='' read -r -d '' CMD2 <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die

# create video with cover scaled with black padding
ffmpeg \
    -loop 1 \
    -i $tmp_cover \
    -i "$INPUT" \
    -vf "scale=1920:1080:force_original_aspect_ratio=decrease:flags=lanczos,
pad=1920:1080:-1:-1:color=black" \
    -crf 18 -preset ultrafast -tune stillimage \
    -shortest \
    -c:a copy \
    "$OUTPUT" || die
EOF

# Variation 3 (slower, unnecessary?)
#  extract cover, create video with cover NOT scaled, scale video with padding.
IFS='' read -r -d '' CMD3 <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die

# create video with song and cover without scaling
ffmpeg \
    -loop 1 -i $tmp_cover \
    -i "$INPUT" \
    -crf 18 -preset ultrafast \
    -c:a copy \
    -c:v libx264 -tune stillimage \
    -shortest \
    $tmp_video || die

# scale video afterwards
ffmpeg \
    -i $tmp_video \
    -vf "scale=1920:1080:force_original_aspect_ratio=decrease:flags=lanczos,
pad=1920:1080:-1:-1:color=black" \
    -crf 18 -preset ultrafast -tune stillimage -strict -2 \
    "$OUTPUT" || die
EOF

# Variation 4 (best results, but unpredictable file resolution)
#  extract cover, create video with cover scaled to min fit, might overdo.
IFS='' read -r -d '' CMD4 <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die

# create video with cover scaled with black padding
ffmpeg \
    -loop 1 \
    -i $tmp_cover \
    -i "$INPUT" \
    -vf "scale='min(1920,iw)':'min(1080,ih)'" \
    -crf 18 -preset ultrafast -tune stillimage \
    -shortest \
    -c:a copy \
    "$OUTPUT" || die
EOF

    
echo -e "\n> Command to execute:\n"
echo -e "---------------------------"
echo "$CMD4"
echo -e "---------------------------\n"

echo -ne "Press enter to continue\n> "
read in

## Processing: --

# execute
eval "$CMD4"

echo "Completed"
