#!/bin/sh

### INFO:
## Script made for converting m4a into mkv video using its album cover
##  as stillimage video!
## Still a WIP. :3
##


### Important functions ###

die() { echo -e "> Error! $1.\n" ; exit 1; }

yes_or_no () {
    while : ; do
        echo -n "> "
        local stdin=
        read stdin
        # if ret with no input, do default
        if [ "$stdin" = "" ]; then
            stdin=$1
        fi
	if [[ "${stdin}" == @(y|yes|Y|YES) ]]; then
	    break
	elif [[ "${stdin}" == @(n|no|N|NO) ]]; then
	    echo "Exiting..."
	    exit 0
	else
	    echo "Invalid option, try again."
	fi
    done
}

print_usage() {
    cat <<EOF
Usage: $(basename $0) -i [INPUT] ... -o [OUTPUT]

EXAMPLES:
  $(basename $0) -i [INPUT]
  $(basename $0) -i [INPUT] -o [OUTPUT]
 
  If OUTPUT is not specified, create output file at working directory
  with the same name but different file extension.

DESCRIPTION:
  A script for converting m4a into mkv files with the album cover as a stillimage video.
  Optimized for uploading onto youtube.

ARGUMENTS:
  -h     show print menu
 
  -d     print debug information when running
 
  -i [INPUT]
         specify input file (m4a)
 
  -o [OUTPUT]
         specify output file (mkv) (optional, will be set to INPUT.mkv in the working directory if not specified)
 
EOF
    exit 0
}


### FFMPEG command variations ###

## Variation 1 (original method, slow, found better solutions...):
## - extract cover, create black video with audio, add cover as
##   overlay with fixed scale2ref resizing.
ffmpeg_1 () {
    IFS='' read -r -d '' CMD <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die

# create black video with input audio
ffmpeg \
    -f lavfi -i color=c=black:s=$Xx$Y:r=5 \
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
}


## Variation 2 (Optimal? fast, good video quality):
## - extract cover, create video with audio and cover with fixed
##   resizing and pad.
ffmpeg_2 () {
    IFS='' read -r -d '' CMD <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die "failed to extract cover"

# create video with cover scaled with black padding
ffmpeg \
    -loop 1 \
    -i $tmp_cover \
    -i "$INPUT" \
    -vf "scale=$X:$Y:force_original_aspect_ratio=decrease:flags=lanczos,
pad=$X:$Y:-1:-1:color=black" \
    -crf 18 -preset ultrafast -tune stillimage \
    -shortest \
    -c:a copy \
    "$OUTPUT" || die "failed to create video"
EOF
}

## Variation 3 (slower, unnecessary?)
## - extract cover, create video with cover NOT scaled, scale video
##   with padding.
ffmpeg_3 () {
    IFS='' read -r -d '' CMD <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die "failed to extract cover"

# create video with song and cover without scaling
ffmpeg \
    -loop 1 -i $tmp_cover \
    -i "$INPUT" \
    -crf 18 -preset ultrafast \
    -c:a copy \
    -c:v libx264 -tune stillimage \
    -shortest \
    $tmp_video || die "failed to create video"

# scale video afterwards
ffmpeg \
    -i $tmp_video \
    -vf "scale=$X:$Y:force_original_aspect_ratio=decrease:flags=lanczos,
pad=$X:$Y:-1:-1:color=black" \
    -crf 18 -preset ultrafast -tune stillimage -strict -2 \
    "$OUTPUT" || die "failed to create video"
EOF
}

## Variation 4 (best video results, but unpredictable file resolution)
## - extract cover, create video with cover scaled to minimum fit,
##   might go over.
ffmpeg_4 () {
    IFS='' read -r -d '' CMD <<"EOF"
# extract cover
ffmpeg \
    -y -i "$INPUT" -an \
    $tmp_cover || die "failed to extract cover"

# create video with cover scaled with black padding
ffmpeg \
    -loop 1 \
    -i $tmp_cover \
    -i "$INPUT" \
    -vf "scale='min($X,iw)':'min($Y,ih)'" \
    -crf 18 -preset ultrafast -tune stillimage \
    -shortest \
    -c:a copy \
    "$OUTPUT" || die "failed to create video"
EOF
}


### Main functions ###

# startup stuff
init() {
    # clean up script when exit
    tmp_dir=$(mktemp -d -t ffmpeg-me-uwu-XXXX) || die
    trap "test -d $tmp_dir && rm -fr $tmp_dir" EXIT || die
}

# requires "$@" to be passed to function to get script args
process_script_args() {
    # if no args to script, print usage
    [ "$#" -eq 0 ] && print_usage ;

    # process args
    while getopts ":i:o:h:d" opt; do
	case $opt in
	    # Follow-up arg required
	    i)
		INPUT_RAW="$OPTARG" ;;
	    o)
		OUTPUT_RAW="$OPTARG" ;;
	    # No follow-up arg required
	    h)
		print_usage ;;
	    d)
		DEBUG=1 ;; # print extra info
	    *)
		die "Invalid argument option, run with -h to view available" ;;
	esac
    done

    # define input variables
    
    WORKING_DIR="$(pwd)"

    INPUT_REALPATH="$(readlink -f "$INPUT_RAW")"
    INPUT_BASENAME="$(basename "$INPUT_REALPATH")"
    INPUT_DIRNAME="$(dirname "$INPUT_REALPATH")"
    INPUT_EXT="${INPUT_BASENAME##*.}"
    INPUT="$INPUT_REALPATH"
    
    # exit if no -i INPUT specified
    [ -z "$INPUT_RAW" ] &&
	die "No input file specified, use -i to specify input file"

    # set default -o OUTPUT value if not specified
    if [ -z "$OUTPUT_RAW" ]; then
	OUTPUT_RAW="$WORKING_DIR/${INPUT_BASENAME%.*}.mkv"
	echo "> Warning: auto setting output file name: \"$(basename "$OUTPUT_RAW")\""
    fi

    # define output variables

    OUTPUT_REALPATH="$(readlink -f "$OUTPUT_RAW")"
    [ -d "$OUTPUT_REALPATH" ] && OUTPUT_REALPATH="$OUTPUT_REALPATH/${INPUT_BASENAME%.*}.mkv"
    OUTPUT_BASENAME="$(basename "$OUTPUT_REALPATH")"
    OUTPUT_DIRNAME="$(dirname "$OUTPUT_REALPATH")"
    OUTPUT_EXT="${OUTPUT_BASENAME##*.}"
    OUTPUT="$OUTPUT_REALPATH"

    # misc...

    # temp files
    tmp_cover="$tmp_dir/tmp_cover.png"
    tmp_video="$tmp_dir/tmp_video.$OUTPUT_EXT"

    # output resolution (TODO: prompt user for resolution)
    X="1920"
    Y="1080"
}

# errors and warnings
args_error_checking() {
    # print variables if debug
    if [ "$DEBUG" = 1 ]; then
	echo "input raw: $INPUT_RAW"
	echo "output raw: $OUTPUT_RAW"
	echo "working dir: $WORKING_DIR"
	echo "input realpath: $INPUT_REALPATH"
	echo "input basename: $INPUT_BASENAME"
	echo "input dirname: $INPUT_DIRNAME"
	echo "input ext: $INPUT_EXT"
	echo "input: $INPUT"
	echo "output realpath: $OUTPUT_REALPATH"
	echo "output basename: $OUTPUT_BASENAME"
	echo "output dirname: $OUTPUT_DIRNAME"
	echo "output ext: $OUTPUT_EXT"
	echo "output: $OUTPUT"
    fi

    # exit if path to input file does not exist
    if [ ! -f "$INPUT_REALPATH" ]; then
	die "Path to input file \"$INPUT_REALPATH\" does not exist"
    fi

    # exit if dirname of output file does not exist
    if [ ! -d "$OUTPUT_DIRNAME" ]; then
	die "Directory of output file \"$OUTPUT_DIRNAME\" does not exist"
    fi

    # warn if input file is not m4a
    if [ "$INPUT_EXT" != "m4a" ]; then
	echo "> Warning: this script is made for processing m4a files"
    fi

    # warn if output file is not mkv
    if [ "$OUTPUT_EXT" != "mkv" ]; then
	echo "> Warning: this script is made for creating mkv videos"
    fi
}

# review args before proceeding
review_args() {
    # Print variables to screen for review:
    echo 
    echo "INPUT: $INPUT"
    echo "OUTPUT: $OUTPUT"
    echo

    echo "Proceed? [Y/n]"
    yes_or_no "Y"
}

# run the ffmpeg command
run_ffmpeg_command () {
    ## Run ffmpeg command variant to set $CMD
    ## TODO: prompt for variant to run!
    # ffmpeg_1
    ffmpeg_2  # best one?
    # ffmpeg_3
    # ffmpeg_4

    # print full command before running
    echo -e "\nCommand to execute:"
    echo -e "---------------------------\n"
    echo -e "X=$X; Y=$Y\n"
    echo "$CMD"
    echo -e "---------------------------\n"

    echo "Press enter to continue [Y/n]"
    yes_or_no "Y"
    
    ## Processing: --
    
    # execute
    eval "$CMD"

    echo "Completed"
}

main() {
    init
    process_script_args "$@"
    args_error_checking
    review_args
    run_ffmpeg_command
}

main "$@"

