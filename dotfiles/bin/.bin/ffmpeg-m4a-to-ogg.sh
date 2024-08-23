#!/usr/bin/env sh

OPTIONS="-vn -c:a libvorbis -q:a 8"

ffmpeg -i $INPUTFILE $options $OUTPUTFILE
