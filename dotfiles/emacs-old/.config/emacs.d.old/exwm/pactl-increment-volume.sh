#!/bin/sh
volume=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | sed s/%//) # e.g. 60
origInput="$1" # e.g. +5%
posOrNeg=$(echo $origInput | awk '{print substr($0,0,1)}') # e.g. +
input=${origInput//[!0-9]/} # e.g. 5

if [[ "$posOrNeg" == "+" ]]; then
    newVol=$(($volume + $input)) # e.g. 65
elif [[ "$posOrNeg" == "-" ]]; then
    newVol=$(($volume - $input)) # e.g. 65
else
    echo "input doesn't contain + or - !!"
    exit 1
fi

if [[ "$newVol" -le "100" ]]; then
    pactl set-sink-volume @DEFAULT_SINK@ $posOrNeg$input%
fi
