#!/bin/sh
volume=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | sed s/%//)
muted=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{print $2}')

if [[ "$muted" == "yes" ]]; then
        echo "$volume | muted "
else
        echo "$volume"
fi
