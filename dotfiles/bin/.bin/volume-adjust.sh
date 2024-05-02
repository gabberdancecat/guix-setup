#!/bin/sh

die() { echo "$@. exiting..." ; exit 1 ; }

[ $# -lt 1 ] && die "Not enough args"

cmd_volpd="$1"

### Functions ###

get_vol() {
    echo "$(pactl get-sink-volume @DEFAULT_SINK@ | head -n 1 | grep -Eo '[0-9]+%.*' | sed 's/%.*//')"
}

set_vol() {
    pactl set-sink-volume @DEFAULT_SINK@ $1
}

fix_vol_if_over() {
    if (( $1 > 100 )); then
	set_vol "100%"
    fi
}

### Main ###

if [ "$(echo $cmd_volpd | grep -c '%')" -ne 1 ]; then
    die "There must be a '%' following the number"
fi
if [ "$(echo $cmd_volpd | grep -c '^[+]')" -eq 1 ]; then
    plusminus=1
elif [ "$(echo $cmd_volpd | grep -c '^[-]')" -eq 1 ]; then
    plusminus=2
else
    plusminus=0
fi
echo "DEBUG: cmd_volpd: $cmd_volpd"

cur_vol=$(get_vol)
echo "DEBUG: cur_vol: $cur_vol"

if [[ $plusminus -eq 1 && $cur_vol -lt 100 ]] ||
       [[ $plusminus -eq 2 && $cur_vol -gt 0 ]] ||
       [[ $plusminus -eq 0 ]]; then
    set_vol "$cmd_volpd"
    cur_vol=$(get_vol)
    echo "DEBUG: cur_vol post: $cur_vol"
    fix_vol_if_over "$cur_vol"
fi
  
echo "new vol: $(get_vol)%"
