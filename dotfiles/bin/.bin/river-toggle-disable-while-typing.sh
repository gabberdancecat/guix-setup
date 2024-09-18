#!/usr/bin/env sh

# riverctl input pointer-1267-12864-ELAN0412:00_04F3:3240_Touchpad disable-while-typing disabled

input_device="pointer-1267-12864-ELAN0412:00_04F3:3240_Touchpad"

die() { echo "Error: $@. Exiting..."; exit 1; }

get_status() {
    riverctl list-input-configs |
	awk "/$input_device/{flag=1;next} /^$/{flag=0} flag{print}" |
	grep "disable-while-typing" |
	sed 's/.*: //'
}

set_status() {
    [ $# -le 0 ] && die "set_status: not enough inputs"

    if [ $1 = 1 ]; then
	echo "> enabling..."
	riverctl input $input_device disable-while-typing enabled
    elif [ $1 = 0 ]; then
	echo "> disabling..."
	riverctl input $input_device disable-while-typing disabled
    else
	die "set_status: invalid arg: $1"
    fi
}

toggle_status() {
    status="$(get_status)"
    if [ "$status" = 'enabled' ]; then
	set_status 0
    elif [ "$status" = 'disabled' ]; then
	set_status 1
    else
	die "toggle_status: invalid status received: $status"
    fi
}

main() {
    echo "> Status: $(get_status)"
    if [ $# = 0 ]; then
	toggle_status
    else
	if [[ "$1" == @(enable|enabled|1) ]]; then
	    set_status 1
	elif [[ "$1" == @(disable|enabled|0) ]]; then
	    set_status 0
	else
	    die "invalid input to command: $1"
	fi
    fi
    echo "> Status: $(get_status)"
    echo "> Done."
}

main "$@"

