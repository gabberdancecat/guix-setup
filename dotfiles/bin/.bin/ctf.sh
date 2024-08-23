#!/usr/bin/env bash

##
## Cli program for CTF stuff
##

##
## Commentary:
##
## For all commands to work optimally, source this script when running.
## Perhaps an 'alias ctf="source $HOME/.bin/ctf.sh"'
##

main() {
    args=( "$@" )
    cmd="${args[0]}"
    rest="${args[@]:1}"

    case "$cmd" in
	"finish"|"f")
	    cmd_finish "${rest[@]}" ;;
	"save"|"s")
	    cmd_save "${rest[@]}" ;;
	"grepflag"|"g")
	    cmd_grepflag "${rest[@]}" ;;
	*) echo "Invalid command name \"$cmd\""
    esac
}

cmd_finish() {
    orig_path="$(pwd)"
    echo "orig dir: $orig_path"
    cd ..
    mv "$orig_path" "${orig_path}_COMPLETED"
    echo "new dir: ${orig_path}_COMPLETED"
    echo "New working dir."
}

cmd_save() {
    # cmd="$(history | tail -n 1 | cut -b 8-)"
    cmd="$(tail -n 1 $HOME/.bash_history)"
    echo -e "> cmd: $cmd\n"
    cat <<EOF > ./get_flag.sh
#!/bin/sh

${cmd}
EOF
    echo "> contents of get_flag.sh:"
    cat ./get_flag.sh
    chmod +x get_flag.sh
    ./get_flag.sh > flag.txt
    echo 
    
    echo -e "> contents of flag.txt:"
    cat ./flag.txt
    echo
}

cmd_grepflag() {
    args=( $@ )
    name="${args[0]}"
    piped="$(cat)"

    echo "$piped" | grep -oE "$name{.*}" --color=none
}

main "$@"
