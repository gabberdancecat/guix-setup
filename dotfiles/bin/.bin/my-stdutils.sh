#!/usr/bin/env bash

## Info:

# Personal standard bash functions
# - err
# - yes_no_prompt
#

set -E
export tmpDir="$(mktemp -d)"
trap 'rm -rf -- "$tmpDir"' EXIT

err() {
    echo -e "\nERROR:"
    echo "-------"
    for i in {10..1}; do
	[ -n "${FUNCNAME[$i]}" ] && echo "$i: ${FUNCNAME[$i]}"
    done
    echo -e "err: $@, exiting...\n"
    exit 1
}

err_if() {
    [ "$1" = "$2" ] && err "${@:3}"
}

export prompt=""
export result=""

_yn_prompt_gen() {
    [ -z "$1" ] && err "${FUNCNAME[1]} param 1 (\$prompt) is not set or empty"
    [ -z "$2+x" ] && err "${FUNCNAME[1]} param 2 (\$default) is not set"
    local prompt1="$1"
    local default="${2,,}"
    local prompt_ext=$(
	( [ "$default" = y ] && echo 'Y/n' ) ||
	( [ "$default" = n ] && echo 'y/N' ) ||
	( [ "$default" = '' ] && echo 'y/n' ) ||
	( echo "err" ))
    err_if "$prompt_ext" "err" "${FUNCNAME[0]} param 2 \$default is invalid: $2"
    # set global var $prompt to new prompt
    prompt="$(echo -e "$prompt1 ($prompt_ext)\n> ")"
}

yn_prompt() {
    local prompt1="$1"
    local default="$2"
    _yn_prompt_gen "$prompt1" "$default"
    # loop:
    while true; do
	read -p "$prompt" input
	# set $input to $default if empty return
	[ "$input" = "" ] && input="$default"
	case $input in
            [Yy]* ) return 0;;  # yes
            [Nn]* ) return 1;;  # no
            * ) echo "Please answer yes or no.";;
	esac
    done
}

yn_proceed_prompt() {
    local prompt1="${1:-Continue?}"
    local default="$2"
    _yn_prompt_gen "$prompt1" "$default"
    # loop:
    while true; do
	read -n 1 -p "$prompt" input
	echo
	# set $input to $default if empty return
	[ "$input" = "" ] && input="$default"
	case $input in
            [Yy] ) return 0;;  # yes
            [Nn] ) return 1;;  # no
            * ) echo "Please answer yes or no.";;
	esac
    done
}

# TODO range separated, multi-select, saves into $result
options_prompt_num() {
    local prompt1="$1"
    local options=("${@:2}")
}

# fzf multi-select, saves into $result
options_prompt_fzf() {
    local prompt1="$1"
    local options=("${@:2}")
    result="$(printf "%s\n" "${options[@]}" | fzf --multi --prompt="$prompt1")" ||
	err "failed to fzf";
}

# fzf single-select, saves into $result
option_prompt_fzf() {
    local prompt1="$1"
    local options=("${@:2}")
    result="$(printf "%s\n" "${options[@]}" | fzf --prompt="$prompt1")" ||
	err "failed to fzf";
}
