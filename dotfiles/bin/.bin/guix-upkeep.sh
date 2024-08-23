#!/usr/bin/env bash

source ~/.bin/my-stdutils.sh

## Guix System upkeep:
#
# Ideas:
# - prompt for input
#  - enter range like 1-6, or 1 2 3 ?
#

## Functions: 

# die() { echo -e "\nError: $@. exiting..." ; exit 1 ; }

## Variables:

categories=("updates" "backups" "checkups")

updates_lst=(
    "1. guix-update-channels"
    "2. guix-update-profiles all"
    "3. guix-system-reconfigure 3"
    "4. nix-update-packages y"
    "5. flatpak-update"
    "6. arkenfox-updater.sh"
)
backups_lst=(
    "1. snapper backup"
)
checkups_lst=(
    "1. guix gc --verify=contents"
)

## Main:

main() {
    echo "----------------------------"
    echo " Guix System upkeep script: "
    echo "----------------------------"

    # prompt for category (single)
    option_prompt_fzf "Select category: " "${categories[@]}"
    case $result in
	updates ) subcmds=("${updates_lst[@]}") ;;
	backups ) subcmds=("${backups_lst[@]}") ;;
	checkups) subcmds=("${checkups_lst[@]}") ;;
	*) err "invalid result type" ;;
    esac

    # prompt for commands in category (multiple)
    options_prompt_fzf "Select types: " "${subcmds[@]}"

    # print before continuing
    echo "To run:"
    echo "--------"
    for i in "${result[@]}"; do
	echo "$i"
    done
    echo "--------"
    yn_proceed_prompt "Proceed?" Y

    # execute
    for i in "${result[@]}"; do
	eval "$(echo "$i" | sed "s|[0-9]*\.\s||")"
    done

    # finished
    echo -e "\nFinished! exiting...\n"
}

main "$@"
