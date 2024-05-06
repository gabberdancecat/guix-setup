#!/bin/sh

###                            ###
### Guix System upkeep script: ###
###                            ###

exit 1

#### Moved to elisp script!

## MAKE this an elisp script???????????? might be easier with X prompt or something that Xah Lee uses in the bionic font script...

#
# To run:
# - arkenfox (after any guix profile update) - check if new firefox ver
# - 
#
# Ideas:
# - prompt for input
#  - enter range like 1-6, or 1 2 3 ?

die() { echo -e "\nError: $@. exiting..." ; exit 1 ; }

cmd_checks=(
    "guix gc --verify=contents"
)


cmd_backups=(
    "snapper backup"
)

cmd_updates=(
    "1. guix-update-channels"
    "2. guix-update-profiles all"
    "3. guix upgrade"
    "4. guix-system-reconfigure 3"
    "5. nix-update-packages y"
    "6. flatpak-update"
    "7. arkenfox-updater.sh"
)

y_or_n() {
    while true ; do
	echo -n "> "
	read stdin
	# if ret w/ no input, do default ($1)
	if [ "$stdin" = "" ]; then
	    stdin="$1"
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
    unset stdin
}

process_updates() {
    echo -e "\nListing all commands:"
    for((i=0; i<${#cmd_updates[@]}; i++)); do
       echo "$i. ${cmd_updates[i]}"
    done
    echo "done"
}

process_backups() {
    echo "placeholder"
}

process_checks() {
    echo "placeholder"
}

main() {
    echo -e "
-------------------------
 ~  Update All Script  ~ 
-------------------------"
#     echo -e "
# > Enter action to run:
#   Updates(1)/Checks(2)/Backups(3)
#   all_updates(4)/everything(5)"
    
    main_fzf=(
	"1. cmd_updates"
	"2. cmd_backups"
	"3. cmd_checks"
    )
    main_fzf_ret=$(
	printf "%s\n" "${main_fzf[@]}" |
	    fzf --header "Select operation type" |
	    awk '{print $2}')
    echo $main_fzf_ret
    dumparray $main_fzf_ret

    exit 1

    
    
    exit 1

    echo -n "> "
    read stdin
    [[ "$(echo $stdin | grep '^[0-9]*$')" -gt 0 ]] ||
	die "must be number"
    case $stdin in
	1)
	    process_updates
	    ;;
	2)
	    process_checks
	    ;;
	3)
	    process_backups
	    ;;
	4)
	    
	    ;;
	5)
	    
	    ;;
	*)
	    
    esac

}

main "$@"
