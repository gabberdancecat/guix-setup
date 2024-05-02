#!/bin/sh

## Arkenfox updater script
# - Run both updater and cleaner after
#   make changes to user-overrides.js or new update.
#
# Reminders:
# - Add RSS feed for new releases/commits
# - Look through gui settings wiki, linked in github repo.
#

profileName="3eozzsqk.default"

die() { echo "Error: $@." 1>&2 ; exit 1; }

profileParent="$HOME/.mozilla/firefox"
profileDir="$profileParent/$profileName"

run_updater() {
    echo "Running updater.sh..."
    $profileDir/updater.sh
    echo "Ran updater.sh"
}

run_cleaner() {
    echo -e "\nRunning prefsCleaner.sh..."
    echo "Make sure Firefox is closed! enter to continue..."
    prompt IGNORE IGNORE 0
    $profileDir/prefsCleaner.sh
}

make_lowercase() {
    echo $(echo "$1" | tr '[:upper:]' '[:lower:]')
}

# takes 3 args: $1 ret 0 if match, $2 ret 1 if match, $3 # to ret if match.
prompt() {
    # process args
    [ "$*" = "quit" ] || [ "$*" = "exit" ] && exit 0 ;
    local args=( "$@" )
    for i in "${args[@]}" ; do
	[ "$i" != "IGNORE" ] && i="$(make_lowercase "$i")"
    done
    local pos="${args[0]}"
    local neg="${args[1]}"
    local def="${args[2]}"
    # repl
    local stdin=""
    while true ; do
	echo -n "> "
	read stdin
	stdin="$(make_lowercase "$stdin")"
	# if Return, do set stdin to def
	if [ "$stdin" = "" ] ; then
	    return $def
	fi
	# check if match pos or neg
	if [[ "${stdin}" == @($pos) ]] && [ "$pos" != "IGNORE" ]; then
	    return 0
	elif [[ "${stdin}" == @($neg) ]] && [ "$neg" != "IGNORE" ]; then
	    return 1
	else
	    echo "Invalid option, try again."
	fi
    done
}

### Main function

body() {

    # check firefox directory dir
    [ -d "$profileParent" ] && cd $profileParent ||
	    die "Firefox is not installed you dummy ;3"

    # print all firefox profiles
    echo -e "all profiles:\n-------------------"
    ls -1
    echo -e "-------------------\n"

    # die if profile not found
    [ -d "$profileDir" ] ||
	die "Specified profile $profileDir does not exist in $profileParent"

    # is default profile correct? if not, enter one
    echo "Is '$profileName' the correct profile? (Y/n)"
    if ! prompt 'y|yes' 'n|no' '0' ; then
	echo -en "Enter the name of the profile\n> "
	read stdin
	profileDir="$profileParent/$stdin"
	[ -d "$profileDir" ] ||
	    die "Directory '$profileDir' does not exist!"
	echo "Note: make sure to update the script with the correct firefox profile!"
    fi

    # create profile backup if not exists
    if [ "$(ls | grep -c "$profileName-backup")" -eq 0 ]; then
	echo "Missing backup profile '$profileName-backup', create? (Y/n)"
	if prompt 'y|yes' 'n|no' '0' ; then
	    cp -r ./"$profileName" ./"$profileName-backup" || die "Not in correct dir"
	    echo "- Backup $profileName-backup created"
	else
	    echo "exiting..." ; exit 0 ;
	fi
    fi

    # cd into profileDir
    cd "$profileDir" || die "Failed to enter $profileDir"

    # symlink user-overrides.js if not already present in profileDir
    if ! [ -L "$profileDir/user-overrides.js" ]; then
	echo "user-overrides.js does not exist, symlink? (Y/n)"
	if prompt 'y|yes' 'n|no' '0' ; then
	    ln -s ~/.bin/user-overrides.js ~/.mozilla/firefox/$profileName/user-overrides.js &&
		echo "Symlink created." ||
		    die "symlink failed to create"
	else
	    echo "exiting..." ; exit 0 ;
	fi
    else
	echo "- Symlink to user-overrides.js found"
    fi

    # check if user.js, updater, and user.js exist in profileDir
    [ -e "$profileDir/user.js" ] ||
	die "user.js doesn't exist, please copy or symlink from arkenfox repo"
    [ -e "$profileDir/updater.sh" ] ||
	die "updater.sh doesn't exist, please copy or symlink from arkenfox repo"
    [ -e "$profileDir/prefsCleaner.sh" ] ||
	die "prefsCleaner.sh doesn't exist, please copy or symlink from arkenfox repo"

    # prompt main action
    while true ; do
	echo "What action would you like to take?"
	echo "Note: run both scripts if modified user-overrides.js or updated firefox."
	echo "y|<RET>) run updater.sh and prefsCleaner.sh (<default>)"
	echo "u) run only updater.sh"
	echo "p) run only prefsCleaner.sh"
	read stdin
	[ "$stdin" = "" ] && stdin="y"
	[ "$stdin" = "quit" ] || [ "$stdin" = "exit" ] && exit 0 ;
	case $stdin in
            y)
		run_updater
		run_cleaner
		return
		;;
            u)
		run_updater
		return
		;;
            c)
		run_cleaner
		return
		;;
            *)
		echo "invalid option, try again..."
		continue
	esac
    done
}

### Main

echo -e "\nArkenfox updater\n"

body

echo -e "\nFinished..."

