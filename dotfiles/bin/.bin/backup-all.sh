#!/usr/bin/env bash

source ${HOME}/.bin/my-stdutils.sh

backup_keypass() {
    # title
    echo -e "\n--- Backup keepassxc database: ---\n"
    # vars
    local database_path="${HOME}/Documents/secure/Passwords.kdbx"
    local database_backup_dir="${HOME}/Documents/secure/keepass_backups"
    local database_backup_name="$(basename "${database_path%%.*}")-$(date +"%Y-%m-%d").kdbx"
    local database_backup_path="$database_backup_dir/$database_backup_name"
    # error handling
    if [ ! -f "$database_path" ]; then
	err "database_path \"$database_path\" is invalid"
    elif [ ! -d "$database_backup_dir" ]; then
	echo "Warning: database_backup_dir \"$database_backup_dir\" is missing, creating..."
	mkdir -p "$database_backup_dir" || err "failed to create database_backup_dir"
    fi
    # print
    echo -e "> database_path: $database_path"
    echo -e "> database_backup_path: $database_backup_path\n"
    # password prompt and export
    if ! yn_proceed_prompt "Backup database?" Y ; then
	return 0
    else
	cp "$database_path" "$database_backup_path" || err "failed to copy"
	chmod 600 "$database_backup_path" || err "failed to chmod"
	echo "Success!"; return 0
    fi
}

backup_firefox() {
    # title
    echo -e "\n--- Backup firefox bookmarks: ---\n"
    # vars
    local firefox_profile="3eozzsqk.default"
    local json_path="$(find ${HOME}/.mozilla/firefox/$firefox_profile/bookmarkbackups | sort | tail -n1)"
    local json_backup_dir="${HOME}/Backups/firefox_bookmarks"
    local json_backup_name="bookmarks-$(date +"%Y-%m-%d").${json_path##*.}"
    local json_backup_path="$json_backup_dir/$json_backup_name"
    # ensure
    if [ ! -f "$json_path" ]; then
	err "json_path \"$json_path\" is invalid"
    elif [ ! -d "$json_backup_dir" ]; then
	echo "Warning: json_backup_dir \"$json_backup_dir\" is missing, creating..."
	mkdir -p "$json_backup_dir" || err "failed to create json_backup_dir"
    fi
    # print
    echo -e "> firefox_profile: $firefox_profile"
    echo -e "> json_path: $json_path"
    echo -e "> json_backup_path: $json_backup_path"
    # export
    if ! yn_proceed_prompt "Backup firefox bookmarks?" Y ; then
	return 0
    else
	cp "$json_path" "$json_backup_path" || err "failed to copy"
	echo "Success!" ; return 0
    fi
}

# backup_btrfs() {}

# copy over all backups (~/.backups & keepassxc_backups dir) to hard drive
# backup_harddrive() {}

# menu() {}


main() {
    if [[ "$1" = [yY] ]]; then
	printf "\n" | backup_keypass
	printf "\n" | backup_firefox
    else
	backup_keypass
	backup_firefox
    fi
    echo -e "\nExiting..."
    exit 0
}

main "$@"

