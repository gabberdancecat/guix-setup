#!/bin/sh

die() {
    echo "$*, exiting..."
    exit 1
}

prompt() {
    echo -n "> "
    read in
    if [[ "$in" = "quit" || "$in" = "exit" ]]; then
	exit 0
    fi
}

# not included: --dry-run --omit-dir-times --no-perms --inplace
defs="--progress --human-readable --verbose --recursive"

laptop="$HOME/Notes"
phone="$HOME/mount/Internal shared storage/OrgFiles"
laptopBackup="$HOME/Documents/Org-backups/Laptop-Backup-1"
laptopBackup2="$HOME/Documents/Org-backups/Laptop-Backup-2"
laptopBackup3="$HOME/Documents/Org-backups/Laptop-Backup-3"
laptopBackup4="$HOME/Documents/Org-backups/Laptop-Backup-4"
laptopBackup5="$HOME/Documents/Org-backups/Laptop-Backup-5"
phoneBackup="$HOME/Documents/Org-backups/Phone-Backup-1"
phoneBackup2="$HOME/Documents/Org-backups/Phone-Backup-2"
phoneBackup3="$HOME/Documents/Org-backups/Phone-Backup-3"
phoneBackup4="$HOME/Documents/Org-backups/Phone-Backup-4"
phoneBackup5="$HOME/Documents/Org-backups/Phone-Backup-5"
phoneTmpDir="$HOME/Documents/Org-backups/phoneTmpDir"

get_status() {
    isConnected=$(jmtpfs --listDevices | grep -c 'Google Inc Nexus/Pixel')
    isMounted=$(ls $HOME/mount | grep -c 'Internal shared storage')
    isBroken=$(ls $HOME/mount >/dev/null 2>&1 ; [ "$?" -ge 1 ] && echo "1" || echo "0" )
}

rsync_phone_to_backup() {
    [ -d "$phoneBackup5" ] && trash "$phoneBackup5"
    [ -d "$phoneBackup4" ] && mv "$phoneBackup4" "$phoneBackup5"
    [ -d "$phoneBackup3" ] && mv "$phoneBackup3" "$phoneBackup4"
    [ -d "$phoneBackup2" ] && mv "$phoneBackup2" "$phoneBackup3"
    [ -d "$phoneBackup"  ] && mv "$phoneBackup"  "$phoneBackup2"
    mkdir -p "$phoneBackup"
    echo
    echo "----------------------------------------------------------------"
    rsync $defs --dry-run "$phone/" "$phoneBackup"
    echo
    echo "> Backing up phone files to local storage..."
    echo "> $phone/ -> $phoneBackup"
    echo "Continue?"
    prompt
    [ "$in" = "n" ] && exit 0
    rsync $defs "$phone/" "$phoneBackup"
    ## copy all to $phoneTmpBackup, so that the $phoneBackup is untouched
    ## by unison and a perfect copy can be preserved.
    echo "copying to phoneTmpDir..."
    mkdir -p $phoneTmpDir
    rsync $defs "$phoneBackup/" "$phoneTmpDir"
}

rsync_laptop_to_backup() {
    [ -d "$laptopBackup5" ] && trash "$laptopBackup5"
    [ -d "$laptopBackup4" ] && mv "$laptopBackup4" "$laptopBackup5"
    [ -d "$laptopBackup3" ] && mv "$laptopBackup3" "$laptopBackup4"
    [ -d "$laptopBackup2" ] && mv "$laptopBackup2" "$laptopBackup3"
    [ -d "$laptopBackup"  ] && mv "$laptopBackup"  "$laptopBackup2"
    mkdir -p "$laptopBackup"
    echo
    echo "----------------------------------------------------------------"
    rsync $defs --dry-run "$laptop/" "$laptopBackup"
    echo 
    echo "> Backing up laptop files..."
    echo "> $laptop/ -> $laptopBackup"
    echo "Continue?"
    prompt
    [ "$in" = "n" ] && exit 0
    rsync $defs "$laptop/" "$laptopBackup"
}

unison_sync() {
    [ "$(ls -A $phoneTmpDir)" ] || die "phoneTmpDir is empty"
    echo
    echo "----------------------------------------------------------------"
    echo 
    echo "> Performing unison sync"
    echo
    unison "$phoneTmpDir" "$laptop" || exit 1
    # $phoneTmpDir is like a replacement for $phoneDir since unison can't directly write.
    # note that $laptopBackup is never copied to $laptop (that's why we use $laptop here).
    # also we already backed everything up once, so no need to do it again.
    sleep 1
}

rsync_tmpdir_to_phone() {
    echo
    echo "----------------------------------------------------------------"
    rsync $defs --dry-run --delete \
	  --omit-dir-times --no-perms --inplace \
	  "$phoneTmpDir/" "$phone"
    # If inplace disabled, mtp may want to copy the file a new, and then rename it to the old one.
    # omit-dir-times necessary for mtp
    # no-perms       necessary for mtp
    echo 
    echo "> Save phoneTmpDir to phone."
    echo "> $phoneTmpDir/ -> $phone"
    echo "Continue?"
    prompt
    [ "$in" = "n" ] && exit 0
    rsync $defs --delete \
	  --omit-dir-times --no-perms --inplace \
	  "$phoneTmpDir/" "$phone"
}

rsync_laptop_to_phone() {
    echo
    echo "----------------------------------------------------------------"
    rsync $defs --dry-run --delete \
	  --omit-dir-times --no-perms --inplace \
	  "$laptop/" "$phone"
    # If inplace disabled, mtp may want to copy the file a new, and then rename it to the old one.
    # omit-dir-times necessary for mtp
    # no-perms       necessary for mtp
    echo 
    echo "> Save current laptop to phone?"
    echo "> $laptop/ -> $phone"
    echo "Continue?"
    prompt
    [ "$in" = "n" ] && exit 0
    rsync $defs --delete \
	  --omit-dir-times --no-perms --inplace \
	  "$laptop/" "$phone"
}

save_regular_backup() {
    local regularBackup="$HOME/Documents/Org-backups/Regular-backups/Backup-$(date +'%m-%d-%Y_%H-%M')"
    echo
    echo "> saving backup of laptop dir..."
    echo "> Continue?"
    prompt
    [ "$in" = "n" ] && exit 0
    mkdir -p $regularBackup
    rsync $defs "$laptop/" "$regularBackup"
}

intro() {
    cd ~
    clear
    get_status
    echo
    echo "Sync org files!"
    echo
    echo "--Status: ------"
    echo "isConnected=$isConnected"
    echo "isMounted=$isMounted"
    echo "isBroken=$isBroken"
    echo "________________"
    echo 
    echo "Enter option:"
    echo "m -> (Default) Do all as appropriate."
    echo "b -> backup from phone"
    echo "l -> backup laptop files"
    echo "u -> unison sync"
    echo "s -> overwrite phoneTmpDir to phone"
    echo "----------------"
    echo "c -> Send current laptop to phone."
    echo "r -> Save regular backup of laptop dir."
    echo "----------------"
    echo "t -> mount android device."
    echo "----------------"
    echo "q -> quit"
    prompt
    [ "$in" = "" ] && in="m"
    case $in in
	m)
	    rsync_phone_to_backup
	    rsync_laptop_to_backup
	    unison_sync
	    rsync_tmpdir_to_phone
	    ;;
	b)
	    rsync_phone_to_backup
	    ;;
	l)
	    rsync_laptop_to_backup
	    ;;
	u)
	    unison_sync
	    ;;
	s)
	    rsync_tmpdir_to_phone
	    ;;
	r)
	    save_regular_backup
	    ;;
	c)
	    rsync_laptop_to_phone
	    ;;
	t)
	    if [ "$(which mount-android.sh| grep -c 'which: no')" -eq 0 ]; then
		mount-android.sh
		main
	    fi
	    ;;
	q)
	    exit 0
	    ;;
	*)
	    echo "invalid input, exititng..."
	    exit 1
	    ;;
    esac
}

main() {
    intro
}

main

