#!/bin/sh

die() {
    echo "$*, exiting..."
    exit 1
}

refresh_checks_vars() {
    isConnected=$(jmtpfs --listDevices | grep -c 'Google Inc Nexus/Pixel')
    isMounted=$(ls $HOME/mount | grep -c 'Internal shared storage')
    isBroken=$(ls $HOME/mount >/dev/null 2>&1 ; [ "$?" -ge 1 ] && echo "1" || echo "0" )
}

init() {
    cd $HOME
    clear
    refresh_checks_vars
    echo
    echo "rsync org files between linux and android"
    echo
    echo "--Status: ------"
    echo "isConnected=$isConnected"
    echo "isMounted=$isMounted"
    echo "isBroken=$isBroken"
    echo "________________"
    echo 
    echo "Enter option:"
    echo "m -> mount"
    echo "u -> unmount"
    echo -n "> "
    read input
    case $input in
	m)
	    mount_checks
	    mount
	    ;;
	u)
	    unmount_checks
	    unmount
	    ;;
	*)
	    echo "invalid input, exiting..."
	    exit 1
	    ;;
    esac
}

mount_checks() {
    refresh_checks_vars
    
    if [ "$isBroken" = 1 ]; then
	echo "mount point broken, fixing..."
	fusermount -u $HOME/mount
	ls -l ~ | grep "mount"
	"exiting..."
	sleep 1
	exit 0
    fi

    if [ "$isMounted" = 1 ]; then
	echo "already mounted, exiting..."
	sleep 1
	exit 0
    fi
}

unmount_checks() {
    refresh_checks_vars
    if [ "$isMounted" = 0 ]; then
	if [ "$isBroken" = 0 ]; then
	    echo "is already unmounted, exiting..."
	    sleep 1
	    exit 0
	else
	    echo "disconnected, but broken."
	fi
    fi
}

mount() {
    echo 
    echo "Setup: "
    echo "- Android: Sync org files in Orgzly." 
    echo "           Change USB mode to File Transfer."
    echo
    echo -ne "Enter to continue...\n> "
    read input
    echo
    echo "mounting..."
    jmtpfs $HOME/mount -o auto_unmount
    sleep 1
}

unmount() {
    echo "unmounting..."
    fusermount -u $HOME/mount || die "failed to unmount with fuse"
    ls -l ~ | grep "mount"
    if [ "$(ls -l $HOME | grep -c '?')" = 0 ]; then
	echo "success!"
    else
	echo "failed, exiting..."
    fi
    sleep 1
}

main() {
    init
}

main
