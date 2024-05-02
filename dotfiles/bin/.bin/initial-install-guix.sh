#!/bin/sh
# This file is edited in System.org

## After wifi, loadkeys, and partitioning, git clone and run the script!

ROOT="nvme0n1p2"
SWAP="sda1"

main() {
    while
        LOOP=0
        echo "_____________________________________________________________________"
        echo ""
        echo "Did you partition your drives first? Is 'dotfiles1' in the home dir?"
        echo "mkfs.btrfs and mkswap must be done first if not already done so."
        echo ""
        echo "Options:"
        echo "0.) ALREADY CREATED RECOVERY mount (cryptroot, $ROOT), swapon ($SWAP)."
        echo "1.) herd start cow-store."
        echo "2.) nonguix channel, guix pull, prompt to run GUIX_PROFILE."
        echo "3.) install neovim (and other essentials), then prompt to run GUIX_PROFILE."
        echo "4.) copy config.scm to /etc/guix, then get and add signing key."
        echo "5.) edit config.scm with nvim."
        echo "6.) proceed with system install (regular) (prompts text if not working)."
        echo "7.) change default root ($ROOT) and swap ($SWAP) partitions."
        echo ""
        echo -n "> "
        read in
        case $in in
            0)
                cryptsetup open /dev/$ROOT my-partition
                mount LABEL=my-root /mnt
                # swapon /dev/$SWAP
                ;;
            1)
                herd start cow-store /mnt
                ;;
            2)
                mkdir -p ~/.config/guix
                cp ~/dotfiles1/.config/guix/base-channels.scm ~/.config/guix/channels.scm
                guix pull --channels="$HOME/.config/guix/base-channels.scm"
                echo -e "\n> Run the following manually, by hand:"
                echo "GUIX_PROFILE=\"/root/.config/guix/current\""
                echo ". \"$GUIX_PROFILE/etc/profile\""
                echo "hash guix"
                ;;
            3)
                guix install neovim
                echo -e "\n> Run the following manually, by hand:"
                echo "GUIX_PROFILE=\"/root/.guix-profile\""
                echo ". \"$GUIX_PROFILE/etc/profile\""
                echo "hash guix"
                ;;
            4)
                cp ~/dotfiles1/.config/guix/gnuwu.scm /etc/guix/
                curl https://substitutes.nonguix.org/signing-key.pub -o signing-key.pub
                guix archive --authorize < signing-key.pub
                ;;
            5)
                nvim /etc/guix/gnuwu.scm
                ;;
            6)
                guix system init /etc/guix/gnuwu.scm /mnt --substitute-urls="https://ci.guix.gnu.org https://bordeaux.guix.gnu.org https://substitutes.nonguix.org"
                ;;
            7)
                echo -en "Enter new value for ROOT partition '/dev/VALUE' (old: '$ROOT'):\n> "
                read in
                ROOT="$in"
                echo -en "Enter new value for SWAP partition '/dev/VALUE' (old: '$SWAP'):\n> "
                read in
                SWAP="$in"
                clear
                # loop one more time
                LOOP=1
                ;;
            *)
                echo "Failed to find match, exiting..."
                ;;
        esac

        (( $LOOP == 1 ))
    do true; done
}

main
