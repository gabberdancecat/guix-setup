#!/bin/sh

die() { echo -e "\nError: $@. exiting..." ; exit 1 ; }

cmd_checks=(
    "guix gc --verify=contents"
)


cmd_backups=(
    "snapper backup"
)

cmd_updates=(
    "guix-system-reconfigure 3"
    "guix-update-channels"
    "guix-update-profiles all"
    "guix upgrade"
    "nix-update-packages y"
    "flatpak-update"
    "arkenfox-updater.sh"
)

# prompt for input
# enter range like 1-6, or 1 2 3 ?
