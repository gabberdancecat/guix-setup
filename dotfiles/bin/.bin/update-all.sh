#!/bin/sh

commands=(
    "guix-update-channels"
    "guix-system-reconfigure 3"
    "guix-update-profiles all"
    "flatpak update"
    "flatpak-update"
    "nix-update-packages y"
)

echo -e "\nCommands to run:"
for c in "${commands[@]}"; do
    echo "  $c"
done

for c in "${commands[@]}"; do
    echo -e "\nContinue? (any key to proceed)"
    echo -n "> "
    read in
    $c
done
