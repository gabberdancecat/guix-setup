
# This is .profile where things are sourced

# Merge search-paths from multiple profiles, the order matters.
eval "$(guix package --search-paths \
-p $HOME/.config/guix/current \
-p $HOME/.guix-profile \
-p /run/current-system/profile \
-p $HOME/.guix-extra-profiles/emacs \
-p $HOME/.guix-extra-profiles/desktop \
-p $HOME/.guix-extra-profiles/workstation \
)" 

# Prepend setuid programs.
export PATH=/run/setuid-programs:$PATH

# Load the default Guix profile
GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

# Necessary??
GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"

# Load additional Guix profiles
GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
for i in $GUIX_EXTRA_PROFILES/*; do
    profile=$i/$(basename "$i")
    if [ -f "$profile"/etc/profile ]; then
	GUIX_PROFILE="$profile"
	. "$GUIX_PROFILE"/etc/profile
    fi
    unset profile
done

# Load Nix environment
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
    . /run/current-system/profile/etc/profile.d/nix.sh
fi

# Load Nix my-packages profile environment
if [ -d "${HOME}/.nix-profile/etc/profile.d" ]; then
    for i in "${HOME}/.nix-profile/etc/profile.d/"*.sh; do
	if [ -r "$i" ]; then
	    . "$i"
	fi
    done
fi

# Make Flatpak apps visible to launcher
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"

# fix gcc compilation
export CC="gcc"

# don't use system PulseAudio configuration
unset PULSE_CONFIG
unset PULSE_CLIENTCONFIG

# autostart mullvad-daemon if not done so already
# if [ "$(which mullvad-daemon ; echo $?)" -eq 0 ]; then # if mullvad-daemon exists
#     if [ "$(pgrep -l mullvad-daemon | grep -c 'mullvad-daemon')" -eq 0 ]; then # if process doesn't exist
# 	sudo mullvad-daemon &
#     fi
# fi


# Load .bashrc to get login environment
# [ -f ~/.bashrc ] && . ~/.bashrc    
