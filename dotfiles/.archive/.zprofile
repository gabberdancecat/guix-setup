
# This file is sourced by zsh for login shells.

if [ $(cat /etc/*-release | grep "^ID=" | sed "s/.*=//") == "guix" ]; then
    # Honor system-wide environment variables
    source /etc/profile
fi

# Honor per-interactive-shell startup file
if [ -f ~/.zshrc ]; then . ~/.zshrc; fi

if type "guix" &> /dev/null ; then

    # Set up Guix Home profile
    if [ -f ~/.profile ]; then . ~/.profile; fi
    
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

    export CC="gcc"
fi

echo "ZSHHHH SOURCE PERFORMED!!!"
export RI_ZSH_SOURCE="YES"
