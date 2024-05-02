
# This file is sourced by bash for login shells.

# is guix system?
if [ $(cat /etc/*-release | grep "^ID=" | sed "s/.*=//") == "guix" ]; then
    # Honor system-wide environment variables
    source /etc/profile
else
    # Honor per-interactive-shell startup file
    if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
fi

# guix installed?
if type "guix" &> /dev/null ; then
    # Set up Guix Home profile (guix-specific sourcing)
    if [ -f ~/.profile ]; then . ~/.profile; fi
fi

echo "Debug: .bash_profile ran"
export RI_BASH_SOURCE="YES"

