# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
[ -f /etc/bashrc ] && source /etc/bashrc

# vterm integration
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# helper: whether guix binary is installed
if type "guix" &> /dev/null ; then
    guix_is_installed=1
else
    guix_is_installed=0
fi

# helper: whether distro is Guix System
# if [[ $(cat /etc/*-release | grep "^ID=" | sed "s/.*=//") = "guix" ]] ||
#        [[ -z $(cat /etc/*-release) ]]; then
#     is_guix_system=1
# else
#     is_guix_system=0
# fi
is_guix_system=1
    
# Do the following when on a foreign distro with Guix:
if (($guix_is_installed == 0)) && (($is_guix_system == 0)); then
    echo "Debug: bashrc: is on a foreign distro."
    # Automatically added by the Guix install script.
    if [ -n "$GUIX_ENVIRONMENT" ]; then
	if [[ $PS1 =~ (.*)"\\$" ]]; then
	    PS1="${BASH_REMATCH[1]} [env]\\\$ "
	fi
    fi
fi

# # set $CC
# alias CC="gcc"

# Import aliases
[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

