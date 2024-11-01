# -*- mode: sh -*-

## .zshrc

# do i need this?

##### Source: ######

# Personal aliases
if [ -f ~/.aliasrc ]; then source ~/.aliasrc; fi

# temporary workaround
# if [ -f "$ZDOTDIR/.zprofile" ]; then
#     source "$ZDOTDIR/.zprofile"
# fi


##### Customize: ######

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

## History

HISTFILE=~/.config/zsh/.histfile
HISTSIZE=1000
SAVEHIST=1000

## Keybinds

bindkey -e

## Completion

autoload -Uz compinit
compinit

# Compinstall configuration path (generated)
zstyle :compinstall filename '/home/nya/.config/zsh/.zshrc'

## Prompt

autoload -Uz promptinit
promptinit
prompt oliver

# # Source anything in .zshrc.d.
# for _rc in ${ZDOTDIR:-$HOME}/.zshrc.d/*.zsh; do
#   # Ignore tilde files.
#   if [[ $_rc:t != '~'* ]]; then
#     source "$_rc"
#   fi
# done
# unset _rc

# Automatically start a tmux session on new interactive shell
if [[ $- == *i* ]]; then
    if [ -z "$TMUX" ]; then
	tmux new-session -A -s default
	exit
    fi
fi
