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

# Lines configured by zsh-newuser-install
HISTFILE=~/.config/zsh/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/nya/.config/zsh/.zshrc'

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit
prompt oliver

# End of lines added by compinstall
