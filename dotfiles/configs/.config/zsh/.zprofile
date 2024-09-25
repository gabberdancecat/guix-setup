# -*- mode: sh -*-

## .zprofile

# With Guix Home, /etc/profile will be sourced automatically.


# MOST IMPORTANT: source .zshrc
if [ -f $ZDOTDIR/.zshrc ]; then source $ZDOTDIR/.zshrc; fi

# zshrc will be loaded automatically

# It's only necessary if zsh is a login shell, otherwise profiles will
# be already sourced by bash (whatever that means)

##### zsh-specific profile stuff: #####
