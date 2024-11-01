# -*- mode: sh -*-

## .zprofile

# With Guix Home, /etc/profile will be sourced automatically.

##### Suggested by Guix Home #####
[ -n $SSH_CLIENT ] && source /etc/profile

##### Personal #####

# Set up the system, user profile, and related variables.
if [ -f /etc/profile ]; then source /etc/profile; fi

# Set up the home environment profile (created by Guix Home)
if [ -f $HOME/.profile ]; then source $HOME/.profile; fi

# Load generic shell_profile (sources paths to Guix profiles)
if [ -f $HOME/.shell_profile ]; then source $HOME/.shell_profile; fi

# MOST IMPORTANT: source .zshrc
if [ -f $ZDOTDIR/.zshrc ]; then source $ZDOTDIR/.zshrc; fi

# zshrc will be loaded automatically

# It's only necessary if zsh is a login shell, otherwise profiles will
# be already sourced by bash (whatever that means)

##### zsh-specific profile stuff: #####
