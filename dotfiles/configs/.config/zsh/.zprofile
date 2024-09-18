# -*- mode: sh -*-

## .zprofile

# With Guix Home, /etc/profile will be sourced automatically.

##### Below is suggested for Guix Home #####

# Set up the system, user profile, and related variables.
if [ -f /etc/profile ]; then source /etc/profile; fi

# Set up the home environment profile.
if [ -f ~/.profile ]; then source ~/.profile; fi

# Load generic shell_profile
if [ -f ~/.shell_profile ]; then source ~/.shell_profile; fi

# zshrc will be loaded automatically

# It's only necessary if zsh is a login shell, otherwise profiles will
# be already sourced by bash (whatever that means)

##### zsh-specific profile stuff: #####
