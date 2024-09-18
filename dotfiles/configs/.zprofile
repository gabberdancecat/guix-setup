# -*- mode: sh -*-

# With Guix Home, /etc/profile will be sourced automatically.

##### Below is suggested for Guix Home #####

# Set up the home environment profile.
if [ -f ~/.profile ]; then source ~/.profile; fi

# Load generic shell_profile
if [ -f ~/.shell_profile ]; then source ~/.shell_profile; fi

# Honor per-interactive-shell startup file (includes aliasrc)
if [ -f ~/.zshrc ]; then source ~/.zshrc; fi

##### zsh-specific profile stuff: #####
