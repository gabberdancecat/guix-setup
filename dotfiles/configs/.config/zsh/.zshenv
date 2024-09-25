# -*- mode: sh -*-

#### .zshenv
## Sourced in all shells, including ssh sessions.

##### Suggested by Guix Home #####
[ -n $SSH_CLIENT ] && source /etc/profile

##### Personal #####

# Set up the system, user profile, and related variables.
if [ -f /etc/profile ]; then source /etc/profile; fi

# Set up the home environment profile (created by Guix Home)
if [ -f $HOME/.profile ]; then source $HOME/.profile; fi

# Load generic shell_profile (sources paths to Guix profiles)
if [ -f $HOME/.shell_profile ]; then source $HOME/.shell_profile; fi
