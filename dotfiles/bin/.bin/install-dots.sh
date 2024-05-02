#!/bin/bash

# This is a script that installs necessary symlinks and such.
# For a permanent installation, use "stow ." on the dotfiles root directory.

get-repo-path() {
    
    # get path to script, then go back two directories to get dotfiles repo path.
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
    cd $SCRIPT_DIR
    cd ../../
    REPO_PATH=$(pwd)
    echo "REPO_PATH: $REPO_PATH"
}

create-links() {
    
    # List of dirs to link:
    #  .bashrc
    #  .zshrc
    #  .aliasrc
    #  .bash_profile
    #  .zprofile
    #  .xinitrc
    #  .emacs.d
    #  .config/guix/manifests/
    #  .config/guix/base-channels.scm
    #  .config/guix/channels.scm
    #  .config/guix/config.scm
    
    symlink-if-not-already ".bashrc"
    symlink-if-not-already ".zshrc"
    symlink-if-not-already ".aliasrc"
    symlink-if-not-already ".bash_profile"
    symlink-if-not-already ".zprofile"
    symlink-if-not-already ".xinitrc"
    symlink-if-not-already ".emacs.d"
    symlink-if-not-already ".config/guix/manifests"
    symlink-if-not-already ".config/guix/systems"
    symlink-if-not-already ".config/guix/base-channels.scm"
    symlink-if-not-already ".config/guix/channels.scm"
    symlink-if-not-already ".config/guix/config.scm"
    symlink-if-not-already ".bin"
}

symlink-if-not-already() {
    local file=$1
    local source=$REPO_PATH/$file
    local dest=~/$file

    if [ ! -e "$source" ]; then
	echo "ERROR: source file cannot be located!"
	echo "File name: $file"
	echo "Source file path: $source"
	echo "Destination path: $dest"
	exit 1
    fi
    
    if [ -L "$dest" ]; then
	if [ -e "$dest" ]; then
	    echo "The symlink for $file already exists, skipping..."
	else
	    echo "ERROR: a broken symlink exists at $dest."
	    echo "Please remove it."
	    echo "exiting..."
	    exit 1
	fi
    elif [[ -f "$dest" ]] || [[ -d "$dest" ]]; then
	echo "ERROR: there already exists a file or directory on $dest."
	echo "Please remove the conflicting file first."
	echo "exiting..."
	exit 1
    else
	echo "Creating symlink for $file"
	ln -s $source $dest
    fi
}

main() {
    get-repo-path
    create-links
}

main

