#!/bin/sh

scriptpath=$(realpath "$0")
scriptdir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dotfilesdir=$(dirname "$scriptdir")

echo "> ScriptPath: $scriptpath"
echo "> ScriptDir: $scriptdir"
echo "> DotfilesDir: $dotfilesdir"

cd "$dotfilesdir"

echo "> Modules:"
ls

echo -e "> Stow pre:"
stow -nv * -t $HOME

echo -e "\n> Proceed? (enter to continue, C-c to exit)"
read stdin

stow * -t $HOME

echo -e "\n> Done!"
