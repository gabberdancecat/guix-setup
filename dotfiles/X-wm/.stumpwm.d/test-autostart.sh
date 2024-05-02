#!/bin/sh

# [ -f "autostart-generic" ] && autostart-generic # guix-home

xmodmap -e "keysym Menu = Super_R" &

xset r rate 220 70 &

xsetroot -cursor_name left_ptr & # changes "X" symbol to pointer

# ~/.fehbg # blank color is better

# 
picom &
~/.fehbg
~/.bin/sct-auto-adjust.sh &
xsettingsd &
#dunst &
nm-applet &
pasystray &
numlockx &



# emacs --daemon
