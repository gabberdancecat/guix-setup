#!/bin/sh
# This script is configured in Desktop.org

Run() {
    if $(which $1 >/dev/null 2>&1 ; echo $?); then
        $@ &
    else
        echo "Command not found: $@" >> ~/.my-wm-errors.log
    fi
}

if [ "$PREPARE_EXWM_SCRIPT_RAN" -eq "1" ]; then
    echo "DEBUG: ran prepare-exwm.sh twice!"
    exit 0
fi

# Set screen DPI (for high DPI displays) (before compton)
#xrdb ~/.emacs.d/exwm/Xresources

# set up dvorak and nocaps (symbols: compose:sclk)
# (only run if not in guix system, because otherwise will be configured).
if ! [ $(cat /etc/*-release | grep "^ID=" | sed "s/.*=//") == "guix" ]; then
    setxkbmap -layout 'us,us' -variant 'dvp,' -option grp:sclk_toggle ctrl:nocaps
fi

# binds S-<Ctrl> to escape (might break functionality!) and remove caps (not needed).
#xmodmap ~/.emacs*/exwm/Xmodmap

# make menu super
xmodmap -e "keysym Menu = Super_R"

# if laptop is thinkpad, load special keybinds
if grep -q "ThinkPad L520" "/sys/devices/virtual/dmi/id/product_family"; then
    xmodmap ~/.emacs*/exwm/Xmodmap-thinkpad
fi

# set hold type startup and speed (delay rate)
# xset r rate 240 60
xset r rate 220 70

# set cursor type (what if not enabled?)
xsetroot -cursor_name left_ptr

# Screen compositor
picom &

# set screen temperature
[ -f "$HOME/.bin/sct-auto-adjust.sh" ] && ~/.bin/sct-auto-adjust.sh &

# Enable screen locking on suspend
# xss-lock -- slock &

# run xsettingsd
xsettingsd &

# dunst notifications
dunst &

# nm-applet
nm-applet &

# volume systray applet
pasystray &

# numlock on
numlockx &
