#!/bin/sh
# This script is configured in Desktop.org

# Start Emacs w/ EXWM
# exec dbus-launch --sh-syntax --exit-with-session emacs -q --load "~/.emacs.d/init.el" -mm --debug-init -l ~/.emacs.d/desktop.el
exec dbus-launch --sh-syntax --exit-with-session emacs -q --load "~/.emacs.d/init.el" -mm --debug-init --start-exwm
