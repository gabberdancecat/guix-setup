# Custom profile for renoise :3
# Main config file
#
# to check in vm: ~/.config/renoise? ~/.renoise ~/.vst ~/.lv2 ~/.local/renoise
#                 ~/.cache/renoise /tmp/renoise
# Links:
# https://wiki.archlinux.org/title/Firejail
# 
# hallo!!! im Emu Otori!! Emu is meaning, Smilee!!! :DDD
# - Madeline :3

# Persistent global definitions
include globals.local

#noblacklist ${HOME}/.config/renoise????? # use virtual machine and free version to test?
noblacklist ${HOME}/.lv2  # undo for disable-programs.inc
noblacklist ${HOME}/.vst  # undo for disable-programs.inc
noblacklist ${MUSIC}  # undo for disable-xdg.inc

include disable-common.inc  # (!) general program paths, like ~/.local/...
include disable-devel.inc  # dev tools, compilers and shi
include disable-exec.inc  # noexec in home, /tmp, /var
include disable-interpreters.inc  # interpreters and shi
include disable-programs.inc  # (!) home hidden dirs (lv2, vst)
include disable-xdg.inc  # block Documents, Music, Pictures, Videos

caps.drop all  # drop all capabilites for processing running in sandbox
ipc-namespace  # new IPC namespace
## net none  # deny network access (!)
nodvd  # disable dvd and audio cd devices (!)
nogroups  #  disable supplementary groups(?)
noinput  # disable input devices
nonewprivs  # child processes cannot acquire new privs w/ execve
noroot  # new user namespace w/ only current user, w/ no root user (!)
notv  # disable digital video broadcasting TV devices
nou2f  # disable U2F devices
protocol unix  # (!) allowed protocols, using seccomp
seccomp  # (!) enable seccomp filter(?)

private-cache  # (!) new temp .cache in temp filesystem (!) (check if there)
private-dev  # new temp /dev
## private-etc  # new tmp /etc
private-tmp  # new temp /tmp (!) (check if there's anything there. if not, private)

dbus-user none
dbus-system none

restrict-namespaces  # prevent creating new namespaces

hosts-file /run/current-system/etc/hosts
