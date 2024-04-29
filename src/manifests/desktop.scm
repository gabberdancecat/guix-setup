(define-module (src manifests desktop)
  #:use-module (gnu packages))

;; return a manifest to use by guix custom profile
(specifications->manifest
 '(;; Emacs EXWM --
   "emacs-exwm"
   "emacs-desktop-environment"

   ;; Wayland --
   ;; "cagebreak"
   "cagebreak-xkb-fix"
   "river"
   "sway"
   "swayidle"
   "swaylock"
   "waybar"
   "fuzzel"
   "mako"
   "gammastep"
   "grimshot" ;; grimshot --notify copy area
   "feh"
   "wev"
   "foot"
   "bemenu"
   "yambar-wayland"

   ;; Fonts --
   "font-iosevka-aile"

   "font-fira-code"
   "font-hack"
   "font-jetbrains-mono"
   
   "font-liberation"
   "font-dejavu"
   "font-libertinus"		; provides latex? try for org?
   "font-ipa-ex"			; japanese? not working...
   "font-microsoft-times-new-roman"
   "font-microsoft-cascadia"
   
   "font-awesome"

   "fontconfig"			; fonts.conf

   ;; Dev --
   "rust-cargo"	      ; ?
   "rust"	      ; ?
   "rust-analyzer"
   "rust-clippy"
   ;; "clang-toolchain" ; ? is gcc better ?
   "gcc-toolchain"
   "xorg-server"			; Xephyr
   "valgrind"
   "clang" ; needed for company backend emacs

   "gprolog"
   
   "glibc"		 ; adding here bc also in root profile

   ;; Video/Music --
   "mpv"
   "yt-dlp"
   "vlc"
   "nicotine+"
   "mpdscribble"

   ;; Misc media --
   "calibre"

   ;; Cli fun --
   "hyfetch"
   "uwufetch"
   "sl"
   "cowsay"
   "btop"

   ;; App fun --
   "extremetuxracer"		; game
   "supertuxkart"		; game

   ;; Audio --
   "pipewire"
   "pulseaudio" ; do i need this? (probably) (add to system list of packages instead?)

   ;; Web --
   "qutebrowser"
   "firefox"

   ;; Stumpwm --
   ;; either:
   ;; - stumpwm:lib and sbcl in same profile
   "stumpwm:lib" ; doesn't work if system-installed...? (only when loading modules? wtf?) ; enabling for testing..
   ;; "sbcl-slynk"     ; dont need if using stumpwm-with-slynk, but can instead use this plus stumpwm
   "stumpish"	  ; stumpwm interactive shell for stumpwm-mode
   "sbcl-clx-xembed"		; stumptray depd
   "snixembed"			; stumptray depd???
   "sbcl" ; if using guix-home, must add to here even if in root profile (for some reason)
   ;; "firejail" ; make it so it can find firefox?
   ;; "stumpwm" ; already defined in root profile
   "stumpwm-with-slynk"		; this package is broken 

   ;; Wayland --
   "xorg-server-xwayland"
   "xdg-desktop-portal-wlr"
   "qtwayland"

   "xlsclients" ; list xwayland clients

   ;; Theme --
   "xsettingsd"			; gtk3
   "kvantum"			; qt
   
   "matcha-theme"		; theme
   "hicolor-icon-theme" ; needed for icons
   "papirus-icon-theme"		; icon
   "breeze-icons"		; qt icons?

   ;; Regular Applications --
   "kdeconnect" ; experiment in the future
   "syncthing"
   "syncthing-gtk"		; necessary??
   "keepassxc"

   ;; Flatpak --
   "flatpak"
   "xdg-desktop-portal-gtk"	; for file manager
   "xdg-desktop-portal"		; broken?
   "shared-mime-info" ; what's this do?
   "xdg-dbus-proxy" ; ?
   
   
   ;; Desktop programs --
   "network-manager-applet" ; original
   "nm-tray"
   "dunst"
   "libnotify"			; notify-send
   
   ;; Cli Net --
   "iptables"
   "gdb"
   "net-tools"
   "glances"
   "openssh" ; do i need this?

   ;; Cli Utils --
   "tmux"
   "brightnessctl"
   "xclip"
   "feh"
   "sct"
   "xdg-utils"
   "scrot"
   "flameshot"
   "stow"
   "picom"
   "htop"
   "playerctl"
   
   "glib:bin"	; gsettings (put this outside the manifest)

   ;; Cli programs -- (?)
   "pasystray"
   "pavucontrol"
   "mupdf" ; keybinds: S-w + - j k h l , . m t [0-9]m [0-9]t / n N [0-9]g c S-h
   "okular"			; pdf reader
   "yad"				; custom tray icons?
   "alacritty"
   "numlockx"
   "acpi"
   "fzf"
   "bat"
   "xprop"
   "espeak-ng"

   ;; Cli programs -- (?)
   "trash-cli"
   "unzip"
   "zip"
   "gtk+:bin"			; gtk-launch
   "tree"
   "rsync"
   "unison"
   "fd"
   "ffmpeg"
   "p7zip"
   "xdot"
   "strace"
   "file"

   ;; X utils --
   "xinit"
   "xev"
   "xset"
   "xmodmap"
   "setxkbmap"
   "xsetroot"
   "xrandr"
   "arandr"
   ;; "slock" ; moved to system profile
   "xss-lock"
   "libinput"
   "xinput"			; ? test devices?

   ;; Security --
   "pinentry-emacs"
   "pinentry-rofi" "rofi"
   "pinentry-bemenu"

   ;; 


   ;; fixing steam:
   ;; "gstreamer"
   ;; "gst-plugins-base"
   ;; "gst-plugins-good"
   ;; "gst-plugins-bad"
   ;; "gst-plugins-ugly"
   ;; "gst-libav"
   

   "dbus" ; do i need this? (command to search for which profiles this is in?)



   ;; note: firejail is declared in the system configuration
   ))
