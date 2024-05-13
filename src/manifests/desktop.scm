(define-module (src manifests desktop)
  #:use-module (gnu packages))

;; return a manifest to use by guix custom profile
(specifications->manifest
 '(
   ;; Emacs EXWM --
   "emacs-exwm"
   "emacs-desktop-environment"

   ;; Wayland --
   "cagebreak"
   "river"
   "sway"
   "swayidle"
   "swaylock"
   "waybar"
   "fuzzel"
   "mako"
   ;; "gammastep"
   "wlsunset"
   "grimshot" ;; grimshot --notify copy area
   "wev"
   "foot"
   "bemenu"
   "yambar-wayland"
   "sandbar"
   "swaybg"
   "rust-wl-clipboard-rs" ; wl-clip

   ;; Wayland utils --
   "xorg-server-xwayland"
   "xdg-desktop-portal-wlr"
   "qtwayland"
   "xlsclients" ; list xwayland clients

   ;; Stumpwm --
   ;; either:
   ;; - stumpwm:lib and sbcl in same profile
   "stumpwm:lib" ; doesn't work if system-installed...?
   ;; "sbcl-slynk"
   "stumpish"	  ; stumpwm interactive shell for stumpwm-mode
   "sbcl-clx-xembed"		; stumptray depd
   "snixembed"			; stumptray depd???
   "sbcl" ; if using guix-home, must add to here even if in root profile (for some reason)
   ;; "stumpwm" ; already defined in root profile
   "stumpwm-with-slynk"		; this package is broken 

   ;; Applications --
   "firefox"
   "qutebrowser"
   "keepassxc"
   "calibre"
   ;; "syncthing"
   "syncthing-gtk"
   "extremetuxracer"		; game
   "supertuxkart"		; game
   "kdeconnect" ; experiment in the future
   "nicotine+"
   "mpv"
   "vlc"
   "mupdf" ; keybinds: S-w + - j k h l , . m t [0-9]m [0-9]t / n N [0-9]g c S-h
   "okular"			; pdf reader
   "cmus"
   
   ;; Desktop misc --
   "network-manager-applet" ; original
   "nm-tray"
   "dunst"
   "libnotify" ; notify-send
   "rofi"
   "sct"
   "feh"
   "brightnessctl"
   "scrot"
   "flameshot"
   "picom"
   "pasystray"
   "pavucontrol"
   "playerctl"

   ;; Desktop tools --
   ;; "yad" ; custom tray icons?
   "numlockx"
   "xmodmap"
   "setxkbmap"
   "xdg-utils"
   "glib:bin"	; gsettings (put this outside the manifest)
   "xclip"

   ;; X utils --
   "xinit"
   "xev"
   "xset"
   "xsetroot"
   "xrandr"
   "arandr"
   ;; "slock" ; moved to system profile
   "xss-lock"
   "libinput"
   "xinput"			; ? test devices?
   "xprop"
   
   ;; Flatpak --
   "flatpak"
   "xdg-desktop-portal-gtk"	; for file manager
   "xdg-desktop-portal"		; broken?
   "shared-mime-info" ; what's this do?
   "xdg-dbus-proxy" ; ?

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

   ;; Themes --
   "xsettingsd"			; gtk3
   "kvantum"			; qt
   
   "matcha-theme"		; theme
   "hicolor-icon-theme" ; needed for icons
   "papirus-icon-theme"		; icon
   "breeze-icons"		; qt icons?


   ;; fixing steam:
   ;; "gstreamer"
   ;; "gst-plugins-base"
   ;; "gst-plugins-good"
   ;; "gst-plugins-bad"
   ;; "gst-plugins-ugly"
   ;; "gst-libav"
   


   ;; note: firejail is declared in the system configuration
   ))
