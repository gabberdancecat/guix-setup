(define-module (src home desktop)
  #:use-module (src home misc display)
  #:use-module (gnu packages)
  #:use-module (gnu services) ; service-type
  #:use-module (gnu home services) ; service-extension
  #:use-module (gnu packages glib)) ; glib

(define-public desktop-packages
  '(;; Emacs EXWM
    "emacs-exwm"
    "emacs-desktop-environment"
    
    ;; Wayland
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

    "xorg-server-xwayland"
    "xdg-desktop-portal-wlr"
    "qtwayland"

    "font-iosevka-aile"
    "xlsclients" ; list xwayland clients
    
    "network-manager-applet"
    "hicolor-icon-theme" ; needed for icons

    ;; either:
    ;; - stumpwm:lib and sbcl in same profile
    "stumpwm:lib" ; doesn't work if system-installed...? (only when loading modules? wtf?) ; enabling for testing...
    ;; "sbcl-slynk"     ; dont need if using stumpwm-with-slynk, but can instead use this plus stumpwm
    "stumpish"	  ; stumpwm interactive shell for stumpwm-mode
    "sbcl-clx-xembed"		; stumptray depd
    "snixembed"			; stumptray depd???
    "sbcl" ; if using guix-home, must add to here even if in root profile (for some reason)
    ;; "firejail" ; make it so it can find firefox?
    ;; "stumpwm" ; already defined in root profile
    "stumpwm-with-slynk"		; this package is broken 
    
    ;; configure these with xsettingsd?
    "xsettingsd"			; gtk3
    "kvantum"			; qt
    
    "matcha-theme"		; theme
    "papirus-icon-theme"		; icon
    "breeze-icons"		; qt icons?
    
    "fontconfig"			; fonts.conf
    
    ;; fonts
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
    
    "hyfetch"
    "uwufetch"
    "sl"
    "cowsay"
    ;; experiment in the future
    "pipewire"
    "kdeconnect"
    "extremetuxracer"		; game
    "supertuxkart"		; game

    ;; fixing steam:
    ;; "gstreamer"
    ;; "gst-plugins-base"
    ;; "gst-plugins-good"
    ;; "gst-plugins-bad"
    ;; "gst-plugins-ugly"
    ;; "gst-libav"
    
    ;; "openssh" ; do i need this?
    "iptables"
    "gdb"
    "net-tools"
    
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
    
    ;; "nm-tray" ; run nm-applet, not tray!
    ;; "network-manager"	       ; will this get things to work?
    
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
    
    "dbus"
    ;; not automatically pulled by system service (maybe add to system packages instead?)
    "pulseaudio"
    "rust-cargo"	      ; ?
    "rust"	      ; ?
    "rust-analyzer"
    "rust-clippy"
    ;; "clang-toolchain" ; ? is gcc better ?
    "gcc-toolchain"
    "xorg-server"			; Xephyr
    "valgrind"
    
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
    
    "mpv"
    "yt-dlp"
    "vlc"
    
    "qutebrowser"
    "firefox-esr"
    
    "syncthing"
    "syncthing-gtk"		; necessary??
    
    "dunst"
    "libnotify"			; notify-send
    
    "flatpak"
    "xdg-desktop-portal"		; ?
    "xdg-desktop-portal-gtk"	; for file manager
    "shared-mime-info" ; what's this do?
    "xdg-dbus-proxy" ; ?
    
    "keepassxc"

    "pinentry-emacs"
    "pinentry-rofi" "rofi"
    "pinentry-bemenu"

    "openssh"

    "glibc"		 ; adding here bc also in root profile
    ;; note: firejail is declared in the system configuration
    ))


;; (define (home-desktop-profile-service config)
;;   (cons*
;;    ;; (list
;;    ;; NOTE: This is a very weird hack to get around an issue where "glib:bin"
;;    ;; returns a newer version of glib than what most packages are using via the
;;    ;; exported `glib' symbol.  The "bin" output is needed to get the `gsettings'
;;    ;; program to control GTK theme settings without a configuration file.
;;    (list glib "bin")			; same as "glib:bin"
;;    (map specification->package+output
;; 	'( ;; Emacs EXWM
;; 	  "emacs-exwm"
;; 	  "emacs-desktop-environment"
	  
;; 	  ;; "stumpwm:lib" ; doesn't work if system-installed...? (only when loading modules? wtf?)
;; 	  ;; "sbcl-slynk"     ; dont need if using stumpwm-with-slynk, but can instead use this plus stumpwm
;; 	  "stumpish"	  ; stumpwm interactive shell for stumpwm-mode
;; 	  "sbcl-clx-xembed"		; stumptray depd
;; 	  "snixembed"			; stumptray depd???
;; 	  "sbcl" ; if using guix-home, must add to here even if in root profile (for some reason)
;; 	  ;; "firejail" ; make it so it can find firefox?
;; 	  ;; "stumpwm" ; already defined in root profile
;; 	  "stumpwm-with-slynk"		; this package is broken
	  
;; 	  ;; configure these with xsettingsd?
;; 	  "xsettingsd"			; gtk3
;; 	  "kvantum"			; qt
	  
;; 	  "matcha-theme"		; theme
;; 	  "papirus-icon-theme"		; icon
;; 	  "breeze-icons"		; qt icons?
	  
;; 	  "fontconfig"			; fonts.conf
	  
;; 	  ;; fonts
;; 	  "font-fira-code"
;; 	  "font-hack"
;; 	  "font-jetbrains-mono"
	  
;; 	  "font-liberation"
;; 	  "font-dejavu"
;; 	  "font-libertinus"		; provides latex? try for org?
;; 	  "font-ipa-ex"			; japanese? not working...
;; 	  "font-microsoft-times-new-roman"
;; 	  "font-microsoft-cascadia"
	  
;; 	  "font-awesome"
	  
;; 	  "hyfetch"
;; 	  "uwufetch"
;; 	  "sl"
;; 	  "cowsay"
;; 	  ;; experiment in the future
;; 	  "pipewire"
;; 	  "kdeconnect"
;; 	  "extremetuxracer"		; game
;; 	  "supertuxkart"		; game
	  
;; 	  ;; "openssh" ; do i need this?
;; 	  "iptables"
;; 	  "gdb"
;; 	  "net-tools"
	  
;; 	  "brightnessctl"
;; 	  "xclip"
;; 	  "feh"
;; 	  "sct"
;; 	  "xdg-utils"
;; 	  "scrot"
;; 	  "flameshot"
;; 	  "stow"
;; 	  "picom"
;; 	  "htop"
;; 	  "playerctl"
;; 	  ;; "glib:bin"	; gsettings (put this outside the manifest)
;; 	  "nm-tray" ; maybe the reason why the icon's missing is bc NetworkManager is not in profile?
;; 	  "network-manager"	       ; will this get things to work?
	  
;; 	  "pasystray"
;; 	  "pavucontrol"
;; 	  "mupdf" ; keybinds: S-w + - j k h l , . m t [0-9]m [0-9]t / n N [0-9]g c S-h
;; 	  "okular"			; pdf reader
;; 	  "yad"				; custom tray icons?
;; 	  "alacritty"
;; 	  "numlockx"
;; 	  "acpi"
;; 	  "fzf"
;; 	  "bat"
;; 	  "xprop"
;; 	  "espeak-ng"
	  
;; 	  "trash-cli"
;; 	  "unzip"
;; 	  "zip"
;; 	  "gtk+:bin"			; gtk-launch
;; 	  "tree"
;; 	  "rsync"
;; 	  "unison"
;; 	  "fd"
;; 	  "ffmpeg"
;; 	  "p7zip"
;; 	  "xdot"
;; 	  "strace"
;; 	  "file"
	  
;; 	  "dbus"
;; 	  ;; not automatically pulled by system service (maybe add to system packages instead?)
;; 	  "pulseaudio"
;; 	  "rust-cargo"	      ; ?
;; 	  "rust"	      ; ?
;; 	  ;; "clang-toolchain" ; ? is gcc better ?
;; 	  "gcc-toolchain"
;; 	  "xorg-server"			; Xephyr
	  
;; 	  "xinit"
;; 	  "xev"
;; 	  "xset"
;; 	  "xmodmap"
;; 	  "setxkbmap"
;; 	  "xsetroot"
;; 	  "xrandr"
;; 	  "arandr"
;; 	  ;; "slock" ; moved to system profile
;; 	  "xss-lock"
;; 	  "libinput"
;; 	  "xinput"			; ? test devices?
	  
;; 	  "mpv"
;; 	  "yt-dlp"
;; 	  "vlc"
	  
;; 	  "qutebrowser"
;; 	  "firefox-esr"
	  
;; 	  "syncthing"
;; 	  "syncthing-gtk"		; necessary??
	  
;; 	  "dunst"
;; 	  "libnotify"			; notify-send
	  
;; 	  "flatpak"
;; 	  "xdg-desktop-portal"		; ?
;; 	  "xdg-desktop-portal-gtk"	; for file manager
	  
;; 	  "keepassxc"

;; 	  "glibc"		 ; adding here bc also in root profile
;; 	  ;; note: firejail is declared in the system configuration
;; 	  ))))

;; (define-public home-desktop-service-type
;;   (service-type (name 'home-desktop)
;; 		(description "My desktop environment service.")
;; 		(extensions
;; 		 (list (service-extension
;; 			home-profile-service-type
;; 			home-desktop-profile-service)))
;; 		(default-value #f)))

;; (define desktop-service
;;   (append
;;    (list (service home-desktop-service-type))
   
;;    display-service))
