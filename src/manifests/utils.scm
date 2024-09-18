(define-module (src manifests utils)
  #:use-module (gnu packages))

(specifications->manifest
 '(
   ;; pivotal --
   "alacritty"
   "tmux"
   "htop"
   "stow"
   "trash-cli"
   "ffmpeg"

   ;; util --
   "unzip"
   "zip"
   "fd"
   "p7zip"

   ;; info --
   "tree"
   "bat"
   "strace"
   "fzf" ; also for scripts
   "file"
   "acpi"

   ;; video/music --
   "yt-dlp"
   "mpdscribble"

   ;; dev --
   "pkg-config"
   "rust-cargo" ; ?
   "rust" ; ?
   "rust-analyzer"
   "rust-clippy"
   ;; "clang-toolchain" ; ? is gcc better ?
   "gcc-toolchain"
   "xorg-server" ; Xephyr
   "valgrind"
   "clang" ; needed for company backend emacs
   "gprolog" ; prolog interpreter
   "glibc" ; adding here bc also in root profile
   "zig"
   "guile-hall"
   "icedtea:jdk" ; java dev package

   ;; network --
   "iptables"
   "gdb"
   "net-tools"
   "openssh" ; do i need this?
   "glances"

   ;; scripts --
   "rsync"
   "unison"
   ;; "xdot" ; ? speech-to-text?
   "gtk+:bin" ; gtk-launch
   "espeak-ng"
   "wl-clipboard"

   ;; eww dependencies --
   "gtk+"
   "libdbusmenu"
   "gtk-layer-shell"

   ;; fun --
   "hyfetch"
   "uwufetch"
   "sl"
   "cowsay"
   "btop"

   ;; renoise
   ;; "mpg123"

   ;; System --
   "dbus" ; do i need this? (command to search for which profiles this is in?)

   ;; Security --
   "pinentry-emacs"
   "pinentry-rofi" "rofi" "wofi"
   "pinentry-bemenu"

   ))
