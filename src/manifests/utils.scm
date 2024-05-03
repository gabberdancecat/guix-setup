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
   
   ;; fun --
   "hyfetch"
   "uwufetch"
   "sl"
   "cowsay"
   "btop"

   ;; System --
   "dbus" ; do i need this? (command to search for which profiles this is in?)

   ;; Security --
   "pinentry-emacs"
   "pinentry-rofi" "rofi" "wofi"
   "pinentry-bemenu"

   ))
