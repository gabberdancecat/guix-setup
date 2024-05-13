(define-module (src manifests emacs)
  #:use-module (gnu packages))

;; return a manifest to use by guix custom profile
(specifications->manifest
 '(;; putting this here fixed some stuff...
   ;; "emacs"
   "emacs-pgtk"
   ;; vterm
   "emacs-vterm"
   "cmake"
   "make"
   "gcc-toolchain"
   "libvterm"
   "libtool"
   "perl"
   ;; fonts
   "fontconfig" ; for fc-list
   "font-dejavu"
   "font-fantasque-sans"
   "font-borg-sans-mono"
   "font-iosevka"
   "font-iosevka-comfy"
   "font-hermit"
   ;; bitmap fonts
   "xfontsel" ; rem from regular profile (also broken?) (install in system?)
   "font-tamzen" ; nice (dylex but not bold)
   "font-spleen" ; bold robotic
   ;; "font-unscii" ; nice ; takes priority over fontawesome?
   ;; "font-gnu-unifont" ; only unicode? ; doesn't work well with emacs?
   ;; other
   "ncurses" ; for eterm
   "fd" ; for dirvish
   "w3m" ; for w3m
   ;; misc
   "bear" ; put in dev instead?
   ;; email
   "mu"
   "isync"
   "meson"
   "pkg-config"
   "glib"
   "gmime"
   "xapian"
   "ninja"
   ))
