(define-module (src manifests workstation)
  #:use-module (gnu packages))

;; return a manifest to use by guix custom profile
(specifications->manifest
 '("gimp"
   ;; "abiword"
   "libreoffice"
   ;; "ardour"
   ;; "signal-desktop"
   ;; "godot"
   "obs"
   "steam"
   ;; "nheko"
   "minetest"
   ;; "monero-gui"
   ;; "lynx"
   ))
