(define-module (src manifests workstation)
  #:use-module (src home workstation)
  #:use-module (gnu packages))

;; return a manifest to use by guix custom profile
(specifications->manifest

 ;; from (yui home workstation
 workstation-packages
 
 )
