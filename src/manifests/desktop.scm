(define-module (src manifests desktop)
  #:use-module (src home desktop)
  #:use-module (gnu packages))

;; return a manifest to use by guix custom profile
(specifications->manifest

 ;; from (yui home desktop)
 desktop-packages
 
 )
