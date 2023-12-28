(define-module (src manifests emacs)
  #:use-module (src home emacs)
  #:use-module (gnu packages))

;; return a manifest to use by guix custom profile
(specifications->manifest

 ;; from (yui home emacs)
 emacs-packages
 
 )
