(define-module (src manifests renoise)
  #:use-module (src home renoise)
  #:use-module (gnu packages)
  #:use-module (guix profiles))

;; return a manifest to use by guix custom profile
;; (concatenate-manifests
;;  (list paid-renoise-ver-manifest
;;        (specifications->manifest
;; 	renoise-packages))) ; from (yui home desktop)

;; (packages->manifest renoise-packages)

