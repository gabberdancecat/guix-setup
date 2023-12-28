(define-module (src home renoise)
  #:use-module (gnu packages)
  #:use-module (gnu services) ; service-type
  #:use-module (gnu home services) ; service-extension
  #:use-module (guix packages) ; package
  #:use-module (guix transformations) ; options->transformation
  #:use-module (guix profiles) ; concatenate-manifests
  )

(define transform-install-path
  (options->transformation
   '((with-source
      . "renoise=/home/yui/Music/prod/misc/rns_343_linux_x86_64.tar.gz"))))

(define-public renoise-util-packages
  (cons*
   (transform-install-path (specification->package "renoise"))
   (specifications->packages
    '( ;; vv jack vv
      "jack" "jack2" "qjackctl"
      "dbus"
      "alsa-utils"
      "openbox"				; for running inside stumpwm
      "fluxbox"

      ;; "renoise-full"
      ))))


;; (define-public paid-renoise-pkg
;;   (package
;;     (inherit
;;      (transform-install-path (specification->package "renoise")))
;;     (name "renoise-full")))

(define-public renoise-packages
  ;; return full manifest
  ;; paid-renoise-pkg
  renoise-util-packages)

;; create symlink for dynamic linker

;; (define-public home-renoise-service-type
;;   (service-type (name 'home-renoise)
;; 		(description "My renoise service.")
;; 		(extensions
;; 		 (list
;; 		  ;; (service-extension
;; 		  ;;  home-profile-service-type
;; 		  ;;  home-renoise-profile-service)
;; 		  ))
;; 		(default-value #f)))

;; (define-public renoise-service ; use
;;   (list
;;    (service home-renoise-service-type)))

