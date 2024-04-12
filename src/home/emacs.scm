(define-module (src home emacs)
  #:use-module (gnu packages)
  #:use-module (gnu services) ; service-type
  #:use-module (gnu home services)) ; service-extension


;; (define (home-emacs-profile-service config)
;;   (map specification->package+output
;;        '(;; putting this here fixed some stuff...
;; 	 "emacs"
;; 	 ;; vterm
;; 	 "emacs-vterm"
;; 	 "cmake"
;; 	 "make"
;; 	 "gcc-toolchain"
;; 	 "libvterm"
;; 	 "libtool"
;; 	 "perl"
;; 	 ;; fix?
;; 	 )))

;; (define-public home-emacs-service-type
;;   (service-type (name 'home-emacs)
;; 		(description "My emacs service.")
;; 		(extensions
;; 		 (list (service-extension
;; 			home-profile-service-type
;; 			home-emacs-profile-service)))
;; 		(default-value #f)))

;; (define-public emacs-service
;;   (list (service home-emacs-service-type)))
