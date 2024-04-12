(define-module (src home workstation)
  #:use-module (gnu packages)
  #:use-module (gnu services) ; service-type
  #:use-module (gnu home services)) ; service-extension

;; (define (home-workstation-profile-service config)
;;   (map specification->package+output
;;        '("gimp"
;; 	 "abiword"
;; 	 "libreoffice"
;; 	 "ardour"
;; 	 "signal-desktop"
;; 	 "ungoogled-chromium"
;; 	 "godot"
;; 	 "obs")))

;; (define-public home-workstation-service-type
;;   (service-type (name 'home-workstation)
;; 		(description "My worstation service.")
;; 		(extensions
;; 		 (list (service-extension
;; 			home-profile-service-type
;; 			home-workstation-profile-service)))
;; 		(default-value #f)))

;; (define-public workstation-service
;;   (list (service home-workstation-service-type)))
