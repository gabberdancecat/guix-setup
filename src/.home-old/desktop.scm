(define-module (src home desktop)
  #:use-module (src home misc display)
  #:use-module (gnu packages)
  #:use-module (gnu services) ; service-type
  #:use-module (gnu home services) ; service-extension
  #:use-module (gnu packages glib)) ; glib

;; (define-public home-desktop-service-type
;;   (service-type (name 'home-desktop)
;; 		(description "My desktop environment service.")
;; 		(extensions
;; 		 (list (service-extension
;; 			home-profile-service-type
;; 			home-desktop-profile-service)))
;; 		(default-value #f)))

;; (define desktop-service
;;   (append
;;    (list (service home-desktop-service-type))
   
;;    display-service))
