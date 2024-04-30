(define-module (src home misc display)
  #:use-module (gnu services) ; service
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)) ; home-env-vars-service-type

(define-public display-service
  (list
   ;; note: redshift doesn't work on wayland
   ;; (service home-redshift-service-type
   ;; 	    (home-redshift-configuration
   ;; 	     (location-provider 'manual)
   ;; 	     (dawn-time "06:00")
   ;; 	     (dust-time "19:00")))
   (service home-unclutter-service-type
	    (home-unclutter-configuration
	     (idle-timeout 6)))))
