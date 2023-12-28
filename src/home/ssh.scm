(define-module (src home ssh)
  #:use-module (gnu services)
  #:use-module (gnu home services ssh)
  #:use-module (guix gexp))

;; https://guix.gnu.org/manual/devel/en/html_node/Secure-Shell.html

(define-public default-ssh-service
  (list (service home-openssh-service-type
		 (home-openssh-configuration
		  (hosts
		   (list (openssh-host (name "ci.guix.gnu.org")
				       (user "charlie"))
			 (openssh-host (name "chbouib")
				       (host-name "chbouib.example.org")
				       (user "supercharlie")
				       (port 10022))))
		  (authorized-keys (list (local-file "alice.pub")))))))
