(define-module (src home dev)
  #:use-module (gnu home services gnupg) ; home-gnupg-agent
  #:use-module (gnu packages gnupg) ; gnupg
  #:use-module (gnu services) ; service
  #:use-module (guix gexp)) ; local-file

(define-public dev-service
  (list
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             ;; ssh support
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)
             ;; settings
             (default-cache-ttl 28800)
             (max-cache-ttl 28800)
             (default-cache-ttl-ssh 28800)
             (max-cache-ttl-ssh 28800)))))
