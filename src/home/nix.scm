(define-module (src home nix)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)) ; plain file

(define nix-channels-file
  "https://nixos.org/channels/nixpkgs-unstable nixpkgs")

(define (home-nix-files-service config)
  (list
   `(".nix-channels" ,(plain-file "nix-channels"
				  nix-channels-file))))

(define-public home-nixpkgs-service-type
  (service-type (name 'home-nixpkgs)
		(extensions
		 (list (service-extension
			home-files-service-type
			home-nix-files-service)))
		(default-value #f)
		(description "Prepares files like nix-channels for Nix.")))

(define-public nix-service
  (list
   (service home-nixpkgs-service-type)))
