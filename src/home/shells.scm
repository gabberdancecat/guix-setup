(define-module (src home shells)
  #:use-module (src home misc aliases) ; my-misc-aliases
  #:use-module (gnu home services) ; home-env-vars-service-type
  #:use-module (gnu home services shells) ; home-bash
  #:use-module (gnu services) ; service
  #:use-module (guix gexp)) ; local-file

(define env-service
  (simple-service 'general-env-vars-service
		  home-environment-variables-service-type
		  '(;; Sort hidden (dot) files first in `ls` listings
                    ("LC_COLLATE" . "C")
		    ;; fix CC compilation
		    ("CC" . "gcc")
		    ;; Emacs is our editor
		    ("VISUAL" . "emacsclient")
                    ("EDITOR" . "emacsclient")
		    ;; add to path
		    ("PATH" . "$HOME/.bin:$PATH")
		    ;; make flatpak apps visible
                    ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
		    ;; add setuid programs to path
		    ("PATH" . "/run/setuid-programs:$PATH")
		    ;; FIX for stumpwm???
		    ;; ("SBCL_HOME" . "$HOME/.guix-home/profile/lib/sbcl")
		    ;; no telemetry when compiling osu
		    ("DOTNET_CLI_TELEMETRY_OPTOUT" . "1"))))

(define-public bash-service
  (list
   (service home-bash-service-type
	    (home-bash-configuration
	     (aliases my-misc-aliases) ; get aliases from (. misc aliases)
	     (bashrc (list (local-file
			    "../.files/.bashrc" "bashrc")))
	     (bash-profile (list (local-file
				  "../.files/.shell_profile" "bash_profile")))))
   env-service))
