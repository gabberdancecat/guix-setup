;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (src users owo)
  ;; modules
  #:use-module (src home desktop)
  #:use-module (src home workstation)
  #:use-module (src home emacs)
  #:use-module (src home pipewire)
  #:use-module (src home renoise)
  #:use-module (src home shells)
  #:use-module (src home dev)
  #:use-module (src home nix)
  ;; other
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.

 ;; do it like this:
 ;; https://hg.sr.ht/~yoctocell/guixrc/browse/yoctocell/users/yoctocell.scm?rev=tip
 ;; should the manifests be put in the extra-profiles? bc longer n louder installs?
 ;; maybe...
 ;; (packages (map specification->package+output
 ;;        	(list "sed"
 ;;                      "grep"
 ;;                      "vscodium"
 ;;                      "lolcat"
 ;;                      ;; "font-bitstream-vera"
 ;;                      ;; "font-adobe-source-sans-pro"
 ;;                      "gvfs"
 ;;                      "neovim")))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services (append
	    
	    bash-service
	    pipewire-service
	    ;; renoise-service
	    nix-service
	    dev-service
	    ;; desktop-service ; now is packages
	    ;; workstation-service
	    ;; emacs-service

	    ;; TODO: home-channels, fontconfig,
	    ;; https://guix.gnu.org/manual/devel/en/html_node/Fonts-Home-Services.html
	    
	    )))
