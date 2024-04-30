(define-module (src home-new owo))

(use-modules (src home-new modules shells) ; bash, aliases
             (src home-new modules desktop) ; wayland env+theme, pipewire, gpg, fontconfig, nix
             ;; (src home-new modules system) ; channels
             ;; (src home-new modules config) ; dotfiles
             (src home-new modules renoise) ; renoise(packages)
             ;; depend
             (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp))

;; home configuration:

(home-environment
 
 ;; packages
 (packages (append
            ;; renoise+depends only
            my/renoise-packages))

 ;; services
 (services (append
            ;; (pipewire+wayland+fontconfig+gpg+nix)
            my/desktop-service
            ;; bash+aliases
            my/bash-service
            ;; dotfiles
            ;; my/setup-service
            )))

;; Ideas:
;; channels, fontconfig, dotfiles, unclutter(wayland?)
