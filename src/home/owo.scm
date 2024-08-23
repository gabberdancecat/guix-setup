(define-module (src home owo))

(use-modules (src home modules shells) ; bash, aliases
             (src home modules desktop) ; wayland env+theme, pipewire, gpg, fontconfig, nix
             ;; (src home modules system) ; channels
             (src home modules setup) ; dotfiles
             ;; (src home modules renoise) ; renoise(packages)
             ;; depend
             (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp))

;; home configuration:

(home-environment
 ;; packages
 (packages (append
            (specifications->packages
             '("font-awesome"
               "font-dejavu"
               "font-tamzen"))
            ;; renoise+depends only
            ;; my/renoise-packages
            ))
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
