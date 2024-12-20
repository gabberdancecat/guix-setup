(define-module (src home owo))

(use-modules (src home modules shells) ; bash, aliases
             (src home modules desktop) ; wayland env+theme, pipewire, gpg, fontconfig, nix
             ;; (src home modules system) ; channels
             (src home modules setup) ; dotfiles
             ;; (src home modules renoise) ; renoise(packages)
             ;; (src home modules dwl-config) ; dotfiles
             (src packages dwl) ; dwl-custom
             ;; depend
             (gnu home)
             (gnu home services shepherd)
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
               "font-tamzen"

               "mpd"))
            (list dwl-custom
                  slstatus-custom
                  dwlb-custom)
            ;; renoise+depends only
            ;; my/renoise-packages
            ))
 ;; services
 (services
  (append
   ;; (pipewire+wayland+fontconfig+gpg+nix)
   my/desktop-service
   ;; bash+aliases
   ;; my/bash-service
   ;; my/zsh-service
   ;; dotfiles
   ;; my/setup-service
   ;; my/dwl-service
   ;; (list (service home-shepherd-service-type
   ;;                (home-shepherd-configuration
   ;;                 (services
   ;;                  (list
   ;;                   (shepherd-service
   ;;                    (provision '(mpd))
   ;;                    (start #~(make-system-constructor "mpd"))
   ;;                    (stop #~(make-system-destructor "mpd" "--kill"))
   ;;                    (documentation "Start the Music Player Daemon")))))))
   )))

;; Ideas:
;; channels, fontconfig, dotfiles, unclutter(wayland?)
