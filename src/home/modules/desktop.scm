(define-module (src home modules desktop))

(use-modules (gnu home services sound) ; pipewire-service
             (gnu home services desktop) ; dbus-service
             (gnu home services shells) ; wayland home-bash-extension
             (gnu home services gnupg) ; gpg-agent-service
             (gnu home services fontutils) ; fontconfig-service
             (gnu home services) ; home-files-service-type
             (gnu packages gnupg) ; gnupg, pinentry-rofi
             (gnu packages) ; specification->package
             (gnu services) ; service
             (guix gexp) ; local-file
             (ice-9 pretty-print)
             (ice-9 rdelim) ; read-line
             )

(export my/desktop-service
        my-pipewire-service
        ;; my-wayland-service
        my-fontconfig-service
        my-gpg-agent-service
        my-nix-service)

;; Notes:
;; - batsignal? (notifs)
;;

;; pipewire

(define my-pipewire-service
  (list
   (service home-dbus-service-type)
   (service home-pipewire-service-type)))

;; wayland

;;; DEPENDS ON BASH SERVICE
;; (define my-wayland-env-vars
;;   '(;; wayland stuff
;;     ("XDG_CURRENT_DESKTOP" . "river")
;;     ("XDG_SESSION_TYPE" . "wayland")
;;     ("RTC_USE_PIPEWIRE" . "true")
;;     ("SDL_VIDEODRIVER" . "wayland")
;;     ("MOZ_ENABLE_WAYLAND" . "1")
;;     ("CLUTTER_BACKEND" . "wayland")
;;     ("ELM_ENGINE" . "wayland")
;;     ("ECORE_EVAS_ENGINE" . "wayland")
;;     ;; ("QT_QPA_PLATFORM" . "wayland") ; breaks qt apps
;;     ("QT_QPA_PLATFORM" . "xcb")
;;     ;; wayland theme
;;     ;; TODO: move this to desktop, add theme to guix-home packages
;;     ;; (also combine config a bit more
;;     ("GTK_THEME" . "Matcha-dark-azul")))

;; (define my-wayland-service
;;   (list
;;    (simple-service 'wayland-env
;;                    home-bash-service-type
;;                    (home-bash-extension
;;                     (environment-variables my-wayland-env-vars)))))

;; fontconfig

(define my-fontconfig-service
  (list
   (simple-service 'additional-fonts-service
                   home-fontconfig-service-type
                   (list "~/.nix-profile/share/fonts"
                         "~/.guix-extra-profiles/desktop/share/fonts"
                         "~/.guix-extra-profiles/emacs/share/fonts"
                         '(alias
                           (family "monospace")
                           (prefer
                            (family "Liberation Mono")))))))

;; gpg-agent

;; if sign_and_send_pubkey error, run "gpg-connect-agent updatestartuptty /bye"
(define my-gpg-agent-service
  (list
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             ;; ssh support
             (pinentry-program
              (file-append pinentry-rofi "/bin/pinentry-rofi"))
             (ssh-support? #t)
             ;; settings
             (default-cache-ttl 28800)
             (max-cache-ttl 28800)
             (default-cache-ttl-ssh 28800)
             (max-cache-ttl-ssh 28800)
             (extra-content
              (string-append
               "default-key "
               (read-line (open-input-file "/home/nya/.gpg-default-key"))))))
   ;; dbus is needed or else gpg ssh will error (OR NOT I GUESS)
   ;; (simple-service 'gpg-agent-dependencies
   ;;                 home-profile-service-type
   ;;                 (map specification->package
   ;;                      '("dbus")))
   ))

;; nix

(define channels-file-content
  "https://nixos.org/channels/nixpkgs-unstable nixpkgs")

(define nixpkgs-file
  (list
   `(".nix-channels" ,(plain-file "nix-channels"
				  channels-file-content))))

(define my-nix-service
  (list
   (simple-service 'home-nixpkgs-service
                   home-files-service-type
                   nixpkgs-file)))

;; combined

(define my/desktop-service
  (append my-pipewire-service
          ;; my-wayland-service
          my-fontconfig-service
          my-nix-service
          my-gpg-agent-service
          ))

;; (pretty-print my/desktop-service)
