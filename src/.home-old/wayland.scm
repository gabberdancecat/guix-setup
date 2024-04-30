(define-module (src home wayland)
  #:use-module (gnu services) ; service
  #:use-module (gnu home services)) ; home-env-vars-service-type

(define-public wayland-env-vars-service
  (simple-service 'wayland-env-vars-service
		  home-environment-variables-service-type
		  '(;; wayland stuff
		    ("XDG_CURRENT_DESKTOP" . "sway")
                    ("XDG_SESSION_TYPE" . "wayland")
                    ("RTC_USE_PIPEWIRE" . "true")
                    ("SDL_VIDEODRIVER" . "wayland")
                    ("MOZ_ENABLE_WAYLAND" . "1")
                    ("CLUTTER_BACKEND" . "wayland")
                    ("ELM_ENGINE" . "wayland_egl")
                    ("ECORE_EVAS_ENGINE" . "wayland-egl")
                    ("QT_QPA_PLATFORM" . "wayland-egl"))))
