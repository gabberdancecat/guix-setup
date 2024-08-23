(define-module (src home modules renoise))

(use-modules (gnu packages) ; specification->
             (gnu services) ; service-type
             (gnu home services) ; service-extension
             (guix packages) ; package
             (guix transformations) ; options->transformation
             (guix profiles) ; concatenate-manifests
             )

(export my/renoise-packages)

;;;;;;;;;;;;; Disabled

;;; set renoise version and path here:

(define renoise-version "3.4.3")
(define renoise-path "/home/nya/Music/prod/misc/rns_343_linux_x86_64.tar.gz")

;; rest

(define renoise-fullname
  (string-append "renoise@" renoise-version))

(define renoise-fullname+path
  (string-append renoise-fullname "=" renoise-path))

(define transform-source
  (options->transformation
   `((with-source . ,renoise-fullname+path))))

(define package-renoise-transformed
  (transform-source
   (specification->package "renoise@3.4.3")))

(define my/renoise-packages
  (cons*
   package-renoise-transformed
   (specifications->packages
    '(;; "jack"
      ;; "jack2" ; pipewire already provides libjack.so?
      "qjackctl"
      "qpwgraph"
      "pipewire" ; need here for pw-jack (just the service is insufficient)
      ;; "carla" ; audio plugin host, VSTs
      "dbus"
      "alsa-utils"
      "openbox"
      "fluxbox"
      "mpg123"
      "libquicktime"
      "rubberband"
      ))))
