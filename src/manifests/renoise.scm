(define-module (src manifests renoise))

(use-modules (gnu packages)  ; specification->
             (guix packages) ; ?
             (guix transformations) ; options->transformation
             (guix profiles) ; concatenate-manifests
             )

;;; transformed renoise package

(define renoise-version "3.4.4")
(define renoise-path "/home/nya/Music/Prod/misc/rns_344_linux_x86_64.tar.gz")

(define renoise-fullname
  (string-append "renoise@" renoise-version))

(define renoise-fullname+path
  (string-append renoise-fullname "=" renoise-path))

(define transform-package-source
  (options->transformation
   `((with-source . ,renoise-fullname+path))))

(define renoise-transformed-package
  (transform-package-source
   (specification->package "renoise@3.4.4")))

;;; returned manifest

(packages->manifest
 (append
  ;; transformed renoise package:
  ;; (list renoise-transformed-package)
  ;; renoise manifest:
  (specifications->packages
   '( ;; renoise depends
     ;; "jack"
     ;; "jack2" ; pipewire already provides libjack.so?
     "qjackctl"
     "qpwgraph"
     "pipewire"      ; need here for pw-jack (just the service is insufficient)
     ;; "carla" ; audio plugin host, VSTs
     ;; "dbus"
     "alsa-utils"
     ;; "openbox"
     ;; "fluxbox"
     "mpg123"
     ;; "libquicktime" ; quicktime is only on mac...
     "rubberband"
     "carla" ; audio plugin host
     ;; fix renoise segfault?
     ;; "glib" ; nah
     ;; "glibc" ;; nah
     ;; "glibc-locales" ;; nah
     ))))



;; (packages->manifest renoise-packages)

