(define-module (src packages wayland))

(use-modules (guix packages)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix build-system zig)
             (guix gexp)
             (gnu packages xorg)
             (gnu packages xdisorg)
             (gnu packages pkg-config)
             (gnu packages man)
             (gnu packages freedesktop)
             (gnu packages wm))

(define-public dwl-custom-9999
  (package
    (inherit dwl)
    (version "9999")
    (name "dwl-custom")
    (source
     (local-file "/home/nya/code/in-use/dwl" #:recursive? #t))))

;; dwl-custom-9999

;; (define-public river-0.2.5
;;   (package
;;     (name "river")
;;     (version "0.2.5")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://codeberg.org/river/river")
;;              (commit (string-append "v" version))
;;              (recursive? #t)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0va3kb063yyqqx67hlgg5r2cibv2s2a0w08grd21jk06nn1n3psx"))))
;;     (build-system zig-build-system)
;;     (arguments
;;      (list
;;       #:phases
;;       #~(modify-phases %standard-phases
;;           (add-after 'install 'install-wayland-session
;;             (lambda* (#:key outputs #:allow-other-keys)
;;               (let* ((out (assoc-ref outputs "out"))
;;                      (wayland-sessions
;;                       (string-append out "/share/wayland-sessions")))
;;                 (mkdir-p wayland-sessions)
;;                 (install-file "contrib/river.desktop"
;;                               wayland-sessions)))))
;;       #:zig-build-flags #~(list "-Dxwayland") ;experimental xwayland support
;;       #:zig-release-type "safe"))
;;     (native-inputs (list libevdev
;;                          libxkbcommon
;;                          pkg-config
;;                          pixman
;;                          scdoc
;;                          wayland
;;                          wayland-protocols
;;                          wlroots-0.16))
;;     (home-page "https://codeberg.org/river/river")
;;     (synopsis "Dynamic tiling Wayland compositor")
;;     (description
;;      "River is a dynamic tiling Wayland compositor with flexible
;; runtime configuration.  It can run nested in an X11/Wayland session or also
;; directly from a tty using KMS/DRM.")
;;     (license license:gpl3)))

