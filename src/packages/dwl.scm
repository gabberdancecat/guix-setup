(define-module (src packages dwl))

(use-modules (gnu packages wm)
             (gnu packages fontutils) ; fcft
             (gnu packages suckless)
             (guix packages)
             (guix gexp)
             ;; for dwlb:
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages freedesktop) ; wayland
             (gnu packages xdisorg) ; pixman
             (gnu packages pkg-config)
             (guix utils) ; cc-for-target
             )

(define-public slstatus-custom
  (package
    (inherit slstatus)
    (name "slstatus-custom")
    (source
     (local-file "/home/nya/code/in-use/slstatus" #:recursive? #t))))

(define-public dwl-custom
  (package
    (inherit dwl)
    (name "dwl-custom")
    (source
     ;; (local-file "/home/nya/Downloads/dwl-v0.7" #:recursive? #t)
     (local-file "/home/nya/code/in-use/dwl-v0.7" #:recursive? #t)
     )
    (inputs (modify-inputs (package-inputs dwl)
                           (append fcft pixman)))))

(define-public dwlb-custom
  (package
    (name "dwlb")
    (version "9999")
    (source
     (local-file "/home/nya/code/in-use/dwlb" #:recursive? #t)
     ;; (origin
     ;;   (method git-fetch)
     ;;   (uri (git-reference
     ;;         (url "https://github.com/kolunmi/dwlb")
     ;;         (commit "0daa1c1fdd82c4d790e477bf171e23ca2fdfa0cb")
     ;;         ))
     ;;   (sha256
     ;;    (base32
     ;;     "08a949yhmv7492r5d7a1c5kq23g12i2706qq8ibgs13hlhib9v86")))
     )
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; (delete 'check)
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (inputs (list wayland wayland-protocols pixman fcft))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/kolunmi/dwlb")
    (synopsis "Feature-Complete Bar for DWL.")
    (description "Feature-Complete Bar for DWL.")
    (license license:gpl3)))

