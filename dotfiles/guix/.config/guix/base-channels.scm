(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       ;; (channel
       ;;  (name 'my-channel)
       ;;  (branch "main")
       ;;  (url "file:///home/yui/yui/channel"))
       (channel
        (name 'yumi)
        (url "https://github.com/senkowo/guix-channel")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "d1256cf136ced2ed4a3736417fbced678bacb1f6"
          (openpgp-fingerprint
           "864D 7D40 2260 D1A5 E9B9  AC9B B703 FEDE 1CF1 30EA"))))
       (channel
        (name 'home-service-dwl-guile)
        (url "https://github.com/engstrand-config/home-service-dwl-guile")
        (branch "main")
        (introduction
         (make-channel-introduction
          "314453a87634d67e914cfdf51d357638902dd9fe"
          (openpgp-fingerprint
           "C9BE B8A0 4458 FDDF 1268 1B39 029D 8EB7 7E18 D68C"))))
       %default-channels)

;; cons* appends channel to the end of %default-channels
;; https://gitlab.com/nonguix/nonguix
