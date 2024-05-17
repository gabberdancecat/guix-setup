(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "d1609e765bb48d1a5302ed0df4f1ed300a571eff")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'yumi)
        (url "https://github.com/senkowo/guix-channel")
        (branch "master")
        (commit
          "3cd49819af66c3379d932989f6d07660e5707e9c")
        (introduction
          (make-channel-introduction
            "d1256cf136ced2ed4a3736417fbced678bacb1f6"
            (openpgp-fingerprint
              "864D 7D40 2260 D1A5 E9B9  AC9B B703 FEDE 1CF1 30EA"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "898b5f30f3d485d48275c920da172863da9524c6")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
