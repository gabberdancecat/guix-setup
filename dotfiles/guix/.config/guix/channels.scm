(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "e6b3bb87a46024cd6b2fb1a1cad1387de0053f44")
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
          "969e6b5778a2a0fc04290837519257672383cd18")
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
          "851da3ebbec615bedfd08bf842514f7055dc93d7")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
