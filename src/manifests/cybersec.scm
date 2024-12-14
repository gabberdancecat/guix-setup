(define-module (src manifests cybersec)
  #:use-module (gnu packages))

;; --- Useful commands: ----

;; return a manifest to use by guix custom profile

(specifications->manifest
 '(
   ;; "wireshark" ; moved to system profile
   ;; "qbittorrent"
   "python"
   ;; "remmina"
   "ncftp"
   "ansible"
   ;; "openconnect"
   ;; "cutter" ; nixed
   ;; "rizin" ; nixed
   ;; "binwalk"
   ;; "perl-image-exiftool"
   "netcat"
   ;; "fuse" ;; for appimage ?????
   "openvpn"
   "nmap"
   ;; "samba" ; smbclient
   "mysql"
   ;; "awscli"
   "sshpass"
   ))
