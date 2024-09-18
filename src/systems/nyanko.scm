(define-module (src systems nyanko)
  #:use-module (gnu) ; use-package-modules
  #:use-module (gnu system) ; sudoers
  #:use-module (nongnu packages linux) ; import kernel
  #:use-module (nongnu packages video) ; intel-graphics
  #:use-module (nongnu system linux-initrd) ; import firmware
  #:use-module (gnu system setuid) ; setuid
  ;; services
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm) ; power management
  #:use-module (gnu services sysctl) ; wm.swappiness
  #:use-module (gnu services sound)
  #:use-module (gnu services audio)
  #:use-module (gnu services nix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux) ; kernel-module-loader
  #:use-module (gnu services databases) ; mysql experiments
  #:use-module (gnu services virtualization) ; libvirt
  #:use-module (gnu services dbus) ; dbus-root
  #:use-module (gnu services mcron) ; mcron
  #:use-module (gnu services ssh)
  #:use-module (gnu services vpn) ; openvpn
  ;; packages
  #:use-module (gnu packages wm)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages linux) ; access packages (setuid)
  #:use-module (gnu packages suckless) ; slock
  #:use-module (gnu packages xdisorg) ; xscreensaver
  #:use-module (gnu packages glib) ; xdg-dbus-proxy
  #:use-module (gnu packages gnome) ; network-manager-openvpn
  #:use-module (gnu packages networking) ; wireshark
  #:use-module (gnu packages databases) ; mysql-config
  ;; misc
  #:use-module (srfi srfi-1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Components:

;;; --- General -----

(define ri/use-wayland #t)
;; (define ri/use-wayland #f)

;; Fixes wifi issues with AX200:
;; Below has the same effect as running 'iw wlan0 set power_save off'.
;; Both option sets are needed as iwlmvm will override iwlwifi.
;; For iwlmvm: 1=always on, 2=balanced, 3=low-power.
(define iwlwifi-config
  (plain-file "iwlwifi.conf"
              "options iwlwifi power_save=0
               options iwlmvm power_scheme=1"))  

(define %iwlwifi-fix-services
  (list
   ;; fix unstable wifi
   (service kernel-module-loader-service-type
            '("iwlwifi"))
   (simple-service 'iwlwifi-config etc-service-type
                   (list `("modprobe.d/iwlwifi.conf"
                           ,iwlwifi-config)))))

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

;; turn this into just a regular file at .files
(define nonguix-substitute-server-key
  (plain-file
   "non-guix.pub"
   "(public-key (ecc (curve Ed25519) 
(q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define my-keyboard-layout
  (keyboard-layout "us,us" "dvp,"
                   #:options '("grp:sclk_toggle" "ctrl:nocaps")))

;;; --- Services -----

(define %my-base-services
  (cons*
   ;; nonguix substitute server
   (simple-service 'nonguix-substitute-server
                   guix-service-type
                   (guix-extension
                    (substitute-urls
                     (append (list "https://substitutes.nonguix.org")
                             %default-substitute-urls))
                    (authorized-keys
                     (append (list nonguix-substitute-server-key)
                             %default-authorized-guix-keys))))
   ;; lower swappiness
   (modify-services %base-services
     (sysctl-service-type
      config => (sysctl-configuration
                 (settings (append '(("vm.swappiness" . "1"))
                                   %default-sysctl-settings)))))))

(define %my-session-services
  (cond (ri/use-wayland
         ;; delete some from base services, dont return
         (set! %my-base-services
               (modify-services %my-base-services
                 ;; greetd-service-type provides "greetd" PAM service
                 (delete login-service-type)
                 ;; and can be used in place of mingetty-service-type
                 (delete mingetty-service-type)
                 ;; delete default fonts
                 (delete console-font-service-type)))
         ;; return these new services
         (list
          ;; use custom fonts
          (service console-font-service-type
                   (map (lambda (tty)
                          ;; Use a larger font for HIDPI screens
                          (cons tty (file-append
                                     font-terminus
                                     "/share/consolefonts/ter-132n")))
                        '("tty1" "tty2" "tty3")))
          ;; greetd-wayland session
          (service greetd-service-type
                   (greetd-configuration
                    (greeter-supplementary-groups (list "video" "input"))
                    (terminals
                     (list
                      ;; TTY1 is the graphical login screen for Sway
                      (greetd-terminal-configuration
                       (terminal-vt "1")
                       (terminal-switch #t))
                      ;; Set up remaining TTYs for terminal use
                      (greetd-terminal-configuration (terminal-vt "2"))
                      (greetd-terminal-configuration (terminal-vt "3"))))))
          ;; swaylock
          (service screen-locker-service-type
                   (screen-locker-configuration
                    (name "slock")
                    (program (file-append slock "/bin/slock"))
                    (using-pam? #t)
                    (using-setuid? #t)))))
        ;; NOT wayland:
        (else
         (list
          ;; login manager
          (service slim-service-type (slim-configuration
        		              (display ":0")
        		              (vt "vt4")
        		              (xorg-configuration
                                       (xorg-configuration
                                        (keyboard-layout my-keyboard-layout)
                                        ;; IMPORTANT! Libinput.
                                        (extra-config (list %xorg-libinput-config))))))
          ;; screen lock
          (service screen-locker-service-type
                   (screen-locker-configuration
                    (name "swaylock")
                    (program (file-append slock "/bin/swaylock"))
                    (using-pam? #t)
                    (using-setuid? #t)))))))

(define %my-desktop-services
  (list
   ;; -- seat management --------
   ;; cant use seatd bc wireplumber depends on elogind
   (service elogind-service-type)

   ;; -- Xorg --------
   ;; NOTE: Requires (keyboard-layout):
   (service xorg-server-service-type	; maybe solves xinit?
            (xorg-configuration
             (keyboard-layout my-keyboard-layout)))

   ;; x11 socked dir for Xwayland
   (service x11-socket-directory-service-type)

   ;; -- virtualization -------
   ;; vm libs
   (service libvirt-service-type
            (libvirt-configuration
             (unix-sock-group "libvirt")
             (tls-port "16555")))
   (service virtlog-service-type
            (virtlog-configuration
             (max-clients 1000)))
   ;; docker service
   (service docker-service-type)
   (service containerd-service-type)

   ;; -- package management -------
   ;; nix service type
   (service nix-service-type)
   
   ;; -- desktop -------
   ;; mpd basic setup
   (service mpd-service-type)          ; doesn't work, didnt pull depend?
   ;; auto mount usb devices
   (service udisks-service-type)

   ;; -- application configurations -------
   ;; pipewire + brightnessctl udev rules
   (udev-rules-service 'pipewire-add-udev-rules pipewire)
   (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
   ;; for firejail
   (extra-special-file "/usr/bin/xdg-dbus-proxy"
		       (file-append xdg-dbus-proxy "/bin/xdg-dbus-proxy"))
   ;; jack realtime mode
   (service pam-limits-service-type
            (list
             (pam-limits-entry "@realtime" 'both 'rtprio 99)
	     ;; (pam-limits-entry "@realtime" 'both 'nice -19)
             (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))))

(define %my-system-services
  (list
   ;; -- networking --------
   ;; network manager
   (service network-manager-service-type
            (network-manager-configuration
             (vpn-plugins (list network-manager-openvpn))))
   ;; needed for network-manager backend
   (service wpa-supplicant-service-type)
   ;; standalone openvpn client
   ;; (service openvpn-client-service-type)
   ;; bluetooth for filesharing
   ;; (service bluetooth-service-type
   ;;          (bluetooth-configuration
   ;;           (privacy 'network/on)))
   ;; (service nftables-service-type
   ;;          (nftables-configuration
   ;;           (ruleset
   ;;            #~(define nfslsk
   ;;                (nftables-make-ruleset
   ;;                 (nftables-table
   ;;                  (name "filter")
   ;;                  (nftables-chain
   ;;                   (name "input")
   ;;                   (type filter)
   ;;                   (hook input)
   ;;                   (priority 0)
   ;;                   (policy accept)
   ;;                   ;; allow incoming TCP traffic on port 2234
   ;;                   (nftables-rule
   ;;                    (ip protocol tcp)
   ;;                    (tcp dport 2234)
   ;;                    (accept)))))))))
   
   ;; -- system services -------
   ;; polkit (dont exactly know what this does)
   (service polkit-service-type)       ; unbound variable???
   ;; dbus system bus?
   (service dbus-root-service-type)

   ;; -- system optimizations --------
   ;; power management
   (service tlp-service-type
            (tlp-configuration
             ;; for renoise/music DAW
             (cpu-scaling-governor-on-ac (list "performance"))
             (cpu-scaling-governor-on-bat (list "performance"))
             (energy-perf-policy-on-ac "performance")
             (energy-perf-policy-on-bat "performance")))
   ;; cpu frequency scaling
   (service thermald-service-type
            (thermald-configuration
             (adaptive? #t)))
   ;; weekly SSD-trim
   (service fstrim-service-type
            (fstrim-configuration
             (schedule "0 22 * * 4")))
   ;; upower, power consumption monitor?
   (service upower-service-type)

   ;; -- appimages (doesn't work) -----
   (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                       (file-append glibc "/lib/ld-linux-x86-64.so.2"))

   ;; -- garbage collection -----
   (simple-service 'system-cron-jobs
                   mcron-service-type
                   (list
                    ;; Run `guix gc' 5 minutes after midnight every day.
                    ;; Clean up generations older than 2 months and free
                    ;; at least 10G of space.
                    #~(job "5 0 * * *" "guix gc -d 2m -F 10G")))))

(define %my-misc-services
  (list
   ;; Add whatever below:
   ;;
   ;; -- dev/testing ------
   ;; mysql for testing
   (service mysql-service-type
            (mysql-configuration
             (mysql mysql)))
   ))

(define %my-services
  (append
   ;; list of services for iwlwifi
   %iwlwifi-fix-services
   ;; modified %base-services
   %my-base-services
   ;; Wayland or X
   %my-session-services
   ;; general desktop services
   %my-desktop-services
   ;; general system services
   %my-system-services
   ;; misc services
   %my-misc-services))

;;; --- Users: ----

(define %my-users
  (cons*
   (user-account
    (name "nya")
    (comment "Nya")
    (group "users")
    (home-directory "/home/nya")
    (supplementary-groups '("wheel" ; sudo
                            "audio" "video"
                            "netdev" ; network devices
                            "kvm" "libvirt" "docker"
                            "wireshark" ; wireshark w/o sudo (not needed?)
                            "realtime"))) ; music
   %base-user-accounts))

;;; --- Groups: ----

;; TODO: define all things necessary for realtime audio up and grouped.
(define %my-groups
  (cons* (user-group (system? #t) (name "realtime")) ; for realtime audio
         (user-group (system? #t) (name "wireshark")) ; wireshark (not needed?)
         %base-groups))

;;; --- Hostname: ----

(define my-hostname "gnyu")

;;; --- system packages: ----

(define %my-packages
  (append
   (map specification->package+output
	'("vim" "git" "stow"
          "emacs"
          "emacs-exwm" "emacs-desktop-environment"
          "sbcl" "stumpwm-with-slynk"
          ;; "stumpwm:lib" ;; deleted
          "slock" "xss-lock"
          "firejail" "xdg-dbus-proxy"
          "wireguard-tools" ; ?
          "wireshark"
          "jmtpfs"
          "intel-media-driver-nonfree" ; nonguix intel drivers
          "mysql" ; for mysql-service
          ;; "glibc" ; don't need, for dynamic linker hack
          "font-terminus" ; for wayland greetd
          "libva-utils" ; ?
          "gvfs" ; user mounts?
          ;; flatpak
          "xdg-desktop-portal"
          "xdg-desktop-portal-gtk"
          ))
   %base-packages))

;;; --- setuid programs: ----

;; add firejail
(define %my-setuid-programs
  (cons* (setuid-program (program (file-append firejail "/bin/firejail")))
	 ;; (setuid-program (program (file-append xscreensaver "/bin/xscreensaver")))
	 (setuid-program (program (file-append wireshark "/bin/dumpcap"))
                         (group "wireshark"))
	 %setuid-programs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main system configuration:

(operating-system
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (host-name my-hostname)
  (keyboard-layout my-keyboard-layout)

  (users %my-users)
  (groups %my-groups)

  (packages %my-packages)
  (setuid-programs %my-setuid-programs)

  (services %my-services)

  ;; Nonfree kernel and firmware
  (kernel linux)                        ; nonfree kernel
  (firmware (list linux-firmware))      ; all linux firmware
  ;; (initrd microcode-initrd) ; cpu microcode
  (initrd (lambda (file-systems . rest)
            (apply microcode-initrd file-systems
                   #:initrd base-initrd
                   #:microcode-packages (list amd-microcode
                                              intel-microcode)
                   #:keyboard-layout my-keyboard-layout
                   rest)))

  ;; Encrypted LUKS mapped device
  ;; Specify a mapped device for the encrypted root partition
  ;; The UUID is that returned by 'cryptsetup luksUUID'
  (mapped-devices (list (mapped-device
                         (source (uuid "60d359e5-c769-4592-ae5d-80a383c801e1"))
                         (target "enc")
			 (type luks-device-mapping)
			 ;; unlock root device
	                 ;;(type (luks-device-mapping-with-options #:key-file "/key-file.bin"))
			 )))

  ;; list of file systems that get mounted.
  ;; (UUID can be obtained with 'blkid' or 'luksUUID')
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=root") ; TODO: try making root subvol zstd compression
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=boot")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/gnu")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=gnu")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/home")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=home")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/var/log")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=log")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/data")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=data")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/swap")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=swap")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/.snapshots")
                         (device "/dev/mapper/enc")
                         (type "btrfs")
                         (options
                          "subvol=snapshots")
                         (dependencies mapped-devices))
                       ;; (file-system
                       ;;   (mount-point "/persist")
                       ;;   (device "/dev/mapper/enc")
                       ;;   (type "btrfs")
                       ;;   (options
                       ;;    "subvol=persist")
                       ;;   (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot/efi")
                         (type "vfat")
                         (device (uuid "49DF-32B8" 'fat32)))
                       %base-file-systems))

  ;; bootloader
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets '("/boot/efi"))
               (keyboard-layout my-keyboard-layout)
               (timeout 3)
               ;; load initrd with key file for luks decryption
               ;;(extra-initrd "/key-file.cpio")
	       ))

  ;; Specify a swap file for the system, which resides on the
  ;; root file system.
  (swap-devices
   (list
    (swap-space
     (target "/swap/swapfile")
     (discard? #t)
     (dependencies (filter (file-system-mount-point-predicate "/swap")
			   file-systems)))))

  ;; hibernation, blacklist modules
  (kernel-arguments
   (cons* "resume=/dev/mapper/enc"
          "resume_offset=533760" ; use "sudo filefrag -e /swap/swapfile", beginning.
          "modprobe.blacklist=uvcvideo"
          %default-kernel-arguments))
 
  ;; resolution of '.local' host names with mDNS
  (name-service-switch %mdns-host-lookup-nss))

;; end of operating system configuration
