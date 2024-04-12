(define-module (src systems main)
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
  ;; misc
  #:use-module (srfi srfi-1)
  )

;; (define ri/use-wayland #t)
(define ri/use-wayland #f)

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
   "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"
   )
  )

;;; Main system configuration

(operating-system
 (locale "en_US.utf8")
 (timezone "America/New_York")
 (host-name "gnuwu")
 (keyboard-layout
  (keyboard-layout "us,us" "dvp,"
                   #:options '("grp:sclk_toggle" "ctrl:nocaps")))

 ;; user accounts
 (users (cons*
	 (user-account
	  (name "yui")
          (comment "Yui")
          (group "users")
          (home-directory "/home/yui")
          (supplementary-groups '("wheel" ; sudo
                                  "audio" "video"
                                  "netdev" ; network devices
                                  "kvm" "docker"
                                  "realtime"))) ; music
         %base-user-accounts))

 ;; define groups (realtime)
 (groups (cons (user-group (system? #t) (name "realtime"))
               %base-groups))

 ;; globally-installed packages (add pipewire? in user instead?)
 (packages
  (append
   (map specification->package+output
	'("vim"
	  ;; essentials
	  "git" "stow"
	  ;; emacs packages
	  "emacs" "emacs-exwm"
	  ;; necessary?
	  "emacs-desktop-environment"
	  ;; firejail setuid
	  "firejail" "xdg-dbus-proxy"
	  ;; wireguard-tools
	  "wireguard-tools"
	  ;; slock
	  "slock" "xss-lock"
	  ;; stumpwm stuff
	  "sbcl" ; if stumpwm:lib is in guix-home profile, add "sbcl" in there as well
	  "stumpwm-with-slynk"
	  "stumpwm:lib" ; test ; removing test (only one installed, user is better?)
	  ;; mount android phone
	  "jmtpfs"
	  ;; graphics drivers
	  "intel-media-driver-nonfree"
	  ;; for dynamic linker hack
	  "glibc"
          ;; fonts
          "font-terminus"
          ;; graphics
          "libva-utils" ; not sure if this does anything...
	  ;; user mounts
	  "gvfs"
	  ;; https access
	  "nss-certs"))
   %base-packages))

 ;; add firejail and slock to setuid
 (setuid-programs
  (append (list (setuid-program (program (file-append firejail "/bin/firejail")))
		(setuid-program (program (file-append xscreensaver "/bin/xscreensaver"))))
	  %setuid-programs))

 ;; services
 (services
  (append
   ;; list of services for iwlwifi
   %iwlwifi-fix-services
   ;; modify %base-services
   (let ((%my-base-services
          (modify-services %base-services
            (sysctl-service-type
             config => (sysctl-configuration
                        (settings (append '(("vm.swappiness" . "1"))
                                          %default-sysctl-settings)))))))
     ;; if use wayland:
     ;; (cond (ri/use-wayland
     ;;        (cons* (service console-font-service-type
     ;;                        (map (lambda (tty)
     ;;                               ;; Use a larger font for HIDPI screens
     ;;                               (cons tty (file-append
     ;;                                          font-terminus
     ;;                                          "/share/consolefonts/ter-132n")))
     ;;                             '("tty1" "tty2" "tty3")))
     ;;               (service greetd-service-type
     ;;                        (greetd-configuration
     ;;                         (greeter-supplementary-groups (list "video" "input"))
     ;;                         (terminals
     ;;                          (list
     ;;                           ;; TTY1 is the graphical login screen for Sway
     ;;                           (greetd-terminal-configuration
     ;;                            (terminal-vt "1")
     ;;                            (terminal-switch #t))
     ;;                           ;; Set up remaining TTYs for terminal use
     ;;                           (greetd-terminal-configuration (terminal-vt "2"))
     ;;                           (greetd-terminal-configuration (terminal-vt "3"))))))
     ;;               (modify-services %my-base-services
     ;;                 ;; greetd-service-type provides "greetd" PAM service
     ;;                 (delete login-service-type)
     ;;                 ;; and can be used in place of mingetty-service-type
     ;;                 (delete mingetty-service-type)
     ;;                 ;; dont use default
     ;;                 (delete console-font-service-type))))
     ;;       (else
     ;;        %my-base-services))
     %my-base-services
     )
   ;; services
   (list
    ;; -- nonguix substitute server -------
    (simple-service 'nonguix-substitute-server
                    guix-service-type
                    (guix-extension
                     (substitute-urls
                      (append (list "https://substitutes.nonguix.org")
                              %default-substitute-urls))
                     (authorized-keys
                      (append (list nonguix-substitute-server-key)
                              %default-authorized-guix-keys))))
    ;; -- seat management --------
    ;; cant use seatd bc wireplumber depends on elogind
    (service elogind-service-type)

    ;; -- Xorg --------
    ;; NOTE: Requires (keyboard-layout):
    (service xorg-server-service-type	; maybe solves xinit?
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
    ;; login manager
    (service slim-service-type (slim-configuration 
        			(display ":0")
        			(vt "vt4")
        			(xorg-configuration
                                 (xorg-configuration
                                  (keyboard-layout keyboard-layout)
                                  ;; IMPORTANT! Libinput.
                                  (extra-config (list %xorg-libinput-config))))))
    ;; screen lock
    (service screen-locker-service-type
             (screen-locker-configuration
              (name "slock")
              (program (file-append slock "/bin/slock"))
              (using-pam? #t)
              (using-setuid? #t)))
    ;; x11 socked dir for Xwayland
    (service x11-socket-directory-service-type)

    ;; -- networking --------
    ;; network manager
    (service network-manager-service-type
             (network-manager-configuration
              (vpn-plugins (list network-manager-openvpn))))
    ;; needed for network-manager backend
    (service wpa-supplicant-service-type)
    ;; standalone openvpn client
    (service openvpn-client-service-type)
    ;; bluetooth for filesharing
    (service bluetooth-service-type
             (bluetooth-configuration
              (privacy 'network/on)))

    ;; -- system services -------
    ;; polkit (dont exactly know what this does)
    (service polkit-service-type)	; unbound variable???
    ;; dbus system bus?
    (service dbus-root-service-type)
    
    ;; -- virtualization -------
    ;; vm libs
    (service libvirt-service-type
             (libvirt-configuration
              (unix-sock-group "libvirt")
              (tls-port "16555")))
    ;; docker service
    (service docker-service-type)

    ;; -- package management -------
    ;; nix service type
    (service nix-service-type)
    
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
              (schedule "0 17 * * 5")))
    ;; upower, power consumption monitor?
    (service upower-service-type)

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
              (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

    ;; -- dev/testing ------
    ;; mysql for testing
    (service mysql-service-type)

    ;; -- etc -------
    ;; pulseaudio
    ;; (service pulseaudio-service-type) ; disable, user-side

    ;; -- garbage collection -----
    (simple-service 'system-cron-jobs
                    mcron-service-type
                    (list
                     ;; Run `guix gc' 5 minutes after midnight every day.
                     ;; Clean up generations older than 2 months and free
                     ;; at least 10G of space.
                     #~(job "5 0 * * *" "guix gc -d 2m -F 10G")))

    ;; -- Rest -------
    ;; returns list of services
    ;; modify %base-services
    )))

 ;; Nonfree kernel and firmware
 (kernel linux) ; nonfree kernel
 (initrd microcode-initrd) ; cpu microcode
 (firmware (list linux-firmware)) ; all linux firmware

 ;; list of file systems that get mounted.
 ;; (UUID can be obtained with 'blkid' or 'luksUUID')
 (file-systems (append
                (list (file-system
                       (mount-point "/")
                       (device (file-system-label "my-root"))
                       (type "btrfs"))
                      (file-system
                       (mount-point "/home")
                       (device (file-system-label "my-root"))
                       (type "btrfs")
                       (options "subvol=home"))
                      (file-system
                       (mount-point "/swap")
                       (device (file-system-label "my-root"))
                       (type "btrfs")
                       (options "subvol=swap"))
                      (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "304B-6C1C" 'fat))
                       (type "vfat")))
                %base-file-systems))

 ;; bootloader
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets '("/boot/efi"))
              (keyboard-layout keyboard-layout)
              (timeout 3)))

 ;; Specify a swap file for the system, which resides on the
 ;; root file system.
 (swap-devices
  (list
   (swap-space
    (target "/swap/swapfile")
    (dependencies (filter (file-system-mount-point-predicate "/swap")
                          file-systems)))))

 ;; hibernation, blacklist modules
 (kernel-arguments
  (cons* "resume=/dev/nvme0n1p2"
         "resume_offset=11543808"
         "modprobe.blacklist=uvcvideo"
         %default-kernel-arguments))
 
 ;; resolution of '.local' host names with mDNS
 (name-service-switch %mdns-host-lookup-nss))

;; end of operating system configuration
