(use-modules (swayipc))

(load "Functions.scm")

;; output
(sway-output "*" "bg ~/Pictures/Random-images/glt-landscape2.png fill")

;; borders
(sway-default-gaps SWAY-GAPS-OPTION-INNER 10)
(sway-default-border-style SWAY-BORDER-STYLE-PIXEL #:n 2)

;; titlebar
(sway-title-format "%title")
(sway-titlebar-border-thickness 0)
(sway-titlebar-padding 8 3) ; horizontal, vertical

;; workspace
(sway-workspace-layout SWAY-LAYOUT-TABBED) ; default layout for empty workspace
(sway-workspace-auto-back-and-forth #t)

;; mouse
(sway-focus-follow-mouse SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO)
(sway-mouse-warping SWAY-MOUSE-WARPING-CONTAINER)
(sway-opacity SWAY-OPACITY-SET 0.95) ; window opacity
(sway-show-marks SWAY-SHOW-MARKS-NO) ; ???

;; gsettings
(exec "gsettings set org.gnome.desktop.interface gtk-theme 'Matcha-dark-azul'")
(exec "gsettings set org.gnome.desktop.interface icon-theme 'Papirus-Dark'")
(exec "gsettings set org.gnome.desktop.interface font-name 'Iosevka Aile 11'")

;; misc
(sway-font "Adobe Helvetica 10")
;; (sway-show-marks SWAY-SHOW-MARKS-NO)
