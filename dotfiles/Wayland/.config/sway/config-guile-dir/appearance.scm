(use-modules (swayipc))

;; output
(sway-output "*" "bg ~/Pictures/Random-images/glt-landscape2.png fill")

;; borders
(sway-default-gaps SWAY-GAPS-OPTION-INNER 10)
(sway-default-border-style SWAY-BORDER-STYLE-PIXEL #:n 4)

;; titlebar
(sway-title-format "%title")
(sway-titlebar-border-thickness 1)
(sway-titlebar-padding 5 0) ; horizontal, vertical

;; workspace
(sway-workspace-layout SWAY-LAYOUT-TABBED) ; default layout for empty workspace
(sway-workspace-auto-back-and-forth #f)

;; mouse
(sway-focus-follow-mouse SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO)
(sway-mouse-warping SWAY-MOUSE-WARPING-CONTAINER)
(sway-opacity SWAY-OPACITY-SET 0.95) ; window opacity
(sway-show-marks SWAY-SHOW-MARKS-NO) ; ???
