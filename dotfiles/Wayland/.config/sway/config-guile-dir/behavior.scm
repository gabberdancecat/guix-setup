
(use-modules (swayipc))

;; for_window [title="Firefox — Sharing Indicator"] kill
;; for_window [title=".* - mpv$"] floating enable, sticky enable

(sway-for-window (sway-criteria #:class "^Steam$" #:title "^Friends$") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title "Steam - News") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title ".* - Chat") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title "^Settings$") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title ".* - event started") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title ".* CD key") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title "^Steam - Self Updater$") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title "^Screenshot Uploader$") "floating enable")
(sway-for-window (sway-criteria #:class "^Steam$" #:title "^Steam Guard - Computer Authorization Required$") "floating enable")
(sway-for-window (sway-criteria #:title "^Steam Keyboard$") "floating enable")
