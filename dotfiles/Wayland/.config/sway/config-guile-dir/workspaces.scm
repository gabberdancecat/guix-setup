(use-modules (modules workspace-groups)
	     (swayipc))

;; configure workspace groups to sync groups
(define OUTPUTS '("WL-1"))
(define GROUPS
  '(("11-browser")
    ("12-development")
    ("13-databases")
    ("14-communication")
    ("15-development")
    ("16-gaming")
    ("17-mail")
    ("18-development")
    ("19-media")))

;; workspace options
(sway-workspace-auto-back-and-forth SWAY-WORKSPACE-AUTO-BACK-AND-FORTH-OPTION-YES)

(workspace-groups-configure #:groups GROUPS #:outputs OUTPUTS)
(workspace-groups-init)

