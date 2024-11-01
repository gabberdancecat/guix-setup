#!/usr/bin/env guile
!#

;; Sway Guile config
;; $HOME/.config/sway/config-guile-dir/init.scm

(use-modules (ice-9 i18n))		; for =access?=

;; Add guile-swayer to path
(add-to-load-path
 (let* ((default-path (string-append (getenv "HOME") "/.config/sway/guile-swayer"))
	(relative-path (if (current-filename)
			   (string-append
			    (dirname
			     (dirname
			      (current-filename)))
			    "/guile-swayer"))))
   (or relative-path
       default-path)))

;; import guile-swayer modules
(use-modules (srfi srfi-18)		; thread-join!
	     (swayipc))

(sway-connect-sockets!) ; connect to sway

;;; Main:

(load "keybinds.scm")

(load "workspaces.scm")

(load "appearance.scm")

(load "theme.scm")

(load "my-which-key.scm")

;;; Misc (to-move):






;;; Finalize:

(format #t "> done!~%")

;; subscribe to all events
(sway-subscribe-all) 

;; start listening to sway events
(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)

