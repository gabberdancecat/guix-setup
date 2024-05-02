;; -*-lisp-*-
;;
;; Madeline :3

  ;;; initial ---------

(in-package :stumpwm)
(setf *default-package* :stumpwm) ; user var???, relative path for funs/vars from package
(defvar *confdir* "~/.stumpwm.d")

;; fix?
;; (require 'sb-cltl2)

(set-prefix-key (kbd "C-z"))
(setf *mouse-focus-policy* :click) ; def :ignore 
;; (setf *debug-level* 20) ; debug!!!

  ;;; Startup ---------

(setf *startup-message* nil) ; no need when initializing
(run-shell-command "~/.stumpwm.d/test-autostart.sh")
;; (run-shell-command "autostart-general.sh stumpwm") ; startup script (create a single one?)

  ;;; Load all config files -----------

(load "~/.stumpwm.d/custom-module-loader.lisp") ; main depd, load first
(load "~/.stumpwm.d/colors.lisp")
(load "~/.stumpwm.d/fonts.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/keybinds.lisp") ; depd commands
(load "~/.stumpwm.d/placement.lisp")
(load "~/.stumpwm.d/modeline.lisp") ; depd windows?
(load "~/.stumpwm.d/utilities.lisp") ; depd keybinds
(load "~/.stumpwm.d/theme.lisp")

  ;;; Modules -------------------

(ri/load-module "beckon") ; teleport cursor when switch window
(ri/load-module "end-session") ; prompt to close applications when quitting wm
(ri/load-module "globalwindows") ; navigate between windows from all workspaces
(ri/load-module "mpd") ; mpd
(ri/load-module "urgentwindows") ; pop-up urgent windows

;; end-session command
(setf end-session:*end-session-command* "loginctl")

;; mpd connect
(mpd:mpd-connect)

;; enable which-key-mode if not enabled
(unless (find 'which-key-mode-key-press-hook *key-press-hook*)
  (which-key-mode))

  ;;; Final ------

(setf *startup-message* "hewwo!")
