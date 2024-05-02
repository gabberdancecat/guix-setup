  ;;; Commands ----------------------

(defvar *terminal* "alacritty")
(defvar *emacs-vterm* (format nil "~A~A" "emacsclient -ce '(vterm)' -a " *terminal*))
(defvar *emacs-multi-vterm* (format nil "~A~A" "emacsclient -ce '(multi-vterm)' -a " *terminal*))

(defcommand firefox () ()
	    (run-or-raise "firefox" '(:class "firefox")))

(defcommand emacsclient () ()
	    (run-shell-command "emacsclient -c -a emacs"))

(defcommand term () ()
	    (run-shell-command (format nil "~A" *terminal*)))

(defcommand emacs-vterm () ()
	    (run-shell-command (format nil "~A" *emacs-vterm*)))

(defcommand emacs-multi-vterm () ()
	    (run-shell-command (format nil "~A" *emacs-multi-vterm*)))

;; half-split term (not best...):
;; vsplit fnext term
;; remove 

;; real scratchpad... (just import module? yeah best solution...)


;; espeak-ng 
(defcommand text-to-speech () ()
	    (run-shell-command "~/.bin/text-to-speech-clipboard.sh"))
(defcommand killall-tts () ()
	    (run-shell-command "killall espeak-ng"))
