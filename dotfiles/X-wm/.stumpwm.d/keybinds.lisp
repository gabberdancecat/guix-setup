;;; Keybinds
;; notes: *root-map* is C-z, *top-map* is Super.

;; super-key commands

(defcommand send-key (key) ((:string "Enter key: "))
            "Send key to the current window."
            (send-meta-key (current-screen) (kbd key)))

(define-key *top-map* (kbd "s-z") "send-key C-z")
(define-key *top-map* (kbd "s-Z") "send-key C-Z")

(define-key *top-map* (kbd "s-C-H") "exec loginctl hibernate")

(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-t") "move-focus up")
(define-key *top-map* (kbd "s-n") "move-focus down")
(define-key *top-map* (kbd "s-s") "move-focus right")

(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-T") "move-window up")
(define-key *top-map* (kbd "s-N") "move-window down")
(define-key *top-map* (kbd "s-S") "move-window right")

;; command for swapping specific windows

;; Windows ---

;; s-g for fother? feel nice
(define-key *top-map* (kbd "s-g") "fother")
(define-key *top-map* (kbd "s-g") "grouplist")
;; (define-key *top-map* (kbd "s-g") "fnext")
(define-key *top-map* (kbd "s-c") "fprev") ; y
(define-key *top-map* (kbd "s-M-g") "fprev")
(define-key *top-map* (kbd "s-M-c") "fnext") ; y
(define-key *top-map* (kbd "s-m") "fnext")
(define-key *top-map* (kbd "s-b") "fprev")
(define-key *top-map* (kbd "s-M-b") "fnext")
(define-key *top-map* (kbd "s-M-m") "fprev")

(defvar *my-frames-float-keymap* nil)
(setf *my-frames-float-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "float-this")
    (define-key m (kbd "F") "unfloat-this")
    (define-key m (kbd "u") "unfloat-this")
    (define-key m (kbd "C-f") "flatten-floats")
    m))
(define-key *top-map* (kbd "s-w") '*my-frames-float-keymap*)

;;; Groups ---

(define-key *top-map* (kbd "s-M-h") "gprev")
(define-key *top-map* (kbd "s-M-s") "gnext")
(define-key *top-map* (kbd "s-M-g") "gprev")
(define-key *top-map* (kbd "s-M-c") "gnext")
(define-key *top-map* (kbd "s-M-b") "gprev")
(define-key *top-map* (kbd "s-M-m") "gnext")
(define-key *top-map* (kbd "s-M-t") "gother")
;; (define-key *top-map* (kbd "s-$") "gother")
;; testing...
(define-key *top-map* (kbd "s-;") "gother")
;; consider s-w s-v gnext/gprev
(define-key *top-map* (kbd "s-TAB") "gother")
;; (define-key *top-map* (kbd "s-TAB") "fnext")
;; (define-key *top-map* (kbd "s-M-b") "gprev")
;; (define-key *top-map* (kbd "s-M-m") "gnext")

(defcommand ri/gselect (&optional num) (:rest)
            (if num
                (let ((init-group (screen-current-group (current-screen)))
                      (dest-group (select-group (current-screen) num)))
                  (if (eq init-group dest-group)
                      (gother)
                      (gselect num)))
                (grouplist)))

(define-key *top-map* (kbd "s-&") "ri/gselect 1")
(define-key *top-map* (kbd "s-[") "ri/gselect 2")
(define-key *top-map* (kbd "s-{") "ri/gselect 3")
(define-key *top-map* (kbd "s-}") "ri/gselect 4")
(define-key *top-map* (kbd "s-(") "ri/gselect 5")
(define-key *top-map* (kbd "s-=") "ri/gselect 6")
(define-key *top-map* (kbd "s-*") "ri/gselect 7")
(define-key *top-map* (kbd "s-)") "ri/gselect 8")
(define-key *top-map* (kbd "s-+") "ri/gselect 9")
(define-key *top-map* (kbd "s-]") "ri/gselect 0")
(define-key *top-map* (kbd "s-$") "ri/gselect 0")
(define-key *top-map* (kbd "s-%") "gmove 1")
(define-key *top-map* (kbd "s-7") "gmove 2")
(define-key *top-map* (kbd "s-5") "gmove 3")
(define-key *top-map* (kbd "s-3") "gmove 4")
(define-key *top-map* (kbd "s-1") "gmove 5")
(define-key *top-map* (kbd "s-9") "gmove 6")
(define-key *top-map* (kbd "s-0") "gmove 7")
(define-key *top-map* (kbd "s-2") "gmove 8")
(define-key *top-map* (kbd "s-4") "gmove 9")
(define-key *top-map* (kbd "s-6") "gmove 0")
(define-key *top-map* (kbd "s-~") "gmove 0")

(define-key *top-map* (kbd "s-,") "grouplist")

;; personal applicant map
(defvar *my-applications-keymap* nil)
(setf *my-applications-keymap*
      (let ((m (make-sparse-keymap)))
	(define-key m (kbd "f") "exec firefox") ; do i need exec? 
	(define-key m (kbd "l") "exec flatpak run io.gitlab.librewolf-community")
	(define-key m (kbd "p") "exec firejail keepassxc") ; firejail-guix-run keepassxc?
	(define-key m (kbd "d") "exec flatpak run dev.vencord.Vesktop")
	(define-key m (kbd "s") "exec flatpak run com.spotify.Client")
	(define-key m (kbd "c") "exec flatpak run com.valvesoftware.Steam")
	(define-key m (kbd "e") "exec emacsclient -a '' -c")
	(define-key m (kbd "r") "exec run-renoise")
	m))
(define-key *root-map* (kbd "a") '*my-applications-keymap*)

;; personal function commands map
(defvar *my-function-commands-map* nil)
(setf *my-function-commands-map*
      (let ((m (make-sparse-keymap)))
	(define-key m (kbd "f") "exec playerctl next")
	(define-key m (kbd "b") "exec playerctl previous")
	m))
(define-key *root-map* (kbd ",") '*my-function-commands-map*)

;; personal misc commands map
(defvar *my-misc-commands-map* nil)
(setf *my-misc-commands-map*
      (let ((m (make-sparse-keymap)))
	(define-key m (kbd "l") "ri/load-module")
	(define-key m (kbd "r") "run-shell-command")
	(define-key m (kbd "s") "exec ~/.bin/sct-auto-adjust.sh")
	(define-key m (kbd "t") "text-to-speech")
	(define-key m (kbd "T") "killall-tts")
	m))
(define-key *root-map* (kbd ".") '*my-misc-commands-map*)

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
	    (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
	      (when cmd
		(eval-command cmd t))))

;; end-session, replace quit-session
(define-key *root-map* (kbd "q") "end-session")
;; Delete window
(define-key *root-map* (kbd "d") "delete")
(define-key *root-map* (kbd "C-d") "delete")
;; other window (alt: o, C-o)
(define-key *root-map* (kbd "C-o") "fnext")

;; emacsclient-or-emacs
(define-key *root-map* (kbd "e") "emacsclient")
(define-key *root-map* (kbd "C-e") "emacsclient")
;; alacritty
;; (define-key *root-map* (kbd "s-RET") "term")
;; (define-key *root-map* (kbd "c") "term")
(define-key *top-map* (kbd "s-RET") "emacs-vterm")
(define-key *top-map* (kbd "s-S-RET") "emacs-multi-vterm")
(define-key *root-map* (kbd "c") "emacs-vterm")
(define-key *root-map* (kbd "C") "emacs-multi-vterm")
;; Browse somewhere
;; (define-key *root-map* (kbd "b") "colon1 exec firefox http://www.")
;; Ssh somewhere
(define-key *root-map* (kbd "C-s") "colon1 exec xterm -e ssh ")
;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock")
;; Grouplist
(define-key *root-map* (kbd "C-t") "grouplist")

;; Web jump (works for DuckDuckGo and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
	       (nsubstitute #\+ #\Space search)
	       (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "duckduckgo" "firefox https://duckduckgo.com/?q=")
(make-web-jump "imdb" "firefox http://www.imdb.com/find?q=")

;; C-t M-s is a terrble binding, but you get the idea.
(define-key *root-map* (kbd "M-s") "duckduckgo")
(define-key *root-map* (kbd "i") "imdb")

  ;;; XF86keys
(define-key *top-map*
    (kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map*
    (kbd "XF86AudioMute") "exec pactl set-sink-mute @DEFAULT_SINK@ toggle")

(define-key *top-map*
    (kbd "XF86AudioLowerVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ -10%")
(define-key *top-map*
    (kbd "XF86AudioRaiseVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ +10%")

(define-key *top-map*
    (kbd "S-XF86AudioLowerVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ -5%")
(define-key *top-map*
    (kbd "S-XF86AudioRaiseVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ +5%")

(define-key *top-map*
    (kbd "XF86MonBrightnessDown") "exec brightnessctl set 10%-")
(define-key *top-map*
    (kbd "XF86MonBrightnessUp") "exec brightnessctl set +10%")
(define-key *top-map*
    (kbd "S-XF86MonBrightnessDown") "exec brightnessctl set 5%-")
(define-key *top-map*
    (kbd "S-XF86MonBrightnessUp") "exec brightnessctl set +5%")
