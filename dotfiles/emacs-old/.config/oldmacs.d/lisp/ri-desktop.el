(setup (:pkg exwm)
  ;; Systray addons
  (require 'exwm)
  (setq display-time-day-and-date t
        display-time-24hr-format t
        display-time-mode t
        display-battery-mode t
        display-time-default-load-average nil)
  (setq exwm-workspace-number 9 ; 0-8
        focus-follows-mouse nil ; click-to-focus
        ;; exwm-workspace-warp-cursor t ; ?
        exwm-debug t) ; enable for debug mode

  ;; See all X windows with exwm-switch-to-buffer, so can pull into current workspace
  (setq exwm-layout-show-all-buffers nil)

  ;; Display all EXWM buffers in every workspace buffer list
  ;; Could always have another keybind that shows all active EXWM buffers...
  (setq exwm-workspace-show-all-buffers nil) ; def nil, better?

  ;; Detach the minibuffer (restart req) (show it with exwm-workspace-toggle-minibuffer)
  (setq exwm-workspace-minibuffer-position nil)

  ;; set screen resolution (arandr to graphically extract xrandr command)
  ;;  (call before exwm-init!)
  (require 'exwm-randr)
  (exwm-randr-enable) ; set resolution before init.
  (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --off --output DP-1 --off --output DP-2 --off --output DP-3 --off --output DP-4 --off")

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height nil)
  (exwm-systemtray-enable)

  (exwm-enable))

;; -- buffer rename hooks ---

(defun ri/exwm-update-class ()
  (exwm-workspace-rename-buffer (format "EXWM: %s" exwm-class-name)))

;; When window "class" updates, use it to set the buffer name
(when ri/exwm-enabled
  (add-hook 'exwm-update-class-hook #'ri/exwm-update-class))

(defun ri/exwm-update-title ()
  (pcase exwm-class-name
    ("librewolf" (exwm-workspace-rename-buffer (format "Librewolf: %s" exwm-title)))
    ("vterm" (exwm-workspace-rename-buffer (format "vterm: %s" exwm-title)))))

;; When window title updates, use it to set the buffer name
(when ri/exwm-enabled
  (add-hook 'exwm-update-title-hook #'ri/exwm-update-title))

;; --- helper functions

;; need (interactive) ?
(defun ri/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun ri/position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))
    (exwm-floating-move (- pos-x) (- pos y))))

;; --- exwm startup functions ---

;; depends on nm-applet, pasystray(?), pavucontrol(?), and blueman
(defun ri/exwm-init-hook ()
  ;; (with-eval-after-load ;'perspective
    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 0)
    ;; Launch programs at startup
    ;; (eshell)
    ;; Run programs in background at startup
    ;; (ri/run-in-background "nm-applet")
    ;; (ri/run-in-background "pasystray")
    ;; (ri/run-in-background "blueman-applet")
    ;; (ri/run-in-background "snixembed && iwgtk -i")
    (ri/run-in-background (concat user-emacs-directory "exwm/prepare-exwm.sh"))
    )

;; When EXWM starts up, run some functions
(when ri/exwm-enabled
  (add-hook 'exwm-init-hook #'ri/exwm-init-hook))

;; --- commands for navigating workspaces ---

;; switch to last workspace (hack)
(defvar ri/exwm-workspace--switch-history-hack (cons exwm-workspace-current-index '()))
(defun ri/exwm-workspace-switch-to-last ()
  "Switch to the workspace that was used before current workspace"
  (interactive)
  (exwm-workspace-switch (cdr ri/exwm-workspace--switch-history-hack)))
;;
(defun ri/exwm-workspace-switch-to-last-hack ()
  "Save location of current workspace when switching, so it can be used by switch-to-last"
  (interactive)
  (message " [%s]" exwm-workspace-current-index)
  (setq ri/exwm-workspace--switch-history-hack
        (cons exwm-workspace-current-index
              (car ri/exwm-workspace--switch-history-hack))))
;;
(when ri/exwm-enabled
  (add-hook 'exwm-workspace-switch-hook #'ri/exwm-workspace-switch-to-last-hack))

;; if workspace switch to the same one, go to the last one

;; switch to next workspace
(defun ri/exwm-workspace-switch-to-next ()
  "Switch to the next workspace"
  (interactive)
  (exwm-workspace-switch (+ exwm-workspace-current-index 1)))

;; switch to previous workspace
(defun ri/exwm-workspace-switch-to-previous ()
  "Switch to the previous workspace"
  (interactive)
  (exwm-workspace-switch (- exwm-workspace-current-index 1)))

;; switch to last window (any frame inclusive)
(defun ri/switch-to-last-window-any-frame ()
  "Switch to the last window, including "
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

;; switch to last window (unused)
(defvar ri/last-window-direction 1)
(defun ri/switch-to-last-window ()
  (interactive)
  (other-window ri/last-window-direction)
  (setq ri/last-window-direction (- 0 ri/last-window-direction)))

(when ri/exwm-enabled
  ;; translate "s-`" to "s-0" so that i can access workspace zero (for non-DVP)
  (define-key key-translation-map (kbd "s-`") (kbd "s-0"))

  ;; an alist of dvp keybinds (car) to key-translation-map to (cdr)
  (setq ri/dvp-super-translation-alist
        '(("s-$" . "s-0")
          ("s-&" . "s-1")
          ("s-[" . "s-2")
          ("s-{" . "s-3")
          ("s-}" . "s-4")
          ("s-(" . "s-5")
          ("s-=" . "s-6")
          ("s-*" . "s-7")
          ("s-)" . "s-8")
          ("s-+" . "s-9")
          ("s-]" . "s-0")))
  ;; perform dvp key-translation map
  (cl-loop for (input . output) in ri/dvp-super-translation-alist
           do (define-key key-translation-map (kbd input) (kbd output)))
  ;; (mapcar (lambda (pair)
  ;;           (let ((input (car pair))
  ;;                 (output (cdr pair)))
  ;;             (define-key key-translation-map (kbd input) (kbd output))))
  ;;         ri/dvp-super-translation-alist)

  ;; send to emacs instead of X application
  (setq exwm-input-prefix-keys
        '(?\M-x
          ?\M-:
          ?\M-` ; text-mode menubar
          ?\M-o
          ?\M-& ; async-shell command
          ?\C-x
          ?\C-h
          ?\C-u ; new C-x
          ?\C-\M-- ; does this even work?
          ?\C-\M-g ; does this even work?
          ?\s-`     ;; for translate "s-`" to "s-0" hack
          ?\C-\M-\)  ;; for adjust/move windows
          ?\s-\M-t  ;; last workspace
          ?\s-\M-h  ;; prev workspace
          ?\s-\M-s  ;; next workspace
          ?\C-\     ;; C-<space> (open application)
          ?\s-\C-L ; Enter Hibernation
          ;; dvorak accomidations
          ?\s-$
          ?\s-&
          ?\s-\[
          ?\s-{
          ?\s-}
          ?\s-\(
          ?\s-=
          ?\s-*
          ?\s-\)
          ?\s-+
          ?\s-\]
          ))

  ;; C-q to send next key to X-applicaiton
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Already auto-defined???
  ;; (define-key exwm-mode-map (kbd "C-c <return>") 'exwm-workspace-move-window)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(,@
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          (mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        ;; if input is same as current workspace, go to last
                        (if (eq ,i exwm-workspace-current-index)
                            (ri/exwm-workspace-switch-to-last)
                          (exwm-workspace-switch-create ,i)))))
                  (number-sequence 0 9))

          ;; Reset to line-mode
          ;; (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          (,(kbd "s-r") . exwm-reset)

          ;; Move between windows
          (,(kbd "s-h") . windmove-left)
          (,(kbd "s-t") . windmove-up)
          (,(kbd "s-n") . windmove-down)
          (,(kbd "s-s") . windmove-right)

          ;; Move windows
          (,(kbd "s-H") . windmove-swap-states-left)
          (,(kbd "s-T") . windmove-swap-states-up)
          (,(kbd "s-N") . windmove-swap-states-down)
          (,(kbd "s-S") . windmove-swap-states-right)

          ;; Shortcuts for windows
          (,(kbd "s-g") . (lambda () (interactive) (other-window -1)))
          (,(kbd "s-c") . (lambda () (interactive) (other-window 1)))
          (,(kbd "s-d") . delete-window)

          ;; alternatively, s-g + s-S-g for windows, and s-c for launcher or sum (comfy?)
          ;; or maybe s-o for windows and s-u s-i for buffers?

          ;; buffers and more
          (,(kbd "s-o") . meow-last-buffer)
          (,(kbd "s-b") . counsel-switch-buffer)

          ;; Toggles
          (,(kbd "s-F") . exwm-floating-toggle-floating)
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-m") . exwm-layout-toggle-mode-line)

          ;; Launch applications via shell command
          (,(kbd "s-a") . (lambda (command)
                            (interactive (list (read-shell-command "$ ")))
                            (start-process-shell-command command nil command)))

          ;; Special
          (,(kbd "s-C-H") . (lambda () (interactive)
                              (start-process-shell-command "hibernate" nil
                                                           "loginctl hibernate")))
          (,(kbd "s-C-L") . (lambda () (interactive)
                              (desktop-environment-lock-screen)))

          ;; Switch workspace
          (,(kbd "s-w") . exwm-workspace-switch)
          (,(kbd "s-M-t") . ri/exwm-workspace-switch-to-last)
          (,(kbd "s-M-h") . (lambda () (interactive) (ri/exwm-workspace-switch-to-previous)))
          (,(kbd "s-M-s") . (lambda () (interactive) (ri/exwm-workspace-switch-to-next)))

          ))

  (exwm-input-set-key (kbd "s-<tab>") 'ri/exwm-workspace-switch-to-last)
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-<return>") 'multi-vterm-dedicated-toggle))

;; if game has trouble put it in exwm-input-release-keyboard, and then s-r to reset.
;; shrink and expand windows...
;; spawn mpv in corner of screen so can watch youtube video.
;; make function keys work in fullscreen... always pass to emacs instead of exwm.
;; move focus onto popup window.

;; Run when startup floating window
;;  (happens before exwm-manage-finish-hook (configure-window-startup))
(defun ri/exwm-configure-floating-setup ()
  (interactive)
  (exwm-layout-hide-mode-line))

;; Hide the modeline on all floating windows
(when ri/exwm-enabled
  (add-hook 'exwm-floating-setup-hook #'ri/exwm-configure-floating-setup))

;; does this even work?
(defun ri/exwm--custom-switch-to-buffer (buffer-or-name)
  (interactive)
  ;; if on EXWM buffer.
  (if (eq exwm--frame exwm-workspace--current)
      ;; On the current workspace/not floating.
      (if (not exwm--floating-frame)
          (switch-to-buffer buffer-or-name)
        ;; Select the floating frame.
        (message "DEBUG in exwm-workspace-switch-to-buffer: is a floating frame on current workspace")
        (select-frame-set-input-focus exwm--floating-frame)
        (select-window (frame-root-window exwm--floating-frame)))
    ;; On another workspace/floating (below is always off)
    (if exwm-layout-show-all-buffers
        (exwm-workspace-move-window exwm-workspace--current
                                    exwm--id)
      (let ((window (get-buffer-window buffer-or-name exwm--frame)))
        (if window
            (set-frame-parameter exwm--frame
                                 'exwm-selected-window window)
          (message "DEBUG in exwm-workspace-switch-to-buffer: another wksp: %s" exwm--frame)
          (set-window-buffer (frame-selected-window exwm--frame)
                             buffer-or-name)))
      (exwm-workspace-switch exwm--frame))))

;; (define-key exwm-mode-map (kbd "C-c C-o")
;;   (lambda () (interactive)
;;     ;; ri/exwm--custom-previous-floating-frame
;;     (select-frame-set-input-focus ri/exwm--custom-previous-floating-frame)
;;     (select-window (frame-root-window ri/exwm--custom-previous-floating-frame))
;;     ))

;; Run when startup any window
;;  Enable (interactive) ?
(defun ri/exwm-configure-window-startup ()
  (interactive)
  (message "Window '%s' appeared!" exwm-class-name)
  (pcase exwm-class-name
    ("mpv" (exwm-floating-toggle-floating))
    ))

;; Configure windows as they're created (includes spawn in certain wkspaces)
;; For automoving to workspace, use a function.
(when ri/exwm-enabled
  (add-hook 'exwm-manage-finish-hook #'ri/exwm-configure-window-startup))

(defvar apps-num-alist '((discord   . 6)
                         (steam     . 7)
                         (spotify   . 8)
                         (librewolf . 3)
                         (firefox   . 2)
                         (keepassxc . 5))
  "Workspace index associated to application.")

(defun get-apps-num-alist-num (name) 
  (cdr (assoc name apps-num-alist)))

(defvar apps-bind-alist '((discord   . "d")
                          (steam     . "c")
                          (spotify   . "s")
                          (librewolf . "l")
                          (keepassxc . "p")
                          (firefox   . "f"))
  "Last letter of keybind for each application.")

(defvar apps-run-alist '((discord   . ri/run-discord)
                         (steam     . ri/run-steam)
                         (spotify   . ri/run-spotify)
                         (librewolf . ri/run-librewolf)
                         (keepassxc . ri/run-keepassxc)
                         (firefox   . ri/run-firefox))
  "Run command for starting")

;; only run this function (called by hook) if the variable is true.
(defvar ri/open-app-in-alist-workspace nil)
(defun ri/exwm-custom-configure-window-startup ()
  "If a window starts up and the variable is enabled, run the following"
  (interactive)
  (message "DEBUG: Entering custom-config-window-startup hook")
  (if (eq ri/open-app-in-alist-workspace t)
      ;; for KeePassXC, when you try to autofill when the database is locked,
      ;; it will pop up a new window in the current workspace, but
      ;; `exwm-workspace-move-window' will move it elsewhere... except it
      ;; doesn't... and it will fail to autoselect the floating window/frame.
      ;; So to prevent this, the below is used:
      (progn
        (unless (equal exwm-title "Unlock Database - KeePassXC")
          (cl-loop for (name . num) in apps-num-alist
                   do (when (equal exwm-class-name name)
                        (exwm-workspace-move-window
                         (get-apps-num-alist-num exwm-class-name)))))
        (setq ri/open-app-in-alist-workspace nil))))

(defun ri/run-discord ()
  (interactive)
  (setq ri/open-app-in-alist-workspace t)
  (ri/run-in-background "flatpak run com.discordapp.Discord")
  (exwm-workspace-switch-create
   (get-apps-num-alist-num 'discord)))
;;
(defun ri/run-spotify ()
  (interactive)
  (setq ri/open-app-in-alist-workspace t)
  (ri/run-in-background "flatpak run com.spotify.Client")
  (exwm-workspace-switch-create
   (get-apps-num-alist-num 'spotify)))
;;
(defun ri/run-librewolf ()
  (interactive)
  (setq ri/open-app-in-alist-workspace t)
  (ri/run-in-background "flatpak run io.gitlab.librewolf-community")
  (exwm-workspace-switch-create
   (get-apps-num-alist-num 'librewolf)))
;;
(defun ri/run-keepassxc ()
  (interactive)
  (setq ri/open-app-in-alist-workspace t)
  (ri/run-in-background "~/.bin/firejail-guix-run keepassxc")
  (exwm-workspace-switch-create
   (get-apps-num-alist-num 'keepassxc)))
;;
(defun ri/run-firefox ()
  (interactive)
  (setq ri/open-app-in-alist-workspace t)
  (ri/run-in-background "firefox")
  ;; (ri/run-in-background "~/.bin/firejail-guix-run firefox")
  (exwm-workspace-switch-create
   (get-apps-num-alist-num 'firefox)))
;;
(defun ri/run-steam ()
  (interactive)
  (setq ri/open-app-in-alist-workspace t)
  (ri/run-in-background "flatpak run com.valvesoftware.Steam")
  (exwm-workspace-switch-create
   (get-apps-num-alist-num 'steam)))

;; ------------------------------------------------------------

(when ri/exwm-enabled
  ;; binds "s-l *" to run ri/run-____ command
  (cl-loop for (app . bind-char) in apps-bind-alist
           do
           (exwm-input-set-key (kbd (concat "s-l " bind-char))
                               (cdr (assoc app apps-run-alist))))

  ;; binds "s-L *" to switch to associated workspace
  ;; (defvar ri/app-launch-input nil)
  ;; (cl-loop for (app-bind . bind-char) in apps-bind-alist
  ;;          for (app-num . num) in apps-num-alist
  ;;          do
  ;;          (exwm-input-set-key (kbd (concat "s-L " bind-char))
  ;;                              (lambda ()
  ;;                                (interactive)
  ;;                                `(exwm-workspace-switch-create ,num))))
  )

;; (exwm-input-set-key (kbd "s-L d") (lambda () (interactive) (exwm-workspace-switch-create 6)))

;; --------------------

;; run the function when a new exwm window appears
(when ri/exwm-enabled
  (add-hook 'exwm-manage-finish-hook #'ri/exwm-custom-configure-window-startup))

;; very annoying?
;; makes the cursor visible?
;; only warp when on X window buffer!
;; warps useful when on EXWM-mode with pop-ups
;; when on EXWM, enable mouse. When on emacs, disable mouse.
(use-package exwm-mff
  :config
  (exwm-mff-mode nil))
;; ^ should already be nil...

;; warp cursor to center when switch to a X window buffer.
(when ri/exwm-enabled
  (add-hook 'exwm-manage-finish-hook (lambda () (exwm-mff-warp-to-selected))))

;; set wallpaper (after xrandr so can get correct dimensions)
(defun ri/set-wallpaper ()
  (interactive)
  (let ((def-wallpaper-path (concat user-emacs-directory "exwm/oneshot-wallpaper3.jpg"))
        (home-wallpaper-path "~/.fehbg"))
    (if (file-exists-p home-wallpaper-path)
        (progn
          (start-process-shell-command
           "feh" nil (concat "sh " "~/.fehbg")))
      (progn
        (start-process-shell-command
         "feh" nil
         (concat "feh --bg-fill " def-wallpaper-path))))))

;; random wallpaper
;; (defun ri/random-wallpaper ()
;;   (interactive))

(when ri/exwm-enabled
  ;; night light
  ;; (ri/run-in-background (expand-file-name "exwm/sct-auto-adjust.sh" user-emacs-directory))
  ;; run prepare-exwm.sh
  ;; (no longer needed since .xsession works)
  ;; (ri/run-in-background (expand-file-name "exwm/prepare-exwm.sh" user-emacs-directory))
  ;; wallpaper
  (ri/set-wallpaper))

;; depends on scrot (screenshot), brightnessctl (brightness), and playerctl (player)
(use-package desktop-environment
  :after exwm
  :config
  (setq desktop-environment-update-exwm-global-keys :global)
  (unbind-key "s-l" desktop-environment-mode-map)
  (desktop-environment-mode)
  :custom
  ;; brightness
  (desktop-environment-brightness-normal-increment "10%+")
  (desktop-environment-brightness-normal-decrement "10%-")
  (desktop-environment-brightness-small-increment "5%+")
  (desktop-environment-brightness-small-decrement "5%-")
  ;; volume
  (desktop-environment-volume-normal-decrement "-10%")
  (desktop-environment-volume-normal-increment "+10%")
  (desktop-environment-volume-small-decrement "-5%")
  (desktop-environment-volume-small-increment "+5%")
  (desktop-environment-volume-set-command "sh ~/.emacs.d/exwm/pactl-increment-volume.sh %s")
  (desktop-environment-volume-get-command "sh ~/.emacs.d/exwm/pactl-print-volume-w-mute.sh")
  (desktop-environment-volume-get-regexp "\\(.*\\)")
  (desktop-environment-volume-toggle-command "pactl set-sink-mute @DEFAULT_SINK@ toggle && bash ~/.emacs.d/exwm/pactl-print-mute.sh")
  (desktop-environment-volume-toggle-regexp "\\(.*\\)" )
  ;; screenshot
  (desktop-environment-screenshot-command "flameshot gui"))

;; for floating minibuffer...
;; ; maybe have disappearing minibuffer above the modeline?
;; ; show minibuffer in separate frame?
;; ; show minibuffer on polybar? make it pop up from under polybar?
;; ; auto-hiding minibuffer at top of screen, and posframe for all else but quit.
;; ; polybar at top, print minibuffer messages, stable, bottom is only modeline

(use-package ivy-posframe
  :disabled
  :after (exwm)
  :config
  (setq ivy-posframe-height-alist '((swiper . 20)
                                    (counsel-M-x . 40)))

  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
         ;; (counsel-M-x    . ivy-posframe-display-at-window-bottom-left)
          (counsel-M-x     . ivy-display-function-fallback)
          (counsel-switch-buffer . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 0))

;; THIS DOESN'T WORK!
(provide 'ri-desktop)
