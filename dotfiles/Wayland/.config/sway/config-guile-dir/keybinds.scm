(use-modules (modules kbd)
             (modules general)
             (swayipc)
             (ice-9 popen) ; threads
	     (srfi srfi-13) ; string-join
             )

;; Note: keyboard layout set in main config

(define (exec command)
  "Execute the given shell command"
  (format #t "running: ~a\n" command)
  (thread-start! (make-thread (lambda () (system command)))))

(define (keybindings-init)
  (kbd-init)
  (general-configure #:keybinding-translator kbd-translate)
  (general-init)

  (exec "swaymsg bindsym Mod4+Tab workspace back_and_forth")
  (exec "swaymsg bindsym Mod4+Alt+h workspace prev")
  (exec "swaymsg bindsym Mod4+Alt+s workspace next")

  ;; define root keybindings
  (general-define-keys
   ;; system
   `("s-C-S-q" (sway-exit) #:wk "Quit sway")
   `("s-F12" (exec "loginctl hibernate") #:wk "Hibernate")
   `("s-C-S-r" (sway-reload) #:wk "Reload sway")
   `("s-C-S-l" (exec "swaylock") #:wk "Lock sway")
   
   ;; basic programs
   `("s-RET" (exec "alacritty") #:wk "Alacritty")
   `("s-c" (exec "emacsclient -a '' -c -e '(dashboard-open)'") #:wk "Emacsclient")
   
   ;; windows
   `("s-M-d" (sway-kill) #:wk "Kill Window")
   `("s-f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE) #:wk "Toggle Fullscreen")
   
   ;; windows - focus
   `("s-h" (sway-focus-container SWAY-DIRECTION-LEFT) #:wk "Focus Container Left")
   `("s-t" (sway-focus-container SWAY-DIRECTION-UP) #:wk "Focus Container Up")
   `("s-n" (sway-focus-container SWAY-DIRECTION-DOWN) #:wk "Focus Container Down")
   `("s-s" (sway-focus-container SWAY-DIRECTION-RIGHT) #:wk "Focus Container Right")
   `("s-w" (sway-focus-container-tiling) #:wk "Focus Container Last")
   
   ;; windows - move
   `("s-S-h" (sway-move-container SWAY-DIRECTION-LEFT) #:wk "Move Container Left")
   `("s-S-t" (sway-move-container SWAY-DIRECTION-UP) #:wk "Move Container Up")
   `("s-S-n" (sway-move-container SWAY-DIRECTION-DOWN) #:wk "Move Container Down")
   `("s-S-s" (sway-move-container SWAY-DIRECTION-RIGHT) #:wk "Move Container Right")
   
   ;; windows - cycle
   `("s-v" (sway-focus-container-sibling SWAY-SIBLING-NEXT) #:wk "Cycle Tabs Next")
   `("s-w" (sway-focus-container-sibling SWAY-SIBLING-PREV) #:wk "Cycle Tabs Previous")
   
   ;; windows - focus last window
   ;; TODO

   ;; windows - move to workspace
   `("s-S-M-h" (workspace-grid-move-container-to-workspace-left) #:wk "Move Container to Workspace Left")
   `("s-S-M-t" (workspace-grid-move-container-to-workspace-up) #:wk "Move Container to Workspace Up")
   `("s-S-M-n" (workspace-grid-move-container-to-workspace-down) #:wk "Move Container to Workspace Down")
   `("s-S-M-s" (workspace-grid-move-container-to-workspace-right) #:wk "Move Container to Workspace Right")
   
   ;; workspaces
   ;; `,@(map (lambda (sym num)
   ;; 	     (list (string-append sym "-" num)
   ;; 		   `(sway-switch-workspace ,num)))
   ;; 	   '("&" "[" "{" "}" "(" "=" "*" ")" "+" "]")
   ;; 	   '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

   `("s-&" (sway-switch-workspace "1"))
   `("s-[" (sway-switch-workspace "2"))
   `("s-{" (sway-switch-workspace "3"))
   `("s-}" (sway-switch-workspace "4"))
   `("s-(" (sway-switch-workspace "5"))
   `("s-=" (sway-switch-workspace "6"))
   `("s-*" (sway-switch-workspace "7"))
   `("s-)" (sway-switch-workspace "8"))
   `("s-+" (sway-switch-workspace "9"))
   `("s-]" (sway-switch-workspace "0"))
   
   `("s-S-&" (sway-move-container-to-workspace "1"))
   `("s-S-[" (sway-move-container-to-workspace "2"))
   `("s-S-{" (sway-move-container-to-workspace "3"))
   `("s-S-}" (sway-move-container-to-workspace "4"))
   `("s-S-(" (sway-move-container-to-workspace "5"))
   `("s-S-=" (sway-move-container-to-workspace "6"))
   `("s-S-*" (sway-move-container-to-workspace "7"))
   `("s-S-)" (sway-move-container-to-workspace "8"))
   `("s-S-+" (sway-move-container-to-workspace "9"))
   `("s-S-]" (sway-move-container-to-workspace "0"))
   
   ;; function-keys - media
   `("XF86AudioLowerVolume" (exec "volume-adjust.sh -10%") #:wk "Decrease Volume")
   `("XF86AudioRaiseVolume" (exec "volume-adjust.sh +10%") #:wk "Increase Volume")
   `("S-XF86AudioLowerVolume" (exec "volume-adjust.sh -5%") #:wk "Decrease Volume slightly")
   `("S-XF86AudioRaiseVolume" (exec "volume-adjust.sh +5%") #:wk "Increase Volume slightly")
   `("XF86AudioMute" (exec "pactl set-sink-mute @DEFAULT_SINK@ toggle") #:wk "Toggle Mute")
   `("XF86AudioPlay" (exec "playerctl play-pause") #:wk "Toggle Player")
   
   ;; function-keys - brightness
   `("XF86MonBrightnessUp" (exec "brightnessctl set +10%") #:wk "Increase Brightness")
   `("XF86MonBrightnessDown" (exec "brightnessctl set 10%-") #:wk "Decrease Brightness")
   `("S-XF86MonBrightnessUp" (exec "brightnessctl set +5%") #:wk "Increase Brightness slightly")
   `("S-XF86MonBrightnessDown" (exec "brightnessctl set 5%-") #:wk "Decrease Brightness slightly")
   
   ;; screenshots

   ;; passthrough

   ;; ratio
   ;; modes
   ;; outputs
   ;; window rules
   ;; steam
   ;; floating
   ;; workspaces (next,prev,last)
   ;; startup programs
   )
  
  ;; define leader keymap
  (general-define-keys
   #:prefix "s-g" #:wk "[Leader]"
   `("n" (exec "$HOME/code/in-use/rofi-network-manager/rofi-network-manager.sh") #:wk "NetworkManager-tui")
   `("," (exec "playerctl previous") #:wk "Previous track")
   `("." (exec "playerctl previous") #:wk "Next track")
   `("s" (exec "text-to-speech-clipboard.sh") #:wk "Text-to-Speech clipboard")
   `(general-define-keys
     #:prefix "a" #:wk "[Applications]"
     ("f" (exec "firefox") #:wk "Firefox")
     ("l" (exec "flatpak run io.gitlab.librewolf-community") #:wk "Librewolf")
     ("p" (exec "firejail keepassxc") #:wk "KeepassXC")
     ("d" (exec "vesktop") #:wk "Vesktop")
     ("s" (exec "flatpak run com.spotify.Client") #:wk "Spotify")
     ("c" (exec "steam") #:wk "Steam")
     ("r" (exec "pw-jack renoise --scripting-dev") #:wk "Renoise")
     )
   `(general-define-keys
     #:prefix "w" #:wk "[Window]"
     ("v" (sway-layout SWAY-LAYOUT-SPLITV) #:wk "Split Vertically")
     ("h" (sway-layout SWAY-LAYOUT-SPLITH) #:wk "Split Horizontally")
     ("f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE) #:wk "Fullscreen")
     ("d" (sway-layout SWAY-LAYOUT-DEFAULT) #:wk "Default Layout")
     ("t" (sway-layout SWAY-LAYOUT-TABBED) #:wk "Tabbed Layout"))
   ))

;; eval
(keybindings-init)
  
