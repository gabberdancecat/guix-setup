(use-modules (modules kbd)
             (modules general)
             (swayipc)
	     (srfi srfi-13) ; string-join
             )

;; Note: keyboard layout set in main config

(load "Functions.scm") ; exec

;; init kbd and general modules
(kbd-init)
(general-configure #:keybinding-translator kbd-translate)
(general-init)

;; Main:

;; define root keybindings
(apply
 general-define-keys
 `(("s-C-S-q" (sway-exit)                 #:wk "Quit sway")
   ("s-F12"   (exec "loginctl hibernate") #:wk "Hibernate")
   ("s-C-S-r" (sway-reload)               #:wk "Reload sway")
   ("s-C-S-l" (exec "swaylock")           #:wk "Lock sway")
   
   ;; basic programs
   ("s-RET" (exec "alacritty")            #:wk "Alacritty")
   ("s-c"   (exec "emacsclient -c -a ''") #:wk "Emacsclient")

   ;; launchers
   ("s-a"   (exec ,(string-append
                    "fuzzel -w 50 -x 8 -y 8 -r 3 -b 232635ff -t A6Accdff"
                    " -s A6Accdff -S 232635ff -C c792eacc -m c792eacc"
                    " -f 'Iosevka Aile:weight=light:size=11'"
                    " --icon-theme='Papirus-Dark'"))    #:wk "fuzzel menu")
   ("s-S-a" (exec "bemenu-run")                         #:wk "bemenu")
   
   ;; windows
   ("s-M-d" (sway-kill)                                 #:wk "Kill Window")
   ("s-f"   (sway-fullscreen SWAY-FULLSCREEN-TOGGLE)    #:wk "Toggle Fullscreen")
   
   ;; windows - focus
   ;; ("s-h" (sway-focus-container SWAY-DIRECTION-LEFT)    #:wk "Focus Container Left")
   ;; ("s-t" (sway-focus-container SWAY-DIRECTION-UP)      #:wk "Focus Container Up")
   ;; ("s-n" (sway-focus-container SWAY-DIRECTION-DOWN)    #:wk "Focus Container Down")
   ;; ("s-s" (sway-focus-container SWAY-DIRECTION-RIGHT)   #:wk "Focus Container Right")
   ;; ("s-w" (sway-focus-container-tiling)                 #:wk "Focus Container Last")

   ;; windows - move
   ("s-M-h" (sway-move-container SWAY-DIRECTION-LEFT) #:wk "Move Container Left")
   ("s-M-t" (sway-move-container SWAY-DIRECTION-UP) #:wk "Move Container Up")
   ("s-M-n" (sway-move-container SWAY-DIRECTION-DOWN) #:wk "Move Container Down")
   ("s-M-s" (sway-move-container SWAY-DIRECTION-RIGHT) #:wk "Move Container Right")
   
   ;; windows - cycle
   ;; ("s-v" (sway-focus-container-sibling SWAY-SIBLING-NEXT) #:wk "Cycle Tabs Next")
   ;; ("s-w" (sway-focus-container-sibling SWAY-SIBLING-PREV) #:wk "Cycle Tabs Previous")

   ;; windows - focus (tiling)
   ("s-n" (exec "sway-focus-window-adj.py next") #:wk "Cycle Tabs Next")
   ("s-t" (exec "sway-focus-window-adj.py prev") #:wk "Cycle Tabs Prev")

   ;; windows - swap (tiling)
   ("s-M-n" (exec "sway-swap-window-adj.py next") #:wk "Cycle Tabs Next")
   ("s-M-t" (exec "sway-swap-window-adj.py prev") #:wk "Cycle Tabs Prev")

   ("s-M-RET" (exec "i3a-swap"))
   
   ;; windows - focus last window
   ;; TODO

   ;; ;; windows - move to workspace
   ;; ("s-S-M-h" (workspace-grid-move-container-to-workspace-left) #:wk "Move Container to Workspace Left")
   ;; ("s-S-M-t" (workspace-grid-move-container-to-workspace-up) #:wk "Move Container to Workspace Up")
   ;; ("s-S-M-n" (workspace-grid-move-container-to-workspace-down) #:wk "Move Container to Workspace Down")
   ;; ("s-S-M-s" (workspace-grid-move-container-to-workspace-right) #:wk "Move Container to Workspace Right")

   ("s-C-h" (sway-resize SWAY-RESIZE-TYPE-GROW   SWAY-RESIZE-DIRECTION-WIDTH 50 #:unit SWAY-SIZE-UNIT-PX))
   ("s-C-t" (sway-resize SWAY-RESIZE-TYPE-SHRINK   SWAY-RESIZE-DIRECTION-HEIGHT 50 #:unit SWAY-SIZE-UNIT-PX))
   ("s-C-n" (sway-resize SWAY-RESIZE-TYPE-GROW SWAY-RESIZE-DIRECTION-HEIGHT 50 #:unit SWAY-SIZE-UNIT-PX))
   ("s-C-s" (sway-resize SWAY-RESIZE-TYPE-SHRINK SWAY-RESIZE-DIRECTION-WIDTH 50 #:unit SWAY-SIZE-UNIT-PX))
   
   ;; workspaces

   ("s-Tab" (exec "swaymsg workspace back_and_forth") #:wk "Workspace back_and_forth") ;; TODO
   ("s-M-c" (exec "swaymsg workspace prev_on_output") #:wk "Workspace prev") ;; TODO
   ("s-M-r" (exec "swaymsg workspace next_on_output") #:wk "Workspace next") ;; TODO
   
   ,@(map (lambda (sym num)
	    (list (string-append "s-" sym)
		  `(sway-switch-workspace ,num)))
	  '("&" "[" "{" "}" "(" "=" "*" ")" "+" "]")
	  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

   ,@(map (lambda (sym num)
	    (list (string-append "s-S-" sym)
		  `(sway-move-container-to-workspace ,num)))
	  '("&" "[" "{" "}" "(" "=" "*" ")" "+" "]")
	  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

   ;; function-keys - media
   ("XF86AudioLowerVolume" (exec "volume-adjust.sh -10%") #:wk "Decrease Volume")
   ("XF86AudioRaiseVolume" (exec "volume-adjust.sh +10%") #:wk "Increase Volume")
   ("S-XF86AudioLowerVolume" (exec "volume-adjust.sh -5%") #:wk "Decrease Volume slightly")
   ("S-XF86AudioRaiseVolume" (exec "volume-adjust.sh +5%") #:wk "Increase Volume slightly")
   ("XF86AudioMute" (exec "pactl set-sink-mute @DEFAULT_SINK@ toggle") #:wk "Toggle Mute")
   ("XF86AudioPlay" (exec "playerctl play-pause") #:wk "Toggle Player")
   
   ;; function-keys - brightness
   ("XF86MonBrightnessUp" (exec "brightnessctl set +10%") #:wk "Increase Brightness")
   ("XF86MonBrightnessDown" (exec "brightnessctl set 10%-") #:wk "Decrease Brightness")
   ("S-XF86MonBrightnessUp" (exec "brightnessctl set +5%") #:wk "Increase Brightness slightly")
   ("S-XF86MonBrightnessDown" (exec "brightnessctl set 5%-") #:wk "Decrease Brightness slightly")

   ;; special
   ("Print" (exec "grimshot --notify save area ~/Pictures/screenshots/$(date -Iseconds).png"))
   ("S-Print" (exec "grimshot --notify save output ~/Pictures/screenshots/$(date -Iseconds).png"))
   ("s-S-Print" (exec "grimshot --notify copy area"))
   
   ))

;;  ;; screenshots

;;  ;; passthrough

;;  ;; ratio
;;  ;; modes
;;  ;; outputs
;;  ;; window rules
;;  ;; steam
;;  ;; floating
;;  ;; workspaces (next,prev,last)
;;  ;; startup programs
;;  )

;; define leader keymap
(apply
 general-define-keys
 `(#:prefix "s-g" #:wk "[Leader]"
   ("n" (exec "$HOME/code/in-use/rofi-network-manager/rofi-network-manager.sh") #:wk "NetworkManager-tui")
   ("," (exec "playerctl previous") #:wk "Previous track")
   ("." (exec "playerctl previous") #:wk "Next track")
   ("s" (exec "text-to-speech-clipboard.sh") #:wk "Text-to-Speech clipboard")
   ("S-s" (exec "killall espeak-ng") #:wk "stop Text-to-Speech")
   (general-define-keys
    #:prefix "a" #:wk "[Applications]"
    ("f" (exec "firefox") #:wk "Firefox")
    ("l" (exec "flatpak run io.gitlab.librewolf-community") #:wk "Librewolf")
    ("p" (exec "firejail keepassxc") #:wk "KeepassXC")
    ("d" (exec "vesktop") #:wk "Vesktop")
    ("s" (exec "flatpak run com.spotify.Client") #:wk "Spotify")
    ("c" (exec "steam") #:wk "Steam")
    ("r" (exec "pw-jack renoise --scripting-dev") #:wk "Renoise")
    ("n" (exec "newmacs.sh") #:wk "newmacs"))
   (general-define-keys
    #:prefix "w" #:wk "[Window]"
    ("v" (sway-layout SWAY-LAYOUT-SPLITV) #:wk "Split Vertically")
    ("h" (sway-layout SWAY-LAYOUT-SPLITH) #:wk "Split Horizontally")
    ("f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE) #:wk "Fullscreen")
    ("d" (sway-layout SWAY-LAYOUT-DEFAULT) #:wk "Default Layout")
    ("t" (sway-layout SWAY-LAYOUT-TABBED) #:wk "Tabbed Layout"))))

(apply
 general-define-keys
 `(#:prefix "s-w" #:wk "[Windows]"
   ("h" (sway-move-container SWAY-DIRECTION-LEFT)
    #:wk "Move Container Left")
   ("t" (sway-move-container SWAY-DIRECTION-UP)
    #:wk "Move Container Up")
   ("n" (sway-move-container SWAY-DIRECTION-DOWN)
    #:wk "Move Container Down")
   ("s" (sway-move-container SWAY-DIRECTION-RIGHT)
    #:wk "Move Container Right")))

(apply
 general-define-keys
 `(#:prefix "s-r" #:wk "[Resize]"
   ("h" (sway-resize SWAY-RESIZE-TYPE-GROW   SWAY-RESIZE-DIRECTION-WIDTH  50 #:unit SWAY-SIZE-UNIT-PX) #:wk "grow horizontally")
   ("t" (sway-resize SWAY-RESIZE-TYPE-GROW   SWAY-RESIZE-DIRECTION-HEIGHT 50 #:unit SWAY-SIZE-UNIT-PX) #:wk "grow vertically")
   ("n" (sway-resize SWAY-RESIZE-TYPE-SHRINK SWAY-RESIZE-DIRECTION-HEIGHT 50 #:unit SWAY-SIZE-UNIT-PX) #:wk "shrink vertically")
   ("s" (sway-resize SWAY-RESIZE-TYPE-SHRINK SWAY-RESIZE-DIRECTION-WIDTH  50 #:unit SWAY-SIZE-UNIT-PX) #:wk "shrink horizontally")
   ))
