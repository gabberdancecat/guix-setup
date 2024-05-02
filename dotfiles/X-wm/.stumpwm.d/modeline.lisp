  ;;; Modeline -------

(setf *mode-line-timeout* 2)

;; format
(setf *mode-line-position* :BOTTOM) ; def :TOP
(setf *mode-line-border-width* 1) ; def 1
(setf *mode-line-pad-y* 1) ; def 1
(setf *mode-line-pad-x* 5) ; def 5

;; time
;; ($man date) 
;; %a Day :: Thu
;; %b Month :: Nov
;; %e Date# :: 09
;; %k Hour# 0-23 :: 18
;; %H Hr#00-23 :: 46
;; %M Min#
;; %F 2023-10-29
(setf *time-modeline-string* "%a %b %e %k:%M")

;; window/group format
;; (setf *window-format* "%n: %30t") ; phundrak
(setf *window-format* "%m%n%s%c") ; %s=symbols ; TODO (improve the display of windows)
;; (setf *group-format* "%t") ; def %s%n%t ; TODO (improve the display of groups)

  ;;; Load modeline modules
(ri/load-module "clx-xembed" t) ; stumptray depd
(ri/load-module "stumptray")
(ri/load-module "battery-portable")
(ri/load-module "mpd")
(ri/load-module "cpu") ; ? 
(ri/load-module "mem") ; ?

;; set module settings
(setf cpu::*cpu-modeline-fmt* "%c"
      cpu::*cpu-usage-modeline-fmt* "C: ^[~A~1D~^] "
      mem::*mem-modeline-fmt* "M: %a") ; TODO (%p get rid of % and change color (rewrite))

;; display format
;; %n group-name
;; %W all-windows-on-current-group (with *window-format*)
;; %B battery-module
;; %d time (with *time-modeline-string*)
(setf *screen-mode-line-format* (list "[^B%n^b] %W"
                                      "^>"
                                      "%M | %C | "
                                      "%B | %d %T"))
;; enable modeline
;; (put the modeline on the bottom?
(enable-mode-line (current-screen) (current-head) t)

;; enable-tray (modeline must be enabled first)
(when *mode-lines*
  (stumptray::stumptray))

;; reload tray command
(defcommand reload-tray () ()
            "Reloads stumptray"
            (stumptray:stumptray)
            (stumptray:stumptray))

  ;;; reload modeline function (note: destroys tray icons)
(defcommand reload-modeline-d () ()
            "Reloads the modeline (usually no need to do this i think...)"
            (mode-line)
            (mode-line))

;; (reload-modeline-d)

  ;;; Modeline-theme [TODO]
(print "modeline-theme")

;; (load "~/.stumpwm.d/colors.lisp")

;; (setf *mode-line-background-color* phundrak-nord1
;;       *mode-line-foreground-color* phundrak-nord5)

;; (setf *mode-line-background-color* phundrak-nord1
;;       *mode-line-foreground-color* phundrak-nord5)
