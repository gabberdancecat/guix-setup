(ri/load-module "swm-gaps")

;; head gaps, along edges of monitor, affects modeline placement
(setf swm-gaps:*head-gaps-size* 8) ; def 0 ; rec 8
;; between windows
(setf swm-gaps:*inner-gaps-size* 4) ; def 5
;; between window border and screen border 
(setf swm-gaps:*outer-gaps-size* 10) ; def 30

;; enable
;; (swm-gaps::toggle-gaps-on)

;;; Commands

;; reload command
(defcommand reload-gaps () () 
            (swm-gaps:toggle-gaps)
            (swm-gaps:toggle-gaps))

;; (reload-gaps)
