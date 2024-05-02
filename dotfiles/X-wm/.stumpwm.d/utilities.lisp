  ;;; Hacking from Emacs
;; https://github.com/c4d3s/dotfiles/blob/master/stumpwm.org
;; https://www.kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/
;; Depends on Guix 'sbcl-slynk' and Emacs 'sly'
;; In Emacs, run 'M-x sly-connect' on port 4004 to connect
(require :slynk)
(defcommand stump-slynk-server () ()
            (slynk:create-server :port 4004
                                 :dont-close t))
(stump-slynk-server)
