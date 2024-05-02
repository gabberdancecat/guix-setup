  ;;; Window Theming
(setf *normal-border-width* 0) ; def 1 ; ?
(setf *window-border-style* :none)
(setf *ignore-wm-inc-hints* t) ; fixes emacs border

;; [[Key: %m space, %n number, %s */+/-/last, %c program, %t title ]]
(setf *window-format* "%n:%t") ; def %m%n%s%c
;; (setf *window-format* "%m%n%s%c") ; def %m%n%s%c
;; ---floating windows---
;; (setf *float-window-border* 0) ; def 1 ; ??
;; (setf *float-window-title-height* 15) ; def 10 ; ???

  ;;; Message and input windows
(setf *input-window-gravity* :top-right) ; [Eval: prompt]
(setf *message-window-gravity* :top-right) ; [message]
(setf *message-window-padding* 7) ; def 5, left message padding?
(setf *message-window-y-padding* 1) ; def 0, top message padding?

;; (defvar *groups-list* 
;;   '((0 . "other")
;;     (1 . "dev")
;;     (2 . "web")
;;     (3 . "prod")
;;     (4 . "four")
;;     (5 . "five")
;;     (6 . "six")
;;     (7 . "seven")
;;     (8 . "eight")
;;     (9 . "nine")
;;     ))

;; (defun set-default-groups ())
;;   "Goes through every group and defines"

(when *initializing*
  (grename "dev") ; 1
  (gnewbg "code") ; 2
  (gnewbg "web") ; 3
  (gnewbg "notes") ; 4
  (gnewbg "five") ; 5
  (gnewbg "social") ; 6
  (gnewbg "games") ; 7
  (gnewbg "music") ; 8
  (gnewbg "nine") ; 9
  (gnewbg "xtra") ; 0
  )

  ;;; Define window placement policy...

;; Clear rules
(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
;; (define-frame-preference "Default"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
;;   (1 t nil :class "XTerm"))

;; (define-frame-preference "Ardour"
;;   (0 t   t   :instance "ardour_editor" :type :normal)
;;   (0 t   t   :title "Ardour - Session Control")
;;   (0 nil nil :class "XTerm")
;;   (1 t   nil :type :normal)
;;   (1 t   t   :instance "ardour_mixer")
;;   (2 t   t   :instance "jvmetro")
;;   (1 t   t   :instance "qjackctl")
;;   (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

;; (define-frame-preference "Shareland"
;;   (0 t   nil :class "XTerm")
;;   (1 nil t   :class "aMule"))

;; (define-frame-preference "Emacs"
;;   (1 t t :restore "emacs-editing-dump" :title "...xdvi")
;;   (0 t t :create "emacs-dump" :class "Emacs"))
