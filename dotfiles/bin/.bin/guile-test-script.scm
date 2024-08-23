#!/usr/bin/env sh
exec guile -s "$0" "$@"
!#

;; Notes:
;; C-c C-d for geiser docs
;; C-c C-d d - guile docs at point
;; C-c C-d i - info docs
;; C-c C-d m - guile module

(use-modules (ice-9 readline))

(display (command-line))
(newline)

(let ((stdin (readline "Type something: ")))
  (display stdin)
  (newline))


;; Local Variables:
;; mode: scheme
;; End:
