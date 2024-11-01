(use-modules (modules which-key))

;; which-key
(which-key-configure #:delay-idle 1.0)
(which-key-init)

(define (show-rofi-message msg)
  (hide-rofi-message)
  (display (format #f "rofi -e \"~a\"" msg))
  (system (format #f "rofi -e \"~a\"" msg)))

(define (hide-rofi-message)
  (system "pkill -f '.*rofi -e.*'"))

(define (show-which-key submap bindings)
  (format #t "Displaying Submap ~a Bindings:\n" submap)
  (let ((message ""))
    ;; printing to display (via repl/terminal)
    (for-each
     (lambda (ls)
       (let ((nmsg (format #f "    - ~a -> ~a\n" (list-ref ls 1) (list-ref ls 3))))
        (display nmsg)
        (set! message (string-append message nmsg))))
     bindings)

    ;; showing in rofi
    (show-rofi-message message)))

(define (hide-which-key submap)
  (format #t "Hiding Submap Bindings:\n")
  ;; hide your which-key viewer (rofi, eww, etc.)
  (hide-rofi-message))

;; add the display and hide hook functions
(add-hook! which-key-display-keybindings-hook show-which-key)
(add-hook! which-key-hide-keybindings-hook hide-which-key)
