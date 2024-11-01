(use-modules (modules which-key))

;; which-key
(which-key-configure #:delay-idle 0.5)
(which-key-init)

(define (show-rofi-message msg)
  (hide-rofi-message)
  (display (format #f "rofi -e \"~a\"" msg))
  (system (format #f "rofi -e \"~a\"" msg)))

(define (hide-rofi-message)
  (system "pkill -f '.*rofi -e.*'"))

(define (sort-pairs-determine-precedence pair)
  (let ((key (car pair))
        (title (cadr pair)))
    (cond
      ((string=? title "Escape") 0)  ; Highest precedence
      ((string-prefix? "[" title) 1)  ; Second precedence
      ((> (string-length key) 1) 2)  ; Third precedence for multi-character keys
      (else 3))))                    ; Lowest precedence

(define (sort-pairs pairs)
  (sort pairs
        (lambda (pair1 pair2)
          (let ((prec1 (sort-pairs-determine-precedence pair1))
                (prec2 (sort-pairs-determine-precedence pair2)))
            (if (= prec1 prec2)
                (string<? (cadr pair1) (cadr pair2))
                (< prec1 prec2))))))

(define (show-which-key submap bindings)
  (format #t "Displaying Submap ~a Bindings:\n" submap)
  (let ((message "")
        (pairs (sort-pairs (map
                            (lambda (ls)
                              (list (list-ref ls 1) (list-ref ls 3)))
                            bindings))))
    ;; printing to display (via repl/terminal)
    (for-each
     (lambda (pair)
       (let ((nmsg (format #f "    - ~a -> ~a\n" (list-ref pair 0) (list-ref pair 1))))
         (display nmsg)
         (set! message (string-append message nmsg))))
     pairs)

    ;; showing in rofi
    (show-rofi-message message)))

(define (hide-which-key submap)
  (format #t "Hiding Submap Bindings:\n")
  ;; hide your which-key viewer (rofi, eww, etc.)
  (hide-rofi-message))

;; add the display and hide hook functions
(add-hook! which-key-display-keybindings-hook show-which-key)
(add-hook! which-key-hide-keybindings-hook hide-which-key)
