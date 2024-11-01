
(use-modules (ice-9 popen) ; threads
	     )

;; provide useful functions

(define (exec command)
  "Execute the given shell command"
  (format #t "running: ~a\n" command)
  (thread-start! (make-thread (lambda () (system command)))))
