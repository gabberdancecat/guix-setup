#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

;;
;; Cli program for CTF stuff
;;

(use-modules (srfi srfi-1) ; first, rest
	     (ice-9 getopt-long)
	     (ice-9 match)
	     (ice-9 popen)
	     (ice-9 textual-ports) ; for get-string-all
	     (ice-9 rdelim)) ; for read-line

(define (car-safe lst)
  (if (pair? lst)
      (car lst)
      #f))

(define (main raw-args)
  (if (< (length (cdr raw-args)) 1)
      (error "Need at least one argument."))
  (let* ((script-path (car raw-args))
	 (args  (cdr raw-args))
	 (first (car args))
	 (rest  (cdr args)))
    (match first
      ((or "f" "finish") (action-finish rest))
      ((or "s" "save") (action-save rest))
      (_ (error "No matching action:" first)))))

(define (action-finish args)
  (let* ((orig-dir (getcwd))
	 (parent-dir (dirname orig-dir))
	 (new-parent-dir (string-append orig-dir "_COMPLETED")))
    (chdir parent-dir)
    (rename-file orig-dir new-parent-dir)))

(define (action-save args)
  (system* "fzf" "one two three!")
  (let* ((shell (getenv "SHELL"))
	 (history-file (match shell
			 ("bash" ".*.bash_history")
			 ("zsh" ".*.zsh_history")))
	 (history-cmd (string-append "tail -n 1 $HOME/" history-file))
	 (flag-cmd (call-with-port (open-input-pipe history-cmd)
		     (lambda (port)
		       (read-line pipe))))
	 (outfile-text (string-append "#!/bin/sh\n\n" flag-cmd))
	 (outfile-name "get_flag.sh")
	 (outfile-path (string-append (getcwd) "/" outfile-name)))
    (display (string-append "> CWD: " (getcwd)))
    (write-to-file outfile-path outfile-text)
    (display "> contents of get_flag.sh:")
    (system* "cat" outfile-path)
    (chmod outfile-path #o755)
    (system (string-append outfile-path > flag.txt))))

;; run cmd and get last cmd name, create new file, chmod, run and redirect to another
;; (define (cmd-save)
;;   (let* ((run-cmd "tail -n 1 $HOME/.bash_history")
;; 	 (call-with-port (open-input-pipe run-cmd)
;; 	   (lambda (port)
;; 	     ()))))

;;   (let* ((runcmd "tail -n 1 $HOME/.bash_history")
;; 	 (port   (open-input-pipe runcmd))
;; 	 (last   (read-line port))
;; 	 ())
;;     (close-pipe port)
;;     (display str) (newline)))

;; Local Variables:
;; mode: scheme
;; End:
