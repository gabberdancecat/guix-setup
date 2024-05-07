

(defun river-run-test (commands)
  "Run the riverctl command.
COMMANDS is a single list of arguments."
  (let* ((commands (mapcar (lambda (x)
                             (if (stringp x)
                                 ;; escape doublequote
                                 (format "%S" x)
                               ;; doublequote symbol
                               (symbol-name x)))
                           commands))
         (commands-string
          (mapconcat 'identity commands " "))
         (river-full-cmd (format "riverctl %s" commands-string))
         (river-output)
         (river-exit-code)
         (tmp))
    (cl-flet ((log-to-buffer
                '(lambda (x) (bufr-message "*river-log*" x))))
      ;; log the command being run
      (log-to-buffer (concat "> " river-full-cmd))
      (message "LOG: %s" river-full-cmd)
      ;; run command
      (message "TO RUN: %S" commands)
      (setq tmp (with-temp-buffer 
                  (list (apply 'call-process "riverctl" nil (current-buffer) nil
                               commands)
                        (buffer-string))))
      ;; log output and exit code
      (setq river-exit-code (nth 0 tmp))
      (setq river-output (nth 1 tmp))
      (log-to-buffer (concat " (" (substring river-output 0 -1) ")")))))

;; pre-order traversal down a recursive list, which is basically a tree

(defmacro river-set-test (&rest commands)
  `(progn
     ,@(let (ret)
         (setq river-set--accumilator nil)
         (river-set--convert commands nil)
         (message "DEBUG: convert results: %s" river-set--accumilator)
         ;; set ret to river-set--accumilator so can reset value
         (setq ret (mapcar (lambda (cmd)
                             `(river-run-test ,(macroexpand `(quote ,cmd))))
                           river-set--accumilator))
         (setq river-set--accumilator nil)
         ret)))

(defun river-set--convert (root traversed)
  ;; recurse
  ;; dolist through elemets of root,
  ;;  if element is a list, recurse (update root and traversed)
  ;;  if element is nlistp, accumilate (with traversed + element)
  ;; (message "recurse 1, root: %s, traversed: %s" root traversed)
  (let ((sublist-found nil))
    ;; iter through root elements
    (dolist (elem root)
      ;; (message "dolist, elem: %s" elem)
      (if (nlistp elem)
          ;; item case
          (setq traversed (append traversed (list elem)))
        ;; (message "after adding to traversed: %s" traversed))
        ;; recurse case
        (setq sublist-found t)
        (river-set--convert elem traversed)))
    ;; tail case, accumilate
    (unless sublist-found
      ;; (message "val of traversed at push: %s" traversed)
      (push traversed river-set--accumilator))))

(river-set-test (map (normal kbd ("s-Q" exit)
                             ("s-R" spawn "'~/.config/river/init-elisp"))
                     (locked (kbd ("XF86AudioMute" spawn 
                                   "pactl-vol-mute.sh")))))
