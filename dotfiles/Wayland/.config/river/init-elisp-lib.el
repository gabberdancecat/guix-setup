
;; lib file for init-elisp config

(require 'cl-lib)
(require 'cl-extra)

;;; Important variables

;; (defvar river-mod "")
;; (defvar river-alt "")
;; (defvar river-mod-formal "")
;; (defvar river-alt-formal "")


;;; Helper functions


(defun bufr-message (bufname message)
  "Inserts formatted string MESSAGE into the dedicated buffer named BUFNAME."

  (with-current-buffer (get-buffer-create bufname)
    (goto-char (point-max))
    (insert (concat message "\n"))))

(defun lst-to-str-maybe (lst)
  "If LST is a list, concat all elements into a string.
Otherwise return LST without changes."
  (if (listp lst)
      (mapconcat (lambda (elem)
		   (format "%s" elem))
		 lst " ")
    lst))

(defun kbd--split-str-to-lst (kbd-str)
  "Split KBD-STR at each \"-\" unless in <arrow-brackets>, 
return as list."
  (let ((raw-lst (split-string kbd-str "-"))
	(new-lst '()))
    (while-let ((head (pop raw-lst)))
      ;; (message "DEBUG: top head: %s" head)
      (if (cl-search "<" head)
	  (progn
	    (push head new-lst)
	    (while (not (cl-search ">" head))
	      ;; (message "DEBUG: new-lst: %s, car: %s" new-lst (car new-lst))
	      (setq head (pop raw-lst))
	      (setcar new-lst (concat (car new-lst)
				      "-"
				      head))
	      ;; (message "DEBUG: new-lst post %s" new-lst)
	      ))
	(push head new-lst)))
    (reverse new-lst)))

(defun kbd--key-conv-modifier (c)
  "Return the river-format modifier key corresponding to kbd-form C.
If match not found, error."
  (pcase c
    ("s" "Super")
    ("M" "Alt")
    ("S" "Shift")
    ("C" "Control")
    (_ (error "invalid modifier key: %s" c))))

(defun kbd--key-conv-single (c)
  "Return the river-format non-modifier key corresponding to kbd-form C.
If not found, return C."
  (pcase c
    ("<return>" "Return")
    ("RET" "Return")
    ("SPC" "Space")
    ("<left>" "Left")
    ("<right>" "Right")
    ("<up>" "Up")
    ("<down>" "Down")
    ("<mouse-left>" "BTN_LEFT")
    ("<mouse-right>" "BTN_RIGHT")
    ("<mouse-up>" "BTN_UP")
    ("<mouse-down>" "BTN_DOWN")
    ("<XF86AudioRaiseVolume>" "XF86AudioRaiseVolume")
    ("<XF86AudioLowerVolume>" "XF86AudioLowerVolume")
    ("<XF86AudioMute>" "XF86AudioMute")
    ("<XF86MonBrightnessUp>" "XF86MonBrightnessUp")
    ("<XF86MonBrightnessDown>" "XF86MonBrightnessDown")
    ("<XF86AudioPlay>" "XF86AudioPlay")
    ("`" "grave")
    ;; is not a special key, use 'c' without changing
    (_ c)))

(defun kbd-to-shell (kbd)
  "Convert kbd-format keybind KBD to riverctl format and return."
  ;; error if river-mod or river-alt are undefined
  ;; (when (or (string-empty-p 'river-mod)
  ;;           (string-empty-p 'river-alt))
  ;;   (error "river-mod or river-alt is not defined"))
  ;; split KBD by each "-" and process each one
  (let* ((kbd-lst (kbd--split-str-to-lst kbd))
	 (kbd-ele nil)
	 (kbd-orig-length (length kbd-lst))
	 (output ""))
    ;; go through kbd-lst, pop off first elems and concat processed keys
    ;; to output
    (while kbd-lst
      ;; pop off first elem
      (setq kbd-ele (pop kbd-lst))
      ;; if not last elem, is a modifier, else is a single key
      (if kbd-lst
	  (setq output (concat output
			       (kbd--key-conv-modifier kbd-ele)
			       (if (> (length kbd-lst) 1)
				   "+"
				 " ")))
	(setq output (concat output
			     (when (= kbd-orig-length 1)
			       "None ")
			     (kbd--key-conv-single kbd-ele)))))
    output))


;;; Main functions

(defun shell-run (command)
  "Simlpy run a shell command."
  (message "LOG: %s" command)
  ;; (start-process-shell-command "shell cmd for river"
  ;;       		       nil
  ;;       		       (format "%s" command))
  (shell-command (format "%s" command))
  ;; t
  )

(defun river-run (command)
  "Run riverctl command."
  (let ((river-full-cmd (format "riverctl %s" command))
        (river-output)
        (river-exit-code)
        (tmp))
    ;; log command being run
    (message "LOG: " river-full-cmd)
    (bufr-message "*river-log*" (concat "> " river-full-cmd))
    ;; run command and log output and exit code
    ;; (setq river-cmd-out (shell-command-to-string river-cmd))
    (setq tmp (with-temp-buffer 
                (list (apply 'call-process "riverctl" nil (current-buffer) nil
                             (split-string (format "%s" command) "[[:space:]]"))
                      (buffer-string))))
    (message "err: %s" tmp)
    (setq river-exit-code (nth 0 tmp))
    (setq river-output (nth 1 tmp))
    ;; if exit code 
    (bufr-message "*river-log*"
                  (concat " (" (substring river-output 0 -1) ")"))))

(defun river-set-modifier (var visible formal)
  "Updates modifier key name in output config.
VAR is variable to be changed, like `river-mod'.
VISIBLE is variable to be used in the output config, like \"$mod\".
FORMAL is the formal name for the key, like \"Mod4\"."
  (cond ((string-empty-p var)
	 (error (format "%s is not set" var)))
	((not (symbolp var))
	 (error (format "%s is not a symbol" var)))
	((not (stringp visible))
	 (format "%s is not a string" visible))
	((not (stringp formal))
	 (format "%s is not a string" formal))
	((not (equal "$" (substring visible 0 1)))
	 (format "first char of %s needs to be \"$\"" visible)))
  ;; e.g. (setq river-mod "$mod")
  (set var visible)
  ;; e.g. (setq river-mod-formal "Mod4")
  (set (intern (concat (symbol-name var)
		       "-formal"))
       formal))

(defun river-run-commands (&rest commands)
  "Run every elem in list COMMANDS with \"riverctl\"."
  (mapcar #'river-run commands))

(defmacro river-pairs (&rest setting-pairs)
  "Run \"riverctl\" with every two elems of list SETTING-PAIRS.
If there is one leftover elem, it will be passed to `river-run' without a
second arg."
  `(progn
     ,@(cl-loop for (key val) on setting-pairs by #'cddr collect
		(progn
		  `(river-run ,(format "%s %s"
				       (lst-to-str-maybe key)
				       (lst-to-str-maybe val)))))))

(defmacro river-spawn (&rest command)
  "Run \"riverctl spawn\" with every elem of list COMMAND."
  `(progn
     ,@(mapcar (lambda (cmd)
		 `(river-run ,(format "spawn \"%s\"" cmd)))
	       command)))

(defvar river-set--temp-list nil
  "Variable utilized in `river-set--process' to build command.")

(defun river-set--process (body new)
  "Process the setting of keybinds for `river-set'
Recursively go down BODY, pushing results to `river-set--temp-list'."
  (when-let ((fst (if body (pop body))))
    ;; (message "DEBUG: fst: %s, new: %s, body: %s" fst new body)
    (if (listp fst)
	(river-set--process fst new)
      ;; fst is an atom
      (progn
	;; convert kbd into shell fmt (will skip if stringp nil)
	(when (and (stringp fst)
		   (equal 'kbd (car new)))
          (setq fst (intern (kbd-to-shell fst)))
	  ;; remove "kbd" from list
	  (setq new (cdr new)))
	;; escape strings
	(if (stringp fst)
	    (setq fst (prin1-to-string fst)))
	(push fst new)
	(unless body
	  (push (lst-to-str-maybe (reverse new)) river-set--temp-list))))
    (river-set--process body new))
  ;; reverse list after complete
  ;; (nreverse river-set--temp-list)
  (setq river-set--temp-list (reverse river-set--temp-list)))

(defmacro river-set (&rest commands)
  "Constructs and runs commands from all nested arguments.
For example:
(river-set (map (normal (kbd (\"s-Q\" exit)
                             (\"s-R\" spawn \"~/.config/river/init-elisp\")
                (locked (kbd (\"XF86AudioMute spawn 
                                              \"pactl-vol-mute.sh\")))))))"
  `(progn
     ,@(let ((ret nil))
	 ;; (nreverse commands)
	 ;; (message "DEBUG: river-set commands: %s" commands)
	 (setq river-set--temp-list nil)
	 (river-set--process commands nil)
	 (setq ret
	       (mapcar (lambda (cmd)
			 `(river-run ,(format "%s" cmd)))
		       (reverse river-set--temp-list)))
	 (setq river-set--temp-list nil)
	 ret)))

;; wrappers around river-set:

(defmacro river-map (&rest commands)
  ;; (declare (indent 0))
  (macroexpand `(river-set ,(cons 'normal commands))))

;; TODO: testing!!!
(cl-defmacro river-map (&rest commands)
  ;; (declare (indent 0))
  (macroexpand `(river-set ,(cons 'normal commands))))

(defmacro river-normal (&rest commands)
  (macroexpand `(river-set (map ,(cons 'normal commands)))))

(defmacro river-n-bind (&rest commands)
  (macroexpand `(river-set (map (normal ,(cons 'kbd commands))))))

(defmacro river-pointer-n-bind (&rest commands)
  (macroexpand `(river-set (map-pointer (normal ,(cons 'kbd commands))))))

;; keymaps

(defmacro river-declare-keymap (keymap &rest commands)
  (macroexpand `(river-set declare-mode ,keymap))
  `(progn
     ,(macroexpand `(river-set (map (,keymap ,(cons 'kbd commands)))))
     ,(macroexpand `(river-set (map (-release
				     ,keymap ,(cons 'kbd
						    (mapcar (lambda (x)
							      (list (car x)
								    'enter-mode
								    'normal))
							    commands))))))))

(defmacro river-n-bind-keymap (&rest commands)
  (macroexpand
   `(river-set (map (normal ,(cons 'kbd
                                   (mapcar (lambda (x)
                                             (list (car x)
                                                   'enter-mode
                                                   (cdr x)))
                                           commands)))))))

