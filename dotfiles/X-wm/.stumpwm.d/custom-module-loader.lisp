  ;;; Custom Module Loader v1.0

;; To set up this module, add it using `(add-to-load-path "PATH")'
;; then load using `(load-module "ri/load-module")'.
;; By default, the *module-dir* will be set to `*confdir*/modules',
;; and set automatically with 

(defvar *symlink-modules* T
  "If T, allow loading from symlinks pointing to modules,
    as well creating new ones.")

(defvar *module-source-dirs*
  '(
    "~/.guix-extra-profiles/desktop/desktop/share/common-lisp/sbcl/"
    ;; "~/.guix-home/profile/share/common-lisp/sbcl/"
    ;; "~/.guix-profile/share/common-lisp/sbcl"
    ;; "~/.guix-profile/share/common-lisp/source"
    "~/.stumpwm.d/extra"
    "~/.stumpwm.d/contrib/")
  "A list of paths to search through and extract
    valid module load-paths from.")

(defvar *modules-to-exclude*
  '("stumpwm")
  "A list of modules to exclude from `*module-valid-load-paths*'.
    By default, 'stumpwm' is excluded, because if stumpwm:lib is
    installed via Guix in one of the dirs in
    `*module-source-dirs*', it's unnecessary")

;;--------

;; DEBUG
(defvar *DEBUG-def-module-dir* *module-dir*)
;; sets *module-dir* to ~/.stumpwm.d/modules
(when *symlink-modules*
  (set-module-dir (format nil "~A" (pathname (concat *confdir* "/" "modules")))))


(defvar *module-valid-load-paths* '()
  "A list of all valid module load-paths, excluding the 
    stumpwm:lib modules (guix)")

(defun ri/update-valid-load-paths ()
  (setf *module-valid-load-paths*
        (let ((paths (mapcan #'build-load-path *module-source-dirs*)))
          (remove-if (lambda (path)
                       (string-equal "stumpwm" (first (last (pathname-directory path)))))
                     paths))))

(ri/update-valid-load-paths)

(defvar ri/added-modules-list '()
  "A list of all module names that have been run with 
    `ri/load-module'")

(defun ri/module-symlink-ready-p (name)
  "Return T if `*symlink-modules*' is T and NAME exists
    in `*module-dir*' as a symlink."
  (and *symlink-modules*
       (directory (format nil "~A/~A" *module-dir* name))))

(defun ri/module-get-path (name)
  "Returns the absolute path to module NAME from `*module-valid-load-paths*'"
  (first (remove-if-not (lambda (path)
                          (string-equal name
                                        (first (last (pathname-directory path)))))
                        *module-valid-load-paths*)))

(defcommand ri/load-module (name &optional noload realpath) ((:string "Load module: "))
	    "A massive custom wrapper around `load-module'. When run,
    add NAME to `ri/added-modules-list' for later reference. If 
    `*symlink-modules*' is T and a symlinked module NAME exists in
    `*module-dir*', `add-to-load-path' and if `*symlink-modules*' 
    is T, create a symlink to be used next startup. Finally, 
    `load-module' if NOLOAD is non-nil"
	    (setf name (string-downcase name))
	    ;; if ready to load, skip this
	    (when-let ((path-to-add 
			(and (not (ri/module-symlink-ready-p name))
			     (ri/module-get-path (or realpath name))))) ; nil if not found
	      ;; add module load-path
	      (add-to-load-path path-to-add)
	      ;; create symlink for next time
	      (when *symlink-modules*
		(let ((symlink-to (format nil "~A" (concat *confdir* "/" "modules")))
		      (dir-namestring-path (first (last (pathname-directory path-to-add)))))
		  (format T "LOG: Creating symlink ~A...~&" name)
		  (run-shell-command (format nil "ln -s ~A ~A" path-to-add symlink-to)))))
	    ;; load-module
	    (unless noload
	      (format T "LOG: loading module ~A...~&" name)
	      (if (load-module name)
		  (when *symlink-modules* ; add to list if successful load
		    (push name ri/added-modules-list)))))

;; FIX: go through *module-dir* and run delete-symlink if not found in ri/added-modules-list
(defun ri/symlink-deletion ()
  "Delete every symlink in `*module-dir*' not found in 
    `ri/added-modules-list'"
  (when (stringp *module-dir*)
    (flet ((delete-symlink (name)
             (format T "deleting symlink ~A..." name)
             (run-shell-command
              (format nil "unlink ~A" (pathname (concat *module-dir* "/" name))))))
      (flatten (mapcar #'delete-symlink ri/added-modules-list)))))
;; delete unused symlinks when quit
;; (add-hook *quit-hook* 'ri/symlink-deletion)
