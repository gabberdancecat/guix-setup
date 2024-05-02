;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes (measured in bytes)
(setq gc-cons-threshold (* 50 1000 1000))

(defun ri/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ri/display-startup-time)

;; after emacs is loaded
(add-hook 'after-init-hook (lambda ()
                             ;; Make gc pauses faster by decreasing the threshold.
                             (setq gc-cons-threshold (* 2 1000 1000))))

;; native comp
(setq native-comp-async-report-warnings-errors 'nil)

;; Set the right directory to store the native comp cache (already enabled?)
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; (require 'subr-x) ; TEST if necessary
(setq ri/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(setq ri/is-thinkpad (and (eq system-type 'gnu/linux)
                          (equal (system-name) "thinkpad1")))

;; use shell-command w/ grep to find "Guix System" in /etc/*release
(setq ri/is-guix-system (and (eq system-type 'gnu/linux)
  			   (or (equal (system-name) "GNUwU")
  			       (equal (system-name) "gnuwu"))))

(setq ri/has-guix-installed (and (eq system-type 'gnu/linux)
                                 (executable-find "guix")))

;; (require 'f)
;; (string-equal (f-read "/etc/issue")
;; "\nThis is the GNU system.  Welcome.\n")))

(unless (featurep 'straight)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(straight-use-package 'use-package) ; use-package calls will be sent to use-package
(setq straight-use-package-by-default t) ; no need to put :straight

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;; (package-install 'setup) ; no longer using package.el (using straight.el)
(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

;; Uncomment this for debugging purposes
;; (defun dw/log-require (&rest args)
;;   (with-current-buffer (get-buffer-create "*require-log*")
;;     (insert (format "%s\n"
;;                     (file-name-nondirectory (car args))))))
;; (add-to-list 'after-load-functions #'dw/log-require)

;; Recipe is always a list
;; Install via Guix if length == 1 or :guix t is present

(defvar ri/guix-emacs-packages '()
  "Contains a list of all Emacs package names that must be
installed via Guix.")

;; Examples:
;; - (org-roam :straight t)
;; - (git-gutter :straight git-gutter-fringe)

(defun ri/filter-straight-recipe (recipe)
  (let* ((plist (cdr recipe))
         (name (plist-get plist :straight)))
    (cons (if (and name (not (equal name t)))
              name
            (car recipe))
          (plist-put plist :straight nil))))

(setup-define :pkg
  (lambda (&rest recipe)
    (if (and ri/is-guix-system	; modified, but will this break?
             nil ; DISABLE
             (or (eq (length recipe) 1)
                 (plist-get (cdr recipe) :guix)))
        ;; if ri/is-guix-system and regular input, install w/ guix.
        (progn `(add-to-list 'ri/guix-emacs-packages
                             ,(or (plist-get recipe :guix)
                                  (concat "emacs-" (symbol-name (car recipe))))))
      ;; else, install directly with straight.el
      `(straight-use-package ',(ri/filter-straight-recipe recipe))))
  :documentation "Install RECIPE via Guix or straight.el"
  :shorthand #'cadr)

(setup-define :delay
  (lambda (&rest time)
    `(run-with-idle-timer ,(or time 1)
                          nil ;; Don't repeat
                          (lambda () (require ',(setup-get 'feature)))))
  :documentation "Delay loading the feature until a certain amount of idle time has passed.")

(setup-define :disabled
  (lambda ()
    `,(setup-quit))
  :documentation "Always stop evaluating the body.")

(setup-define :load-after
  (lambda (features &rest body)
    (let ((body `(progn
                   (require ',(setup-get 'feature))
                   ,@body)))
      (dolist (feature (if (listp features)
                           (nreverse features)
                         (list features)))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES."
  :indent 1)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; UNNECESSARY CHANGE, CHANGE BACK!
(setq user-emacs-directory ;; should be directory of init.el or Emacs.org
      (file-name-directory (or load-file-name (buffer-file-name))))
(setq user-init-file ;; init.el in user-emacs-directory
      (concat user-emacs-directory "init.el"))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

;; no-littering
(setup (:pkg no-littering)
  (require 'no-littering)) ; is the require statement really necessary?

;; keep autosaves in emacs dir
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backup/" user-emacs-directory))))

;; method 1
;; Keep customization settings in a temporary file (does this even work?)
;; (setq custom-file
;;       (if (boundp 'server-socket-dir)
;;           (expand-file-name "custom.el" server-socket-dir)
;;         (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;; (load custom-file t)

;; method 2
(setq custom-file (make-temp-file "emacs-custom"))

;; add user lisp files to path
;; (push vs add-to-list?)
(push (concat user-emacs-directory "lisp/") load-path)

(require 'server)
(with-eval-after-load 'server
  ;; if an instance of emacs is not running, then start server.
  (unless (and (boundp 'server-process)
               (processp server-process)
               (server-running-p))
    (server-start)
    (message "Emacsclient Server started!")
    ))

(setq ri/exwm-enabled
      (and (eq window-system 'x)
           ;; returns 't if process with exwm exists
           (let ((exwm-process (string-to-number
                                (shell-command-to-string
                                 "ps aux | grep exwm | grep -vc grep")))) ;<
             (> exwm-process 0))
           ;; (string-match-p "exwm" (format "%s" command-line-args))
           ;; (seq-contains command-line-args "--start-exwm") 
           (if (eq window-system 'pgtk)
               (progn (message "ERROR: You cant start EXWM bc Emacs was compiled with pgtk!")
                      nil)
             t)))

(when ri/exwm-enabled
  (message "exwm-enabled!")
  (require 'ri-desktop))

(setq ri/use-evil-mode nil)

;; disable startup screen
(setq inhibit-startup-message nil)

;; disable ui
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode 1)     ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(menu-bar-mode -1)   ; disable menu bar

;; other
(setq fill-column 70) ; shadowed by visual-fill-column if visual-fill-column-mode is non-nil.
(recentf-mode 1) ; show recent files when viewing files (counsel enables by def).
(save-place-mode 1) ; go to previous location in file when reopening.

;; disable bell
(setq ring-bell-function 'ignore) ; TURN OFF ONCE AND FOR ALL?
;; (setq ring-bell-function 'silent) ; TURN OFF ONCE AND FOR ALL?

;; enable mode line flash bell
;; (use-package mode-line-bell
;; :if (ring-bell-function 'ignore)
;; :config
;; (mode-line-bell-mode))

;; add line numbers
(global-display-line-numbers-mode t)
(column-number-mode) ; (columns on modeline)

;; line number mode exceptions
(dolist (mode '(org-mode-hook
                dired-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                image-minor-mode-hook
                doc-view-minor-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; default font (modeline, minibuffer, default for applications, etc)
(set-face-attribute 'default nil :font "Fira Code" :height 110 :foreground "white")
;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 115)

;; fixed pitch font (code blocks, property, startup, etc (can add more))
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 110)

;; variable pitch font (toc links, regular text in org, etc...)
;; how about Iosveka instead?
;; (bullets are configured in org-fonts)
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 125 :weight 'regular)

(require 'org)
(defun ri/org-font-setup ()
  (interactive)
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    ;; font for bullets
    (set-face-attribute (car face) nil :font "Liberation Mono" :weight 'bold :height (cdr face))
    ;; (set-face-attribute (car face) nil :font "" :weight 'bold :height (cdr face))
    )

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch :height 1.5)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  (set-face-attribute 'org-hide nil :inherit 'fixed-pitch) ; fixes indentation

  ;; more options that can be set:
  (set-face-attribute 'org-tag nil :inherit '(shadow) :weight 'bold)
  ;; (set-face-attribute 'org-document-info :foreground "dark orange")
  ;; (set-face-attribute 'org-document-info-keyword :inherit (shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent :inherit '(org-hide fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)) ; hide stars
  ;; (set-face-attribute 'org-link :foreground "royal blue" :underline t)
  ;; (set-face-attribute 'org-property-value :inherit fixed-pitch)
  )

(use-package fontaine)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ; scroll when using mouse wheel.
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling.
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse regardless of focus.
(setq scroll-conservatively 0) ; move window when moving off screen.
(setq scroll-margin 0) ; margin before scroll at top and bottom of screen.
(setq scroll-step 1) ; keyboard scroll one line at a time.
(setq use-dialog-box nil) ; (change to nil) make things like yes or no prompts dialogue boxes.

;; set transparency
(defvar ri/transparency-value 94)

(message "TEST: %s" default-frame-alist)

(defun ri/transparency--on-all-frames ()
  "Cycles through and updates transparency value on all existing frames"
  (interactive)
  (let ((iframe (selected-frame))
        (frame (selected-frame)))
    ;; set transparency for the init workspace (god my elisp is messy...)
    (set-frame-parameter
     (eval frame) 'alpha (cons ri/transparency-value ri/transparency-value))
    (setq frame (next-frame frame))
    (while (not (equal iframe frame))
      (set-frame-parameter
       (eval frame) 'alpha (cons ri/transparency-value ri/transparency-value))
      (setq frame (next-frame frame)))))

(defun ri/transparency--default-value-on-new-frame ()
  "Update the default frame transparency value"
  (interactive)
  ;; delete default transparency entry from default-frame-alist.
  (setq default-frame-alist (assoc-delete-all 'alpha default-frame-alist))
  ;; add new default transparency value to alist.
  (add-to-list
   'default-frame-alist (cons 'alpha (cons ri/transparency-value ri/transparency-value))))

(defun ri/update-transparency ()
  "Update both the current transparency on all frames, as well as the
default value for new frames created"
  (interactive)
  (ri/transparency--on-all-frames)
  (ri/transparency--default-value-on-new-frame))

(defun ri/change-transparency (value)
  "Prompt the user to enter a new transparency value for all existing
and future frames"
  (interactive "nEnter new transparency value (0 - 100): ")
  (setq ri/transparency-value value)
  (ri/update-transparency))

;; Startup
(when ri/exwm-enabled
  ;; maximize windows by default
  ;; (message "DEBUG: disabled regular transparency for now!")
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; new transparency options:
  ;; (set-frame-parameter nil 'alpha-background 80) ; For current frame
  ;; (add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth
  ;; Define transparency in theme declaration
  )

(message "TEST: %s" default-frame-alist)

;; Notes:

;; (modify-all-frames-parameters '((cons 'alpha (cons ri/def-transparency ri/def-transparency))))

;; (add-to-list ; works!!!
;; 'default-frame-alist (cons 'alpha ri/def-transparency))
;; (set-frame-parameter ;; set current workspace transparency to
;;  (selected-frame) 'alpha (cons ri/def-transparency ri/def-transparency))

;; (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (100 . 100))

;; shorten y-n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package ace-popup-menu
  :config
  ;; (require 'ace-popup-menu)
  (ace-popup-menu-mode 1))

;; all-the-icons
;; note: on a new machine, must run M-x all-the-icons-install-fonts
;; create a script to automatatically detect whether this has been run?
(setup (:pkg all-the-icons)
  (if (eq (string-to-number
  	 (shell-command-to-string "fc-list | grep -c 'all-the-icons'"))
  	'0)
      (all-the-icons-install-fonts)))

;; nerd-fonts (used by doom-modeline by default)
;; note: on a new machine, must run M-x nerd-icons-install-fonts
(use-package nerd-icons
  :config
  (if (eq (string-to-number
	   (shell-command-to-string "fc-list | grep -c 'NFM'"))
	  '0)
      (nerd-icons-install-fonts)))

;; for directories and such
(setup (:pkg f))

(require 'generic-functions)
(require 'ri-git-interface-commands)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; ESC to quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "<escape>") #'god-mode-all)
;; (global-set-key (kbd "<escape>") #'god-local-mode)

(defun meow-setup ()
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore)
   '("t" . "p") ; improved solution? (access Motion "t" with "SPC t")
   )
  (meow-leader-define-key
   '("t" . "H-t")
   ;; '("p" . "H-p")
   ;; '("u" . ctl-x-map)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   ;; make S-<num> easier to hit with DVP by using symbols.
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   ;; symbols
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(":" . meow-goto-line) ;; moved from "Q" and "E"
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   ;; basic letters
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   ;; '("E" . meow-goto-line) ;; removed, since ":" for it works
   '("f" . meow-find)
   '("F" . meow-search) ;; moved from "s" ("s" is used for movement)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   ;; H Directional key moved to the bottom
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   ;; '("m" . meow-mark-word) ;; swap with w, next-word (because "b"/"m" is easy for mvmnt)
   ;; '("M" . meow-mark-symbol) ;; swap with W, next-symbol (because "b"/"m" is easy for mvmnt)
   '("m" . meow-next-word) ;; moved from "w", mark-word
   '("M" . meow-next-symbol) ;; moved from "W", mark-symbol
   ;; N Directional key moved to the bottom
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . ri/quit-temp-window)
   ;; '("Q" . meow-goto-line) ;; move to " : "
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   ;; '("s" . meow-search) ;; move to F, replace with directional keys
   ;; S Directional key moved to the bottom
   ;; T Directional key moved to the bottom
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   ;; '("w" . meow-next-word) ;; swap with m, mark-word/symbol
   ;; '("W" . meow-next-symbol)
   '("w" . meow-mark-word) ;; moved from "m", mark-word
   '("W" . meow-mark-symbol) ;; moved from "M", mark-symbol
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . ri/scroll-down-half-page) ;; new keys
   '("?" . ri/scroll-up-half-page) ;; new keys
   '("<escape>" .  keyboard-escape-quit)

   ;; Directional keys:

   ;; <-  ^  v  ->
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("t" . meow-prev)
   '("T" . meow-prev-expand)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("s" . meow-right)
   '("S" . meow-right-expand)

   ;; ^  <-  v  ->
   ;; '("h" . meow-prev)
   ;; '("H" . meow-prev-expand)
   ;; '("t" . meow-left)
   ;; '("T" . meow-left-expand)
   ;; '("n" . meow-next)
   ;; '("N" . meow-next-expand)
   ;; '("s" . meow-right)
   ;; '("S" . meow-right-expand)

   ;; ^  /  <-  ->  v
   ;; '("h" . meow-left)
   ;; '("H" . meow-left-expand)
   ;; '("t" . meow-right)
   ;; '("T" . meow-right-expand)
   ;; '("n" . meow-prev)
   ;; '("N" . meow-prev-expand)


   ))

;; is `when-loaded' necessary? is just declaring it like this suffice?

;; issue comes from saving, then running while saving?
(defun ri/meow-exit-all-and-save ()
  (interactive)
  (meow-insert-exit) ; normal mode
  ;; (meow-cancel-selection)
  (yas-abort-snippet)
  (save-buffer)
  (keyboard-quit))

(setup (:pkg meow)
  (require 'meow) ; replaces :when-loaded
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (meow-setup)
  (setq meow-use-cursor-position-hack t) ; opens in front when no selection
  ;; free-keys is very useful!
  (define-key meow-insert-state-keymap (kbd "C-g")
              #'meow-insert-exit) ; cancel selection
  (define-key meow-insert-state-keymap (kbd "C-M-g")
              #'ri/meow-exit-all-and-save)
  ;; make SPC in meow Motion-mode not do anything if specific mode
  (defvar ri/meow-spc-disable t
    "if t, disables meow-keypad on select buffers")
  (defun ri/toggle-meow-spc-disable ()
    (interactive)
    (setq ri/meow-spc-disable (not ri/meow-spc-disable)))
  ;;
  (require 'info)
  (require 'gnus)
  (define-key Info-mode-map (kbd"<f9>") #'ri/toggle-meow-spc-disable)
  (define-key gnus-summary-mode-map (kbd"<f9>") #'ri/toggle-meow-spc-disable)
  (define-key gnus-article-mode-map (kbd"<f9>") #'ri/toggle-meow-spc-disable)
  ;;
  (meow-motion-overwrite-define-key
   '("SPC" . (lambda () (interactive)
               (if ri/meow-spc-disable
                   (progn ; if these buffers, replace command.
                     (cond ((eq major-mode 'Info-mode) (Info-scroll-up))
                           ((eq major-mode 'gnus-summary-mode) (gnus-summary-next-page))
                           ((eq major-mode 'gnus-article-mode) (gnus-article-goto-next-page))
                           ((eq major-mode 'w3m-mode) (w3m-scroll-up-or-next-url nil))
                           (t (meow-keypad))))
                 ;; if var is nil, just do the def.
                 (progn (meow-keypad))))))
  ;; start up applications in insert mode
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (meow-global-mode 1))

;; evil-mode exclude
(defun ri/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; evil-mode
(use-package evil
  :disabled
  :commands evil-mode
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'ri/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) ; wowie

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; -- haven't set up visual line mode yet
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;;hook
;; have these programs be in emacs-mode (C-z)
;;(evil-mode-hook . mi/evil-hook)

;; evil collections
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; general.el
;; (use-package general)
(setup (:pkg general :straight t)
  :config
  (general-create-definer leader-key-def
    :prefix "C-c"))

(defun ri/save-and-meow-normal-mode ()
  (interactive)
  (meow-insert-exit)
  (save-buffer))
;; save and normal mode
(leader-key-def
  "c" '(ri/save-and-meow-normal-mode :which-key "save and normal mode")
  "h" '(ri/save-and-meow-normal-mode :which-key "save and normal mode"))

(leader-key-def
  "s" '(:ignore t :which-key "special"))

(leader-key-def
  "q"  '(:ignore t :which-key "quit/session")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))

;; hydra (fast, transient keybinds)
;; (use-package hydra
;;   :defer t)
(setup (:pkg hydra :straight t))

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("f" nil "finished" :exit t))

;; use another keys
(leader-key-def
  "ss" '(hydra-text-scale/body :which-key "scale text"))

;; maybe replace the first bind with replace kill line with crux?
(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line)
  ;; Fucks things up in some modes:
  ;; ("C-o" . (lambda (arg) (interactive "P")
  ;;            (if (not arg)
  ;;                (progn
  ;;                  (save-excursion
  ;;                    (electric-newline-and-maybe-indent)))
  ;;              (progn
  ;;                (crux-smart-open-line-above)))))
  :config
  ;; (global-set-key [remap kill-line] 'crux-smart-kill-line)
  (leader-key-def
    "mc" 'crux-cleanup-buffer-or-region))

(use-package expand-region
  :commands expand-region)

(use-package avy
  :bind ("C-:" . 'avy-goto-char)
  :commands avy)

(leader-key-def
  "j"  '(:ignore t :which-key "avy")
  "jj" 'avy-goto-char
  "jl" 'avy-goto-line)

(global-set-key (kbd "C-r") 'point-to-register)
(global-set-key (kbd "C-M-r") 'jump-to-register)

(use-package cheatsheet
  :bind (("C-h /" . cheatsheet-show)
         :map cheatsheet-mode-map
         ("q" . kill-buffer-and-window))
  :config
  (setq cheatsheet--cheat-list nil)
  (cheatsheet-add-group 'org
                        '(:key "C-c TAB" :description "close current heading"))
  (cheatsheet-add-group 'generic
                        '(:key "C-c C-SPC" :description "jump to previous location in buffer")
                        '(:key "C-c &" :description "jump to previous location any buffer")
                        '(:key "C-x r SPC <k>" :description "save point to registor")
                        '(:key "C-x r SPC <k>" :description "jump to registor"))
  (cheatsheet-add-group 'commands
                        '(:key "org-lint" :description "debug an org file"))
  (cheatsheet-add-group 'general-notes
                        '(:key "emacs-lisp-mode-map" :description "for viewing local keybinds")))

(use-package free-keys
  :commands free-keys)

(leader-key-def
  "sF" 'free-keys)

;; keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;; scroll window up/down by one line
;; FIX this by changing C-M-g to command name "universal-argument"?
(global-set-key (kbd "M-n") (kbd "M-- 1 C-v"))
(global-set-key (kbd "M-p") (kbd "M-- 1 M-v"))

(global-set-key (kbd "C-M--") #'complete-symbol)

(progn
  ;; (with-eval-after-load 'electric-mode
  ;;   (define-key electric-mode-map (kbd "C-m") #'electric-newline-and-maybe-indent))
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-m") #'org-return-and-maybe-indent)))

(defun ri/delete-window-auto-balance ()
  (interactive)
  (delete-window)
  (balance-windows))

(defun ri/split-window-right-auto-balance ()
  (interactive)
  (split-window-right)
  (balance-windows))

(defun ri/split-window-below-auto-balance ()
  (interactive)
  (split-window-below)
  (balance-windows))

(global-set-key (kbd "C-x 0") #'ri/delete-window-auto-balance)
(global-set-key (kbd "C-x 2") #'ri/split-window-below-auto-balance)
(global-set-key (kbd "C-x 3") #'ri/split-window-right-auto-balance)

;; breaks a lot of stuff, dont do this...
;; (advice-add 'delete-window :after #'balance-windows)
;; (advice-add 'split-window-right :after #'balance-windows)
;; (advice-add 'split-window-below :after #'balance-windows)

(use-package ace-window
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?g ?c ?r))
  (defvar aw-dispatch-alist
    '((?d aw-delete-window "Delete Window")
      (?1 delete-other-windows "Delete Other Windows")
      (?s aw-split-window-horz "Split Horz Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?, aw-split-window-fair "Split Fair Window")
      (?o aw-flip-window "Other Window")
      (?w aw-swap-window "Swap Windows")
      (?m aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?b aw-switch-buffer-in-window "Select Buffer")
      (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  (global-set-key (kbd "M-o") 'ace-window))

;; not downloaded from melpa, because the latest commit is broken.
;; instead downloaded from emacswiki website and loaded from `./lisp/'.
;; Issue: [[https://github.com/lukhas/buffer-move/issues/18]]
;; Homepage: [[https://www.emacswiki.org/emacs/buffer-move.el]]
(require 'buffer-move)

;; bind this to a keybind?
(defhydra ri/hydra-window-adjust (:timeout 5)
  "scale windows"
  ("h" (shrink-window-horizontally -5))
  ("s" (shrink-window-horizontally 5))
  ("t" (shrink-window -5))
  ("n" (shrink-window 5))
  ("H" (buf-move-left))
  ("S" (buf-move-right))
  ("T" (buf-move-up))
  ("N" (buf-move-down))
  ("r" (message "reset!"))
  ("g" nil "finished" :exit t))

(global-set-key (kbd "C-M-)") #'ri/hydra-window-adjust/body)

;; Update this to where if no match, quit-window the latest window (maybe with get-mru-window
;;  or maybe the old function that i scrapped before a commit...) and if universal-argument,
;;  do the last visited window.
(defun ri/quit-temp-window ()
  "Run `quit-window' on a window in the current frame if it's one of the specified,
temporary buffers, like `*cargo-run*'"
  (interactive)
  ;; iwindow - initial window
  (let* ((iwindow (selected-window))
         (ibuffer (buffer-name (window-buffer iwindow)))
         (window (next-window))
         (buffer (buffer-name (window-buffer window)))
         (window-to-quit nil))
    (while (not (equal buffer ibuffer))
      (if (or (equal buffer "*cargo-run*"))
          (progn
            (if (not window-to-quit)
                (setq window-to-quit window)
              (message "There are two windows that can be matched; picking the first..."))))
      (setq window (next-window window))
      (setq buffer (buffer-name (window-buffer window))))
    ;; after complete loop
    (if window-to-quit
        (progn (select-window window-to-quit)
               (quit-window))
      ;; if no matches, just prompt to quit next-window
      (progn (unless (equal iwindow (next-window iwindow))
               (select-window (next-window))
               (if (y-or-n-p "Quit window?")
                   (kill-buffer-and-window)
                 (select-window iwindow)))))))

;; replace evil-direction w/ package
(leader-key-def
  "w"  '(:ignore t :which-key "window")
  "wv" '(split-window-right :which-key "v-split")
  "ws" '(split-window-below :which-key "h-split")
  "wd" '(delete-window :which-key "close window")
  "wc" '(delete-window :which-key "close window")
  "ww" '(evil-window-next :which-key "next-window")
  "wW" '(evil-window-prev :which-key "prev-window")
  ;;
  "wp" '(windmove-up :which-key "window-up")
  "wt" '(windmove-up :which-key "window-up")
  "wn" '(windmove-down :which-key "window-down")
  ;;
  "wb" '(windmove-left :which-key "window-left")
  "wf" '(windmove-right :which-key "window-right")
  "ww" '(aw-flip-window :which-key "other-window")
  ;;
  ;; "wH" '(evil-window-move-far-left :which-key "move left")
  ;; "wJ" '(evil-window-move-very-bottom :which-key "move down")
  ;; "wK" '(evil-window-move-very-top :which-key "move up")
  ;; "wL" '(evil-window-move-far-right :which-key "move right")
  "wa" '(hydra-window-adjust/body :which-key "window-ratio-adjust")
  "wi" '(:ignore t :which-key "minibuffer")
  "wie" 'minibuffer-keyboard-quit
  "wio" 'switch-to-minibuffer)

(leader-key-def
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" 'meow-last-buffer
  "bb" 'counsel-switch-buffer
  "br" 'read-only-mode)

(setup (:pkg key-quiz)
  (:global "C-c z k" key-quiz))

;; magit
;; (add several links...)
;; (magit-status is C-x g)
;; (tab to see diff of files)
;; (hunks, "?" for all commands, C-c C-k to quit commit, push to remote, ssh?)
;; (learn more about magit...)
;; (use-package magit :straight t)
  ;; :bind (:map magit-status-mode-map
  ;;             ("p" . magit-tag)
  ;;             ("t" . magit-section-backward))
  ;; :custom
  ;; what does this do? fullscreen?
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; )

(setup (:pkg magit-todos))

(setup (:pkg magit :straight t)
  (:also-load magit-todos)
  (:global "C-M-;" magit-status)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (setup (:pkg magit-todos))

(leader-key-def
  "v"  '(:ignore t :which-key "magit")
  "vv" '(magit-status :which-key "magit")) ; (same as magit)

;; forge
;; (run forge-pull in a repo to pull down)
;; (pull down all issues, pull-reqs, etc)
;; (need to create a token first, then put in .authinfo)
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; (use-package forge
;;   :disabled
;;   :after magit)
(setup (:pkg forge)
  (:disabled))

;; ivy
(use-package ivy
  :diminish ; hide ivy minor-mode on modeline
  :bind (("C-s" . swiper) ;; fuzzy search tool
         ("C-c s p" . swiper-isearch-thing-at-point)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial-or-done)
         ("C-M-d" . ivy-immediate-done)
         ;; Evil mode:
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-height 15)
  ;; fixes bug with swiper breaking when hovering over links.
  ;; After the bug has been fixed, change it back to `text-properties'.
  (org-fold-core-style 'overlays)
  :config
  (message "Ivy got loaded!")
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; counsel (ivy-enhanced standard emacs commands)
(use-package counsel
  :bind (;; ("M-x" . counsel-M-x)
         ;; ("C-x b" . counsel-ibuffer)
         ;; ("C-x C-f" . counsel-find-file)
         ("C-x b" . 'counsel-switch-buffer)
         ;; ("s-c" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  (message "Counsel loaded!")
  (counsel-mode 1))

;; adds ivy completion regex and order commands by last used
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package ef-themes)

(use-package catppuccin-theme) ; no use?

(use-package doom-themes)

(use-package poet-theme)

(use-package kaolin-themes)

;; (load-theme 'ef-dark t)
;; (load-theme 'ef-trio-dark t) ; o 
;; (load-theme 'ef-cherie t)
;; (load-theme 'ef-winter t) ; o
;; (load-theme 'kaolin-valley-dark t)
;; (load-theme 'kaolin-bubblegum t) ; dark blue, not bad
;; (load-theme 'kaolin-shiva t) ; soft fluffy

;; favorites
;; (load-theme 'kaolin-eclipse t) ; pink pretty noice, brightest?
;; (load-theme 'doom-gruvbox t) ; easy on the eyes and goes well with glt wallpaper

;; (load-theme 'doom-dracula t)

;; (load-theme 'catppuccin t)

;; (load-theme 'doom-snazzy t)
;; (load-theme 'doom-laserwave t)
;; (when ri/exwm-enabled
;; (ri/change-transparency 90)
;; (ri/change-transparency 95)
;; (ri/change-transparency 100)
;; no spectrum, pro, classic
;; nice ristretto (dark), 

;; test
(defun mn (suffix)
  (intern 
   (concat "doom-"
           (mapconcat (lambda (i)
                        (byte-to-string i))
                      '(109 111 110 111 107 97 105))
           "-" suffix)))

(setq ri/theme
      ;; 'doom-gruvbox
      ;; 'doom-material-dark ; sits comfortably
      ;; 'doom-bluloco-dark ; interesting blue
      'doom-moonlight ; feels like the moonlight
      ;; 'doom-sourcerer ; feels like swamp witch
      ;; 'doom-oksolar-dark ; feels like blue-washing (no comments)
      ;; 'doom-old-hope
      ;; 'doom-palenight
      ;; (mn "spectrum") ; no hl
      ;; 'kaolin-eclipse ; best
      ;; 'ef-trio-dark ; nicenice
      ;; 'ef-winter
      ;; 'kaolin-shiva
      )

(setq ri/themes-and-transparency-alist
      '((kaolin-eclipse . 94)
        (doom-gruvbox   . 94)
        (doom-old-hope   . 94)
        (doom-material-dark . 94)
        (ef-trio-dark . 97)
        (ef-winter . 94)
        (kaolin-shiva . 94)
        ;;
        (catppuccin     . 100)
        (ef-dark        . 100)
        (doom-dracula   . 90)
        (doom-moonlight . 95)
        ))

(defvar ri/auto-set-transparency t
  "If true, automatically set transparency value corresponding
to theme.")

;;

(defun ri/set-theme ()
  "Set theme based on ri/theme"
  (load-theme ri/theme t))

(defun ri/set-transparency-from-theme (&optional current-theme)
  "Set transparency value from theme, based on
ri/themes-and-transparency-alist"
  (interactive "P")
  (let* ((pair (assoc ri/theme ri/themes-and-transparency-alist))
         (theme (or current-theme (car pair)))
         (opacity (cdr pair)))
    (if (or pair theme opacity)
        (ri/change-transparency opacity)
      (message "Error: theme not found in alist or not set correctly."))))

;; (defun ri/set-theme-and-transparency ()
;;   (interactive)
;;   (let* ((pair (assoc ri/theme ri/themes-and-transparency-alist))
;;          (theme (car pair))
;;          (opacity (cdr pair)))
;;     (if (or pair theme opacity)
;;         (progn
;;           (load-theme theme t)
;;           (if ri/auto-set-transparency
;;               (ri/change-transparency opacity)))
;;       (message "Error: theme not found in alist or not set correctly."))))

(ri/set-theme) ; always do
(when ri/exwm-enabled
  (ri/set-transparency-from-theme))
(ri/set-transparency-from-theme) ; fuck it lets do this anyway
;;

(defun ri/toggle-theme-auto-transparency ()
  "Enable/disable theme auto transparency"
  (interactive)
  (setq ri/auto-set-transparency (not ri/auto-set-transparency))
  (message "%s" ri/auto-set-transparency))

(defun ri/set-theme-and-org-font-setup (&optional theme)
  (interactive "P")
  (let ((new-theme-name nil))
    ;; if no input, prompt
    (if (not theme)
        (setq new-theme-name (intern (counsel-load-theme)))
      (load-theme theme t))
    (when ri/exwm-enabled
      (ri/set-transparency-from-theme (or counsel-load-theme theme)))
    (ri/org-font-setup)))

;; warning: using this too many times will break the emacs environment.
;;  Like literally, everything starts breaking, eventually freezing the system as a whole.
(defun ri/random-theme ()
  (interactive)
  (let* ((themes (custom-available-themes))
         (n (random (length themes)))
         (pick (nth n themes))
         (loaded (if (null pick) (car themes) pick)))
    (ri/set-theme-and-org-font-setup pick)
    (message "New theme loaded! %s" pick)))

;;

(global-set-key (kbd "C-h T") 'ri/set-theme-and-org-font-setup)

(leader-key-def
  "st" '(ri/set-theme-and-font-setup :which-key "choose theme")
  "sr" '(ri/random-theme :which-key "random theme"))

(use-package diminish)

;; doom-modeline
(setup (:pkg doom-modeline)
  (require 'doom-modeline)
  ;; :init (doom-modeline-mode 1)
  ;; (:hook-into after-init-hook)
  ;; Hacky bugfix with modeline
  (display-battery-mode nil)
  (display-time-mode nil)
  (display-battery-mode t)
  (display-time-mode t)
  (doom-modeline-mode 1)
  (:option doom-dracula-brighter-modeline nil
	   doom-modeline-height 32 ; 45? 40? best 20, stuck with 30 for a while
	   doom-modeline-hud nil))

(use-package mini-modeline
  :disabled
  ;; :custom
  ;; (mini-modeline-r-format)
  ;; :config
  ;; (mini-modeline-mode t)
  )

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defvar infu-bionic-reading-face nil "a face for `infu-bionic-reading-region'.")

(setq infu-bionic-reading-face 'bold)
;; try
;; 'bold
;; 'error
;; 'warning
;; 'highlight
;; or any value of M-x list-faces-display

(defun infu-bionic-reading-buffer ()
  "Bold the first few chars of every word in current buffer.
Version 2022-05-21"
  (interactive)
  (infu-bionic-reading-region (point-min) (point-max)))

(defun infu-bionic-reading-region (Begin End)
  "Bold the first few chars of every word in region.
Version 2022-05-21"
  (interactive "r")
  (let (xBounds xWordBegin xWordEnd  )
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (forward-word)
        ;; bold the first half of the word to the left of cursor
        (setq xBounds (bounds-of-thing-at-point 'word))
        (setq xWordBegin (car xBounds))
        (setq xWordEnd (cdr xBounds))
        (setq xBoldEndPos (+ xWordBegin (1+ (/ (- xWordEnd xWordBegin) 2))))
        (put-text-property xWordBegin xBoldEndPos
                           'font-lock-face infu-bionic-reading-face)))))

;; (use-package modus-themes
;;   :custom
;;   ;(modus-themes-mode-line '(borderless))
;;   :config
;;   (load-theme 'modus-vivendi t))

;; which-key (lists keybinds)
;; (add links above source blocks later)
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; (setup (:pkg which-key)
;;   (message "DEBUG PLEASE!!!: %s" (:pkg which-key))
;;   (diminish 'which-key-mode)
;;   (setq which-key-idle-delay 0.3))

;; helpful (improves help menu)
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind ;; change the function of the command
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  ("C-h M" . which-key-show-major-mode)
  ("C-h h" . helpful-at-point)
  ("C-h E" . describe-keymap))

(leader-key-def
  "sh" 'helpful-at-point
  "sv" 'describe-variable
  "sf" 'describe-function
  "sk" 'describe-key
  "sM" 'which-key-show-major-mode
  "sm" 'describe-mode
  "sR" 'info-display-manual
  "sP" 'describe-package)

(use-package info
  :bind
  (:map Info-mode-map
        ("j" . Info-scroll-down)
        ("S-SPC" . Info-scroll-up)
        ("v" . Info-scroll-up)
        ("k" . Info-scroll-up)))



(setup (:pkg darkroom)
  (:global "C-c s D" #'darkroom-tentative-mode))

(setup (:pkg dashboard)
  (require 'dashboard)
  ;; Do this manually instead.
  ;; (dashboard-setup-startup-hook)
  (when (or (< (length command-line-args) 2) ; >
  	  ri/exwm-enabled)
    (add-hook 'after-init-hook (lambda ()
  			       ;; Display useful lists of items
  			       (dashboard-insert-startupify-lists)))
    (add-hook 'emacs-startup-hook (lambda ()
                                    (switch-to-buffer dashboard-buffer-name)
                                    (goto-char (point-min))
                                    (redisplay)
                                    (run-hooks 'dashboard-after-initialize-hook)))))

(use-package fireplace)

(setup (:pkg discover-my-major)
  (:global "C-h M-m" 'discover-my-major)
  (:global "C-h M-S-m" 'discover-my-mode))

(setup (:pkg guix))

(leader-key-def
  "G"  '(:ignore t :which-key "Guix")
  "Gg" '(guix :which-key "Guix")
  "Gp" '(guix-packages-by-name :which-key "search packages"))

(setup (:pkg sly)
  (require 'sly-autoloads)
  (defun ri/stumpwm-connect ()
    (interactive)
    (sly-connect "localhost" '4004)))

(setup (:pkg stumpwm-mode)
  (require 'stumpwm-mode)
  (defun ri/enable-stumpwm-mode-if-file-matches ()
    (let ((find-path (concat (getenv "HOME") "/.dotfiles/.stumpwm.d/README.org")))
      (when (and (file-exists-p find-path)
                 (equal (file-truename buffer-file-name)
                        find-path))
        (unless stumpwm-mode
          (stumpwm-mode 1)))))
  (add-to-list 'org-mode-hook #'ri/enable-stumpwm-mode-if-file-matches))

;; org
(defun ri/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org :straight (:type built-in)
  :commands (org-capture org-agenda)
  :hook (org-mode . ri/org-mode-setup)
  :bind
  (:map org-mode-map
        ("C-M-<return>" . (lambda () (interactive)
                            (org-insert-heading-respect-content)
                            (org-demote-subtree))))
  :custom
  (org-ellipsis " ▼")
  (org-hide-emphasis-markers t) ; hide formatting chars (* / ~ = etc)
  (doom-modeline-enable-word-count t)
  (org-hide-leading-stars nil)
  :config
  ;; show message when loading (not necessary?)
  (message "Org Mode loaded!"))

;; misc
(leader-key-def
  "o"  '(:ignore t :which-key "org")
  "ox" '(eval-last-sexp :which-key "eval-last-sexp")
  "oX" '(eval-region :which-key "eval-region"))

(with-eval-after-load 'org
  ;; insert-mode when create new heading
  (defun ri/my--insert-heading-hook ()
    (if (eq meow-normal-mode t)
        (meow-insert-mode)))

  (setq org-insert-heading-hook '(ri/my--insert-heading-hook))

  ;; custom func for toggle heading
  (defun ri/my--toggle-heading ()
    (interactive)
    (org-back-to-heading)
    (org-cycle))

  ;; hydra for navigation
  (defhydra ri/hydra-org-navigation (:timeout 60)
    ;; heading navigation
    ("n" org-next-visible-heading "next")     ; C-c C-n
    ("p" org-previous-visible-heading "prev") ; C-c C-p 
    ("t" org-previous-visible-heading "prev") ; C-c C-p
    ;; 
    ("f" org-forward-heading-same-level "forward")   ; C-c C-f
    ("b" org-backward-heading-same-level "backward") ; C-c C-b
    ("s" org-forward-heading-same-level "forward")   ; C-c C-f
    ("h" org-backward-heading-same-level "backward") ; C-c C-b
    ;;
    ("u" outline-up-heading "up") ; C-c C-u
    ("d" outline-up-heading "up") ; C-c C-u
    ;; heading move
    ("H" org-metaleft "metaleft") ; <-
    ("T" org-metaup "metaup") ; ^^
    ("P" org-metaup "metaup") ; ^^
    ("N" org-metadown "metadown") ; v
    ("S" org-metaright "metaright") ; ->
    ;; page navigation
    ("j" ri/scroll-down-half-page "half down")
    ("k" ri/scroll-up-half-page "half up")
    ("/" ri/scroll-down-half-page "half down")
    ("?" ri/scroll-up-half-page "half up")
    ;;
    ("v" scroll-up-command "page up")
    ("V" scroll-down-command "page down")
    ;;
    ("," beginning-of-buffer "top of page")
    ("." end-of-buffer "end of page")
    ;; single down/up cursor
    ("C-n" next-line)
    ("C-p" previous-line)
    ("C-t" previous-line)
    ;; open/close
    ("TAB" ri/my--toggle-heading "open-close")
    ("c" org-shifttab "global-cycle")
    ;;
    ("g" nil "quit" :exit t))

  (leader-key-def
    "n" '(ri/hydra-org-navigation/body :which-key "hydra-navigation"))

  )

(ri/org-font-setup)

(require 'ri-workflow)

;; keybinds! -----

;; mostly just an example
;; (define-key global-map (kbd "C-c j")
;;   (lambda () (interactive) (org-capture nil "jj")))

;; org-agenda leader keybinds (create a separate section?
(leader-key-def
  "oA"  '(org-agenda-list :which-key "org-agenda-schedule")
  "oc" '(org-capture :which-key "org-capture")
  "oa"  '(:ignore t :which-key "org-agenda")
  "oaa" '(org-agenda :which-key "agenda-commands")
  "oas" '(org-agenda-list :which-key "agenda-schedule")
  "oat" '(org-todo-list :which-key "todo-list")
  "oar" '(org-refile :which-key "org-refile") ; put refile in org-mode-map?
  "od"  '(org-deadline :which-key "deadline")
  "os"  '(org-schedule :which-key "schedule")
  "o."  '(org-time-stamp :which-key "time-stamp")
  "o,"  '(org-priority :which-key "priority")
  "ot" '(org-todo :which-key "todo state set")
  "oq" '(org-set-tags-command :which-key "set tags menu")
  "oQ" '(counsel-org-tag :which-key "set tags list menu")
  "op" '(org-set-property :which-key "set property")
  "oe" '(org-set-effort :which-key "set effort")
  ;; for tag multi-add alt-enter! (?)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                        '(("^ *\\([-]\\) "
;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; (use-package extra-empaisi)

;; visual-fill-mode (padding)
(defun ri/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        ;; visual-fill-column-center-text t
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ri/org-mode-visual-fill)
  :config
  (setq visual-fill-column-enable-sensible-window-split nil))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (scheme . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist
               '("mani" . "src scheme :noweb-ref packages :noweb-sep \"\"")))

;; tangle using #+auto_tangle: t
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

;; org-babel (tangle n stuff)
;; Automatically tangle our Emacs.org config file when we save it
;; (defun ri/org-babel-tangle-config ()
;;   (when (string-equal (file-name-directory (buffer-file-name))
;;                       (expand-file-name user-emacs-directory))

    ;; ;; Dynamic scoping to the rescue
    ;; (let ((org-confirm-babel-evaluate nil))
    ;;   (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ri/org-babel-tangle-config)))

(leader-key-def
  "ob"  '(:ignore t :which-key "org-babel")
  "obt" '(org-babel-tangle :which-key "tangle")
  "obe" '(org-babel-execute-src-block :which-key "org-babel-execute-src-block"))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(setup (:pkg org-roam :straight t)
  (let ((dir (file-truename "~/Notes/Roam")))
    (unless (file-directory-p dir)
      (f-mkdir-full-path dir))
    (setq org-roam-directory dir))
  (add-hook 'after-init #'org-roam-mode)
  (:with-map org-roam-mode-map
    (:bind "C-c n l" org-roam-buffer-toggle
           "C-c n f" org-roam-node-find
           "C-c n g" org-roam-graph
           "C-c n i" org-roam-node-insert
           "C-c n c" org-roam-capture
           ;; Dailies
           "C-c n j" org-roam-dailies-caputre-today))
  ;; for vertical completion framework
  (setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

(leader-key-def
  "oL" 'org-latex-export-to-pdf)

(setup (:pkg typo)
  (typo-global-mode 1))

(use-package org-pomodoro
  :bind ("C-c o P" . org-pomodoro))

(use-package org-download
  ;; :config
  ;; (setq org-download-screenshot-method "") ; only supports scrot, gm, and xclip.
  )

(setup markdown-mode
  (:hook visual-line-mode))

;; if nil, use eglot if possible
;; (setq ri/prefer-eglot-mode t)
(setq ri/prefer-eglot-mode nil)

(setup (:pkg lsp-mode)
  ;; (:bind "TAB" completion-at-point)
  (:option lsp-headerline-breadcrumb-enable nil
           lsp-keymap-prefix "C-c l"
           lsp-clients-clangd-executable (executable-find "clangd")
           ;; reduce flashiness
           lsp-eldoc-render-all nil ; all info in minibuffer
           lsp-enable-symbol-highlighting t ; highlight repeated symbols
           lsp-symbol-highlighting-skip-current t ; skip current symbol when highlighting
           ;;
           lsp-inlay-hint-enable t ; ???
           ;; fixes yasnippet last bracket behavior
           lsp-enable-relative-indentation t
           )

  (setq lsp-idle-delay 0.1) ; set unique for each lang mode?

  (:when-loaded
    (progn
     (global-set-key (kbd "C-c l") lsp-command-map) ; to get Meow space to work
     (lsp-enable-which-key-integration)
     ))

  (leader-key-def
    "l" '(:ignore t :which-key "lsp")
    "lx" '(:ignore t :which-key "xref")
    "lxd" 'xref-find-definitions
    "lxr" 'xref-find-references
    "ln" 'lsp-ui-find-next-reference
    "lp" 'lsp-ui-find-prev-reference
    "ls" 'counsel-imenu
    "le" 'lsp-ui-flycheck-list
    "lS" 'lsp-ui-sideline-mode
    "lX" 'lsp-execute-code-action))

(setup (:pkg lsp-ui)
  (:hook-into lsp-mode)
  (:when-loaded
    (progn
      (setq lsp-ui-sideline-enable t) ; enable lsp-ui sideline
      (setq lsp-ui-sideline-show-hover nil) ; hover mesgs in sideline
      (setq lsp-ui-doc-position 'bottom) ; position of prev setting
      (setq lsp-ui-doc-show-with-cursor nil) ; show documentation when hover over
      (lsp-ui-doc-show))))

;; (use-package lsp-treemacs
;;   :after lsp-mode)

;; (use-package lsp-ivy
;;   :after lsp-mode)

;; (use-package ivy-xref
;;   :init
;;   (when (>= emacs-major-version 27) ; < to fix meow-block
;;     (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package eglot)

;; (setup (:pkg eglot)
;;   ;; TODO: Don't load until needed
;;   (require 'eglot)
;;   ;; (define-key eglot-mode-map (kbd "C-c C-a") #'eglot-code-actions)
;;   ;; (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename)
;;   ;; (setq eglot-autoshutdown t
;;   ;;       ;; eglot-confirm-server-edits nil
;;         ;; )
;;   )

(setup (:pkg paredit)
  (:hook-into emacs-lisp-mode scheme-mode)
  (:bind "M-r" nil)) ; originally `paredit-raise-sexp'

;; (setup (:pkg lispy))

;; broken?
(use-package flycheck)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode) ; make sure flycheck is installed first!

(define-key emacs-lisp-mode-map (kbd "C-M--") #'completion-at-point)

;; auto correct comment length
(progn
  (auto-fill-mode 1)
  (require 'newcomment)
  (setq comment-auto-fill-only-comments t))

(leader-key-def
  "ol" 'org-lint)

(setup org
  (:global "C-c e m" emacs-lisp-macroexpand))

;; Include .sld library definition files
(setup scheme-mode
  (:file-match "\\.sld\\'")
  (:hook guix-devel-mode))

(setup (:pkg geiser)
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(setup (:pkg geiser-guile))

(add-hook 'scheme-mode-hook #'flycheck-mode)

(use-package slime)

;; rust-analyzer required. gnu guix package?
(use-package rustic
  ;; ensure t ;; no need *
  :hook (rust-mode . lsp-deferred)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references) ; overwrites xref-find-references
              ("C-M-c" . lsp-execute-code-action)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c S" . lsp-rust-analyzer-status)
              ("C-c C-c j" . lsp-rust-analyzer-join-lines)
              ("C-c C-c t" . lsp-rust-analyzer-related-tests)
              ("C-c C-c C" . lsp-rust-analyzer-open-cargo-toml)
              ("C-c C-c D" . lsp-rust-analyzer-open-external-docs)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c R" . lsp-rename)
              ("C-c C-c A" . lsp-execute-code-action)
              ("C-c C-c g" . lsp-execute-code-action)
              ("C-c C-c y" . ivy-yasnippet)
              ("C-c C-c E" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance)
              ("C-c C-c P" . rust-playground)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown))
  :custom
  ;; what to use when checking on-save. def "check", rec "clippy". try both.
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; (lsp-rust-analyzer-cargo-watch-command "check")
  (rustic-format-on-save nil)

  ;; lsp hints ----
  ;; All below controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Will require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  ;; ----
  ;; Below is built-in (basically shows inferred type inline after var decl.)
  ;; (lsp-rust-analyzer-server-display-inlay-hints t) ; no symbol at point?
  ;; Elided lifetime inlay hints:
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; Inlay type hints for method chains:
  (lsp-rust-analyzer-display-chaining-hints t)
  ;; Parameter names or numeric placeholder names for lifetimes:
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil) ; ??
  ;; Closure return type inlay hints for closures with block bodies:
  (lsp-rust-analyzer-display-closure-return-type-hints t) ; ??
  ;; Function parameter name inlay hints at the call site:
  (lsp-rust-analyzer-display-parameter-hints t)
  ;; Inlay type hints for compiler inserted reborrows:
  (lsp-rust-analyzer-display-reborrow-hints "always")
  ;; Inlay type hints for binding modes:
  (lsp-rust-analyzer-binding-mode-hints t) ; ??

  :config
  ;; on-save features in rustic-mode
  (add-hook 'rustic-mode-hook 'ri/rustic-mode-hook))

(defun ri/rustic-mode-hook ()
  ;; if C-c C-c C-r (run) and buffer not saved, save without prompting and run.
  ;; Doesn't fix, not a rustic issue?
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  ;; each lang will have its own value
  (setq lsp-idle-delay 0.5))


(use-package rust-playground
  :after lsp-mode)

(require 'cc-mode)
;; if ri/prefer-elgot-mode, use ccls instead.
(dolist (mode '(c++-mode-hook
                c-mode-hook
                objc-mode-hook
                cuda-mode-hook))
  (add-hook mode (lambda ()
                   (if ri/prefer-eglot-mode
                       (progn
                         (message "set to use eglot")
                         (eglot-ensure))
                     (progn
                       (lsp-deferred)
                       (setq lsp-idle-delay 0.5))))))

(add-hook 'c-mode-hook
          (lambda ()
            ;; (c-set-style "bsd")
            ;; (setq c-basic-offset 2)
            (indent-tabs-mode 0)
            ))

(defvar ri/c-compile-inputs nil)
(defun ri/c-compile-and-run (&optional use-extra)
  (interactive)
  (unless (or (eq major-mode 'c-mode) (eq major-mode 'comint-mode))
    (error "not in c-mode or comint-mode!"))
  (let* ((main (file-name-nondirectory (buffer-file-name)))
         (exe (file-name-sans-extension main))
         (src (let ((extra-files '("leak_detector_c.c")) ; fill this up
                    (valid-files ""))
                (dolist (p (push main extra-files))
                  (if (file-exists-p
                       (file-name-concat (file-name-directory (buffer-file-name)) p))
                      (setq valid-files (concat valid-files p))))
                valid-files))
         (run-command
          (if (and use-extra ri/c-compile-inputs)
              (concat "gcc " src " -std=gnu11 -lm -o " exe " && ./" exe " && "
                      ri/c-compile-inputs)
            (concat "gcc " src " -std=gnu11 -lm -o " exe " && ./" exe))))
    (save-buffer)
    (compile run-command t) ; -lm for math.h lib
    ;; go through every window in current frame, if match comint-mode, select.
    (let ((orig-win (selected-window))
          (curr-win (next-window)))
      ;; `catch' returns 't if while-loop finds a comint buffer in frame.
      (if (eq 't (catch 'break
                   (let ((inc '0)) ; increments per window change
                     (while (not (equal orig-win curr-win))
                       (if (equal 'comint-mode
                                  (with-current-buffer ; returns value of major-mode when
                                      (window-buffer curr-win) ; on buffer of curr-win.
                                    major-mode))
                           (throw 'break t)) ; return t to `catch', found a match
                       ;; select next window
                       (setq curr-win (next-window curr-win))
                       ;; increment by 1 and err if inc value is 1000
                       (setq inc (+ inc 1))
                       (if (equal inc 1000)
                           (progn (user-error "Infinite loop!")
                                  (throw 'break nil))))
                     ;; ^end of while loop
                     (throw 'break nil)))) ; went through all windows, no match.
          (progn
            (select-window curr-win) ; found match, focus on curr-win.
            (end-of-buffer)
            ;; dont make it strongly dedicated!
            (set-window-dedicated-p (selected-window) nil)
            (if (meow-normal-mode-p)
                (meow-insert-mode)))
        (progn
          (message "%s" "could not find compilation buffer!"))))))

(define-key c-mode-map (kbd "<f8>") #'ri/c-compile-and-run)
(define-key c-mode-map (kbd "S-<f8>") (lambda () (interactive)
                                        (ri/c-compile-and-run 't)))
(define-key comint-mode-map (kbd "<f8>") #'quit-window)

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (require 'cc-mode)
;;             (define-key c-mode-map "\C-c\M-c" #'lsp-ui-imenu)))

(with-eval-after-load 'dap-mode
  (require 'dap-cpptools) ; also for dap
  (dap-cpptools-setup)) ; for dap

;; ;; language server (alternative to clangd)
;; (use-package ccls)
;;   ;; :after lsp-mode
;;   :hook
;;   ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp-deferred)))
;;   :bind (:map c-mode-map
;;               ("C-c C-c" . nil)
;;               ("C-c C-c C-i" . lsp-ui-imenu))
;;   :config
;;   (with-eval-after-load dap-mode
;;     (dap-cpptools-setup) ; for dap
;;     (require 'dap-cpptools))) ; also for dap

(use-package python-mode
  ;; :ensure t ;; no need *
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))



(with-eval-after-load 'mhtml-mode
  (define-key mhtml-mode-map (kbd "M-F") facemenu-keymap)
  (define-key mhtml-mode-map (kbd "M-o") 'nil)
  (define-key mhtml-mode-map (kbd "C-c C-v") #'ri/open-file-in-external-browser))

(use-package quickrun)

(defun ri/abort-company-and-meow-insert-exit ()
  (interactive)
  (company-abort)
  (meow-insert-exit))

(use-package company
  :hook ((eglot-server-initialized lsp-mode) . company-mode)
  :bind
  ((:map company-active-map ; disable C-n, C-p, replace with C-RET, C-M-RET?
         ("<tab>" . company-complete-selection)
         ("C-t" . company-select-previous-or-abort)
         ("C-<return>" . company-select-next)
         ("C-g" . ri/abort-company-and-meow-insert-exit)
         )
   (:map lsp-mode-map ; when in any buffer with lsp minor mode enabled
         ;; ("<tab>" . company-indent-or-complete-common) ; only indent!!!!!!
         ;; ("<tab>" . company-complete-common)
         ;; ("<tab>" . company-complete-common-or-show-delayed-tooltip)
         ;; ("<tab>" . company-complete-common-or-cycle)
         )
   (:map company-search-map
         ("C-t" . company-select-previous-or-abort)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  ;; fixes evil-normal and cancel company autocomplete when escape
  ;; doesn't work if escape hit very quickly
  :config
  (if ri/use-evil-mode
      (add-hook 'company-mode-hook
                (lambda ()
                  (add-hook 'evil-normal-state-entry-hook
                            (lambda ()
                              (company-abort)))))))

;; prettified company front-end with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

(setup (:pkg yasnippet)
  (require 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (yas-reload-all))

(setup (:pkg yasnippet-snippets))

;; provides command ivy-yasnippet
(setup (:pkg ivy-yasnippet))

(setup (:pkg flycheck)
  (:hook-into lsp-mode))

;; check lsp-enable-dap-auto-configure if enabled
(use-package dap-mode
  :after lsp-mode
  :config
  (message "DEBUG: THIS SHOULD SHOW!")
  ;; (setq dap-auto-configure-mode) ; def t
  ;; (dap-ui-mode 1) ; below uncomment if prev custom var is nil
  ;; (dap-tooltip-mode 1)
  ;; (require 'dap-node)
  ;; (dap-node-setup)

  ;; Rust debugger ----
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs vscode extension
  ;; (more info: https://robert.kra.hn/posts/rust-emacs-setup/)
  ;; list errors and fixes here:
  (dap-gdb-lldb-setup)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

;; projectile
;; (project management)
;; (bound to C-p)
;; (dir-locals are pretty cool)
;; (learn more about projectile for better project management)
(use-package projectile
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy)) ;; by default auto
  ;; :bind-keymap
  ;; ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code/code")
    (setq projectile-project-search-path '("~/Code/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; counsel-projectile
;; (more options in M-o... already installed?)
;; (counsel-projectile-rg + M-o for a massive search in project)
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package compile
  :ensure nil ; make sure use-package doesn't compile it
  :straight nil
  :custom
  (compilation-scroll-output t))

(use-package aggressive-indent
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; (use-package evil-nerd-commenter
;;   :disabled
;;   :custom
;;   (evil-want-keybinding nil)
;;   :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(global-set-key (kbd "M-/") 'meow-comment)

(global-prettify-symbols-mode 1)

;;; crdt.el
;;; Explanation by Ari Wari :3
;; -----------------------------
;;; Host: ----
;; - Run M-x `crdt-share-buffer' to create a shared session if not already
;; exists, and add the current buffer (file) to the list of shared buffers in
;; the session. In the menu presented when creating a new shared session, you
;; can specify a password, display name, and client permissions. The other
;; settings can be left as default. (Experimental feature: You can also specify
;; a TLS port under "Secure port" if you want to use that protocol. You can also
;; set the variable `crdt-default-tls' to `t' down below to have it always
;; be enabled by default.)
;;
;;; Client: ----
;; - Run M-x `crdt-connect', which will prompt for an address, port, and your
;; display name.
;;
;;--------
;; + NOTE: in most list interfaces, you can press 'k' or 'd' to
;; remove/kill/disconnect them.
;;----------
;;
;;; Sessions: ----
;; - List active sessions with M-x `crdt-list-sessions'.
;;  - (Terminate session:
;;      'k'/'d' in list, or
;;      (host: M-x `crdt-stop-session')
;;      (client: M-x `crdt-disconnect'))
;;
;;; Buffers: ----
;; - List shared buffers with M-x `crdt-list-buffers'. Navigate and press 'RET'
;; to go to that buffer.
;;  - (Remove buffer from session: 
;;      'k'/'d' in list, or M-x `crdt-stop-share-buffer'.)
;; - M-x `crdt-share-buffer' to add your own local buffers to the
;; list of shared buffers.
;;
;;; Users: ----
;; - List active users with M-x `crdt-list-users'. Navigate to and press 'RET'
;; to go to their current cursor position. Press 'f' to follow that user, and
;; press 'f' again to detach.
;; - M-x `crdt-goto-next-user' and `crdt-goto-prev-user' to cycle through
;; users' cursor positions, from any shared buffer.
;;
;;; Quitting: ----
;; - The host can terminate the session by running M-x `crdt-stop-session'.
;; - The client can disconnect from a session with M-x `crdt-disconnect'.
;;
;;; Fancy stuff: ----
;; - M-x `crdt-visualize-author-mode' to color highlight text based on who
;; edited it (client-side).
;; - M-x `crdt-org-sync-overlay-mode' to toggle the syncing of folding/expanding
;; org-mode headings across peers (client-side).
;; - Read docs for more: sharing a REPL with different terminals, Tox/Teredo
;; proxy or SSH port forwarding to a VPS with a pubilc ip if host ip is not
;; public, and etc. (https://elpa.gnu.org/packages/crdt.html)
;;

(use-package crdt
  :config
  ;; protocol version must be the same on all peers
  (setq crdt-protocol-version "0.3.0")
  ;; enable to use TLS by default (kinda broken, dont recommend)
  (setq crdt-default-tls nil)
  ;; ---------------------------
  ;; To enable crdt-visualize-author-mode or crdt-org-sync-overlay-mode by
  ;;  default, uncomment:
  ;; (crdt-visualize-author-mode 1)
  ;; (crdt-org-sync-overlay-mode 1)
  ;; ---------------------------
  ;; Configure keybinds here:
  ;; (you can do almost everything from the .*-list-.* commands, 
  ;;  so just memorize those)
  (global-set-key 
   (kbd "C-c C") ; parent prefix key for below keybinds (pick something vacant)
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "S") #'crdt-share-buffer) ; start server
     ;; sessions
     (define-key map (kbd "s s") #'crdt-list-sessions) ; useful / press 'k'/'d' to remove
     (define-key map (kbd "s S") #'crdt-share-buffer)
     (define-key map (kbd "s X") #'crdt-stop-session)
     (define-key map (kbd "s d") #'crdt-disconnect)
     (define-key map (kbd "s k") #'crdt-disconnect)
     ;; buffers
     (define-key map (kbd "b b") #'crdt-list-buffers) ; useful
     (define-key map (kbd "b a") #'crdt-share-buffer)
     (define-key map (kbd "b o") #'crdt-switch-to-buffer)
     (define-key map (kbd "b d") #'crdt-stop-share-buffer)
     (define-key map (kbd "b k") #'crdt-stop-share-buffer)
     ;; users
     (define-key map (kbd "u u") #'crdt-list-users) ; useful / press 'f' to follow user
     (define-key map (kbd "u f") #'crdt-follow-user)
     (define-key map (kbd "u n") #'crdt-goto-next-user)
     (define-key map (kbd "u p") #'crdt-goto-prev-user)
     (define-key map (kbd "u g") #'crdt-goto-user)
     ;; misc
     (define-key map (kbd "m v") #'crdt-visualize-author-mode)
     (define-key map (kbd "m o") #'crdt-org-sync-overlay-mode)
     ;; return filled keymap
     map)))

(setup (:pkg pinentry)
  (:option epg-pinentry-mode 'loopback))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") 
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defhydra ri/hydra-vterm-scale (:timeout 5)
  "scale text"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("t" text-scale-increase "in")
  ("n" text-scale-decrease "out")
  ("=" text-scale-set "reset")
  ("g" nil "finished" :exit t))

(setup (:pkg vterm)
  ;; :commands vterm
  (:with-map vterm-mode-map
    (:bind "C-," vterm-send-next-key
           "C-=" ri/hydra-vterm-scale/body
           "C--" text-scale-decrease
           "C-+" text-scale-increase))
  (:when-loaded
    ;; enter Meow insert mode automatically when open
    (when meow-global-mode
      (add-hook 'vterm-mode-hook (lambda ()
                                   (if meow-normal-mode
                                       (meow-insert-mode)))))
    ;; vv already set vv
    ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
    ;; (:when-loaded)
    (setq vterm-shell "bash")
    (setq vterm-buffer-name-string "vterm %s")
    (setq vterm-max-scrollback 10000)
    ;; Fixes vterm compilation on Guix System.
    ;; (https://www.reddit.com/r/GUIX/comments/11gzhyu/
    ;;  how_to_compile_the_vterm_module_from_emacs_and/)
    (defun ri/vterm-link-guix-library-on-compile (f &rest r)
      "Advice to replace compiling vterm with linking to just symlinking the guix library"
      (let ((guix-library "~/.guix-extra-profiles/emacs/emacs/lib/vterm-module.so"))
        (if (f-exists-p guix-library)
            (make-symbolic-link
             (expand-file-name "~/.guix-extra-profiles/emacs/emacs/lib/vterm-module.so")
             (file-name-directory (locate-library "vterm.el" t)) t)
          (message "DEBUG: vterm guix library %s doesn't exist, cant compile" guix-library))))
    (if ri/is-guix-system ; TODO: no need to do this shit :sob::sob::sob:
        (advice-add 'vterm-module-compile :around #'ri/vterm-link-guix-library-on-compile))))

;; eshell config
(defun ri/configure-eshell ()
  ;; Save command history when commands are entered.
  ;;   Commands usually don't save until close, so if crashes, loses all progress.
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;; fixes the issue with cursor going to the beginning... fixed?
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

;; themes
(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . ri/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "ssh")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package multi-term
  :disabled
  :config
  (setq multi-term-program "eshell"))

(use-package multi-vterm
  :config
  ;; dedicated terminal height of 30%
  (setq multi-vterm-dedicated-window-height-percent 30))

(leader-key-def
  "at" 'vterm
  "aT" 'multi-vterm
  "am" '(:ignore t :which-key "multi-vterm-control")
  "amt" 'multi-vterm-project
  "amp" 'multi-vterm-prev
  "amn" 'multi-vterm-next
  "amm" 'multi-vterm-dedicated-toggle
  "ae" 'eshell)

(leader-key-def
  "f"  '(:ignore t :which-key "files")
  "fr" '(counsel-recentf :which-key "recent files")
  "ff" '(find-file :which-key "find-file")
  "fp" '(lambda () (interactive)
          (find-file (expand-file-name "~/.dotfiles/.emacs.d/"))
          :which-key "open Emacs.org"))

;; dired
(use-package dired
  :ensure nil ; make sure use-package doesn't try to install it.
  :straight nil
  :commands (dired dired-jump) ; defer loading of this config until a command is executed.
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t) ; auto select dir to move to if another dired window open.
  (delete-by-moving-to-trash t)
  ;;(dired-compress-files-alist) ; add more file types to compression.
  :config
  (if ri/use-evil-mode
      (evil-collection-define-key 'normal 'dired-mode-map
                                "h" 'dired-single-up-directory
                                "l" 'dired-single-buffer
                                "f" 'dired-create-empty-file)))
;;     ^ Might not work if using two dired windows! (dired-up-directory, dired-find-file)

;; HAS TO COME AFTER dired because using ":after dired".
;; Reuses the current Dired buffer to visit a directory without
;; creating a new buffer.
(use-package dired-single
  :after dired)

(defun ri/all-the-icons-dired-helper ()
  (if (equal dirvish-override-dired-mode nil)
      (all-the-icons-dired-mode)))

(use-package all-the-icons-dired
  :hook (dired-mode . ri/all-the-icons-dired-helper))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions
        '(("mkv" . "mpv")
          ;; ("pdf" . "docview")
          ("png" . "feh")
          ("docx" . "libreoffice"))))

;; ------------------------------

(use-package dired-hide-dotfiles
  :after dired
  :config
  ;; special hack from ./lisp
  (require 'dired-hide-dotfiles-hack)
  ;; :config
  )

(leader-key-def
  "d"  '(:ignore t :which-key "dired")
  "dd" 'dired
  "dj" 'dired-jump
  "dh" 'ri/dired-hide-dotfiles-mode-toggle)

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :commands (dired dired-jump)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("o" "~/Notes/org/"                "Org")
     ("s" "~/Notes/School/"             "School")
     ("c" "~/Notes/School/classes/current/"      "classes")
     ("C" "~/Code/"                     "Code")
     ("d" "~/Downloads/"                "Downloads")
     ("p" "~/Pictures/"                 "Pictures")
     ("e" "~/.dotfiles/.emacs.d/"       "Emacs user directory")
     ("g" "~/yui/guix"                  "Guix config")
     ("y" "~/yui/channel"               "Yumi channel")
     ("N" "~/.newmacs.d"                "Newmacs config")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "Trash")))
  :config
  ;; (define-key dirvish-mode-map (kbd "z") 'ri/dired-hide-dotfiles-mode--toggle)
  (setq dired-listing-switches
        ;; "-ahgo --group-directories-first"
        "-l --almost-all --human-readable --group-directories-first --no-group") ; AhoG
  (setq dired-dwim-target t) ; auto select dir to move to if another dired window open.
  (setq delete-by-moving-to-trash t)
  (dirvish-peek-mode)
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-preview-dispatchers
        (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))
  (setq dirvish-open-with-programs
        (let ((mpv (or (executable-find "mpv") "mpv")))
          `((,dirvish-audio-exts . (,mpv "--profile=builtin-pseudo-gui" "%f"))
            (,dirvish-video-exts . (,mpv "%f")))))
  :bind
  ("C-c d D" . dirvish)
  ("C-c d f" . dirvish-fd)
  ("C-c D" . dirvish-quick-access)
  ("C-c d a" . dirvish-quick-access)
  (:map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
        ("h" . dired-up-directory)
        ("r" . dired-sort-toggle-or-edit)
        ("s" . dired-open-file)
        ("'" . ri/dired-hide-dotfiles-hack-toggle)
        (";" . dirvish)
        ("N" . dired-create-empty-file)
        ("M-T" . ri/dired-set-wallpaper)
        ("/" . dired-isearch-filenames-regexp)

        ("a"   . dirvish-quick-access)))

;; ("f"   . dirvish-file-info-menu)
;; ("y"   . dirvish-yank-menu)
;; ("N"   . dirvish-narrow)
;; ("^"   . dirvish-history-last)
;; ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;; ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;; ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;; ("TAB" . dirvish-subtree-toggle)
;; ("M-f" . dirvish-history-go-forward)
;; ("M-b" . dirvish-history-go-backward)
;; ("M-l" . dirvish-ls-switches-menu)
;; ("M-m" . dirvish-mark-menu)
;; ("M-t" . dirvish-layout-toggle)
;; ("M-s" . dirvish-setup-menu)
;; ("M-e" . dirvish-emerge-menu)
;; ("M-j" . dirvish-fd-jump)))

(defun ri/dired-set-wallpaper ()
  (interactive)
  (let ((path (dired-get-filename)))
    (start-process-shell-command
     "feh dired" nil
     (concat "feh --bg-fill \"" path "\""))))

(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
               ;; causing feh to be opened...
               "feh"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "mupdf"
               '(file)))))

(setq tramp-default-method "ssh")
(setq tramp-verbose 10)

;; doesn't work... what is tramp?
(use-package sudo-edit
  :custom
  (sudo-edit-local-method "doas")
  :config
  (global-set-key (kbd "C-c C-r") 'sudo-edit))

(use-package dired-toggle-sudo
  :commands (dired dired-jump)
  :bind
  (:map dired-mode-map
        ("C-c C-s" . dired-toggle-sudo)))

;; rss
;; maybe don't need, phone is enough?
;; maybe syncthing and import from database?
;; dont use commands elfeed, scan at startup?
;; expand upon this to make it usable (copy over old configs)
(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '(
          "http://pragmaticemacs.com/feed/"
          "https://blog.privacyguides.org/feed_rss_created.xml"
          "https://www.cozynet.org/feed/feed.xml"
          )))

(leader-key-def
  "ar" 'elfeed)

;; (setq browse-url-browser-function 'browse-url-default-browser)
(setq browse-url-browser-function 'w3m)

(defun ri/browse-url-external-open (url)
  "Open the specified url in one of the following browsers:
Librewolf, qutebrowser, firefox, etc..."
  (interactive "s")
  (message "DEBUG: the input: %s" url)
  (cond
   ((executable-find "qutebrowser")
    (call-process "qutebrowser" nil 0 nil url))
   ((executable-find "/usr/bin/librewolf")
    (call-process "/usr/bin/librewolf" nil 0 nil "--new-tab" url))
   (t
    (error "No usable browser found"))
   ))

(defun ri/open-file-in-external-browser (&optional file-path)
  "Open the file in the current buffer, or FILE-PATH, in an external browser."
  (interactive)
  (ri/browse-url-external-open (browse-url-file-url (if file-path
                                                        file-path
                                                      (buffer-file-name)))))

;; eww is shite, also SPC and h trigger prefix. w3 browser?
;; disable cookies, or delete history after closing?
;; (setq browse-url-browser-function 'eww-browse-url)
(use-package eww)

(use-package w3m
  :bind (:map w3m-mode-map
              ("F" . w3m-toggle-filtering)
              ("f" . w3m-scroll-up-or-next-url)))

(use-package gnus
  :commands gnus
  :config
  (setq user-full-name '"aili")
  (setq user-mail-address '"yourname@email.invalid")
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods '((nntp "news.gmane.io")
                                        ;(nntp "news.alt.religion.emacs")
                                        ;(nntp "gnu.emacs.sex")
                                      ))

  (setq gnus-directory (concat user-emacs-directory "News/")
        gnus-startup-file (concat user-emacs-directory "News/.newsrc")
        message-directory (concat user-emacs-directory "Mail/")))

;;(setq gnus-secondary-select-methods '((nntp "alt.religion.emacs")))

;; matrix client
(use-package ement
  :commands ement)

(use-package jabber
  :commands jabber)

;; erc
;; make erc start after startup?
(use-package erc
  :config
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
        erc-server "irc.libera.chat"
        erc-nick "senko"
        erc-user-full-name "artemis"
        ;; erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#linux"))
        ;; By default, ERC selects the channel buffers when it reconnects. If you'd like it to connect to channels in the background, use this:
        erc-auto-query 'bury
        erc-kill-buffer-on-part t ; if nil, will reuse buffers if rejoin.
        erc-fill-static-center 27 ; def: 27
        ;; erc-fill-function 'erc-fill-static ; def: erc-fill-variable
        erc-fill-function 'erc-fill-variable ; the def ^^^
        erc-fill-column 80 ; def: 78
        ))

(leader-key-def
  "ai" 'erc-ssl)

(use-package mastodon
  :commands mastodon)

;; org binding on M-t so make all t key bindings translate to p ?

;; swap ctrl and alt keys, since it's easier to press ctrl with the thumb

(global-set-key (kbd "C-t") 'previous-line)
;; make C-h 'prev-line instead? (make sure to git commit pull and push before, and
;;  don't push for a while. So to revert, simply revert to remote origin main head).
;;  (maybe create a variable and if true change certain keybinds for modes?
;; One issue with swapping is that C-f C-b C-h C-n becomes harder to do and possibly,
;;  /possibly/ harder to do.

(global-set-key (kbd "C-u") ctl-x-map)
;; (global-set-key (kbd "C-z") 'universal-argument)
;; (global-set-key (kbd "C-z") help-map)
(global-set-key (kbd "M--") 'universal-argument)
;; (global-set-key (kbd "C-M-g") 'universal-argument)

;; what if i bind "C-c z" to help-map so that i can do "SPC z" in meow-leader-mode-map to
;;  access "C-z" options? Should work...

;; ;; make gc pauses faster by decreaseing the threshold.
;; (setq gc-cons-threshold (* 2 1000 1000))
