;;; dired-hide-dotfiles-hack.el --- toggle and persist hide dotfiles  -*- lexical binding:t -*-

;;; Commentary:

;; This is a cool hack for 'dired-hide-dotfiles-mode' that persistently
;; toggles whether to run the command at Dired-buffer startup
;; using 'dired-mode-hook'.
;; I am elisp noob.  Hope I didn't write anything horrific.

;;; Code:

(require 'dired)

(defvar ri/dired-hide-dotfiles-hack--persist 1
  "If enabled, run 'dired-hide-dotfiles-mode' at Dired buffer startup.
This is checked in 'ri/dired-hide-dotfiles-hack' below, and if
non-nil, run 'dired-hide-dotfiles-mode'.")

(defun ri/dired-hide-dotfiles-hack-toggle ()
  "Toggle whether to persist auto hide dotfiles at Dired buffer startup.
First, it toggles 'dired-hide-dotfiles-mode' in the current buffer.
Then, it saves the value of the new state of 'dired-hide-dotfiles-mode' in
'ri/dired-hide-dotfiles-hack--persist'.  This persist variable is then
checked at the next 'dired-mode-hook' to persist the new toggled setting."
  (interactive)
  (if dired-hide-dotfiles-mode
      (dired-hide-dotfiles-mode 0)
    (dired-hide-dotfiles-mode 1))
  (setq ri/dired-hide-dotfiles-hack--persist dired-hide-dotfiles-mode))

(defun ri/dired-hide-dotfiles-hack ()
  "Will be added to 'dired-mode-hook'.
This function checks whether the persist variable is enabled, and if it
is, run 'dired-hide-dotfiles-mode' in the new Dired buffer."
  (if ri/dired-hide-dotfiles-hack--persist
      (dired-hide-dotfiles-mode)))

(add-hook 'dired-mode-hook #'ri/dired-hide-dotfiles-hack)

;; this binds "z" in dired-mode-map to toggling the dotfiles visibility.
(define-key dired-mode-map (kbd "z") 'ri/dired-hide-dotfiles-hack-toggle)

;; if use evil-mode, make "H" toggle dotfiles.
(if ri/use-evil-mode
    (eval-after-load 'evil-mode
      (evil-collection-define-key 'normal 'dired-mode-map
				  "H" 'ri/dired-hide-dotfiles-hack-toggle)))

(provide 'dired-hide-dotfiles-hack)

;;; dired-hide-dotfiles-hack.el ends here
