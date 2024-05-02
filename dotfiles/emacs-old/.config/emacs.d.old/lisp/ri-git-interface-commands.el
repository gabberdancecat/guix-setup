(defun ri/git-pull-dotfiles ()
  "Run the pull-dotfiles shell command, which stashes existing changes, runs `git pull origin master', pop the stash, then `stow .' to update."
  (interactive)
  (let ((dot-root (f-parent user-emacs-directory)))
    (if (f-directory-p (concat dot-root "/.git"))
        (shell-command (concat "sh " dot-root "/.bin/pull-dotfiles"))
      (error "The root dotfiles directory is not a parent directory of `.emacs.d', so it cannot locate the `.bin' directory, where `pull-dotfiles' command exists. Maybe try to come up with a better solution, so that the user-emacs-directory doesn't have to be in the root dotfiles directory? Maybe in the form of an environmental variable at startup?"))))

(defun ri/stow-dotfiles ()
  "Run the stow command on the root dotfiles directory."
  (interactive)
  (let ((dot-root (f-parent user-emacs-directory)))
    (if (f-file-p (concat dot-root "/.stow-local-ignore"))
        (shell-command (concat "sh " dot-root "/.bin/stow-dotfiles"))
      (error "The root dotfiles directory is not a parent directory of `.emacs.d', so it cannot locate the `.bin' directory, where `pull-dotfiles' command exists. Maybe try to come up with a better solution, so that the user-emacs-directory doesn't have to be in the root dotfiles directory? Maybe in the form of an environmental variable at startup?"))))

(provide 'ri-git-interface-commands)
