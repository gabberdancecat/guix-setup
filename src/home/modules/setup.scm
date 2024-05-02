(define-module (src home modules setup))

(use-modules (guix utils)
             (ice-9 pretty-print))

(export my/setup-service)

;; stow dotfiles

(define my-dotfiles-service
  (list
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories '("../../../dotfiles"))
             (layout 'stow)
             ;; (packages )
             ;; (excluded '(".*~" ".*\\.swp" "\\.git" "\\.gitignore" "\\.archive" "\\.files"))
             ;; (excluded '(".*~" ".*\\.swp"
             ;;             "\\.gitignore" ".*README\\.org" "\\.stow-local-ignore"
             ;;             "\\.git" "\\.archive" "\\.files"))
             ;; xclude "~" tilde files, "." dot files, and README file
             (excluded '(".*~" "\\..*" "README\\.org")) ; for parent dir of dotfiles dir
             ))))
