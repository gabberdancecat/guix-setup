(define-module (src home modules setup))

(use-modules (gnu home services dotfiles)
             (gnu home services)
             (guix utils)
             (ice-9 pretty-print))

(export my/setup-service)

;; stow dotfiles

;; (define my-dotfiles-service
;;   (list
;;    (service home-dotfiles-service-type
;;             (home-dotfiles-configuration
;;              (directories '("../../../dotfiles"))
;;              (layout 'stow)
;;              ;; for parent dir of dotfiles dir
;;              (excluded '(".*~" "\\..*" "\\.archive" "\\.files" "README\\.org"))))))

;; (define my/setup-service
;;   (append my-dotfiles-service))
