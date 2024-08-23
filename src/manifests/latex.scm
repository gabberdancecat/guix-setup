(define-module (src manifests latex)
  #:use-module (gnu packages))

;; --- Useful commands: ----
;; guix-update-profiles latex
;; tlmgr info 

;; return a manifest to use by guix custom profile



(specifications->manifest
 '("rubber"

   ;; based on texlive-scheme-medium:
   "texlive-scheme-small"
   "texlive-collection-latexrecommended"
   "texlive-collection-fontsrecommended"
   "texlive-collection-langenglish"
   "texlive-collection-latex"
   "texlive-collection-mathscience"
   ;; "texlive-collection-pictures"
   
   ;; texlive-collection-xetex
   ;; texlive-collection-plaingeneric
   ;; texlive-collection-metapost
   ;; texlive-collection-basic
   ;; texlive-collection-binextra
   ;; texlive-collection-context
   ;; texlive-collection-fontutils
   ;; texlive-collection-latexextra (has wrapfig and capt-of (+ A LOT))

   ;; "texlive-babel-english"

   ;; From "latexextra" collection.
   ;; "texlive-tabularray"
   ;; From "binextra" collection.
   ;; "texlive-texdoc"

   ;; wrapfig
   "texlive-wrapfig"
   ;; ulem
   "texlive-ulem"
   ;; capt-of
   "texlive-capt-of"

   ;; -- Misc: --
   ;; lipsum
   "texlive-lipsum"
   ))

;; disable the above with this below
;; (specifications->manifest
;;  '("rubber"))
