(use-modules (swayipc))

(define rosewater "#f4dbd6")
(define flamingo "#f0c6c6")
(define pink "#f5bde6")
(define mauve "#c6a0f6")
(define red "#ed8796")
(define maroon "#ee99a0")
(define peach "#f5a97f")
(define yellow "#eed49f")
(define green "#a6da95")
(define teal "#8bd5ca")
(define sky "#91d7e3")
(define sapphire "#7dc4e4")
(define blue "#8aadf4")
(define lavender "#b7bdf8")
;; (define text "#cad3f5")
(define text "#f2f5fc")
;; (define text "#ffffff")
(define subtext1 "#b8c0e0")
(define subtext0 "#a5adcb")
(define overlay2 "#939ab7")
(define overlay1 "#8087a2")
(define overlay0 "#6e738d")
(define surface2 "#5b6078")
(define surface1 "#494d64")
(define surface0 "#363a4f")
(define base "#24273a")
(define mantle "#1e2030")
(define crust "#181926")

(sway-client-background base)

(sway-client-focused-color mauve base text
                           #:indictor-color mauve
                           #:child-border-color mauve)

(sway-client-focused-inactive-color overlay0 surface0 text
                           #:indictor-color overlay0
                           #:child-border-color overlay0)

(sway-client-unfocused-color overlay0 surface1 text
                           #:indictor-color overlay0
                           #:child-border-color overlay0)

(sway-client-urgent-color peach base peach
                           #:indictor-color overlay0
                           #:child-border-color peach)

(sway-client-placeholder-color overlay0 base text
                           #:indictor-color overlay0
                           #:child-border-color overlay0)
