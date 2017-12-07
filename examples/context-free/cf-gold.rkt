#lang s-exp stamps/lang

;; Original version by Gold - https://contextfreeart.org/gallery2/#design/51
;; Translated to Racket-Stamps by Eric Clack.

;; Creative Commons licensed: Creative Commons Attribution 3.0 Unported
;; https://creativecommons.org/licenses/by/3.0/

(define-shape scene
    (square [x 0.5] [y 0.5] [s 2] [hue 240] [sat 1] [b 0.3])
    (rectangle [x 0] [y 0] [sat 1] [b 0] [hue 0] [s 0.71 1])
    (rectangle [x 1] [y 0] [sat 0] [b 1] [hue 0] [s 0.71 1])
    (rectangle [x 0] [y 1] [sat 1] [b 1] [hue 0] [s 0.71 1])
    (rectangle [x 1] [y 1] [sat 0.5] [b 0.5] [hue 0] [s 0.71 1])
)

(define-shape rectangle
  [1 =>
    (square)
    (rectangle [r 90] [s 0.71] [y 0.5] [alpha -0.4] [b -0.1] [sat -0.2] [hue -4])
    (rectangle [r -90] [s 0.71] [y 0.5] [alpha 0.02] [b 0.2] [sat 0.3] [hue 4])]
  [1 =>
     (square)
     (rectangle [r -90] [s 0.71] [y 0.5] [alpha -0.4] [b -0.1] [sat -0.2] [hue -4])
     (rectangle [r 90] [s 0.71] [y 0.5] [alpha 0.02] [b 0.2] [sat 0.3] [hue 4])])

(start-shape scene)