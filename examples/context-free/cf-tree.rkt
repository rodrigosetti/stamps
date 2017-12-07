#lang s-exp stamps/lang

;; Original version by Momo https://contextfreeart.org/gallery2/index.html#design/137
;; Translated to Racket-Stamps by Eric Clack.

;; Creative Commons licensed: Creative Commons Attribution 3.0 Unported
;; https://creativecommons.org/licenses/by/3.0/

;; Changes: removed [skew] adjustments and flowers

(define-shape start
  (branch)
  (branch [flip 90]))

(define-shape shape
  [1 => (square)]
  [0.01 => (square)
        (circle [s 3 10])])

(define-shape branch
  [1 =>
     (shape [hue 10] [sat 0.5] [b 0.5])
     (branch [y 1] [s 0.995] [r 0.5])]
  [0.08 => (branch [flip 90])]
  [0.05 => (branch [s 0.85] [r 15])
        (branch [s 0.8] [r -15])]
)


(background '(1 .08 1))
(maximum-render-cycles 20000)
(start-shape start)


