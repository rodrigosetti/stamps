#lang s-exp stamps/lang

;; Original version by https://contextfreeart.org/gallery2/index.html#design/3139
;; Translated to Racket-Stamps by Eric Clack.

;; Creative Commons licensed: Creative Commons Attribution 3.0 Unported
;; https://creativecommons.org/licenses/by/3.0/

(define rr random-real)
(define ri random-integer)

(define-shape stack
  (paper [s (rr 0.5 1.5)]
         [h (ri 1 360)]
         [r 5]
         [sat 0.7]))

(define-shape paper
  (square [b -1] [a -0.97] [s 1.3])
  (square [b -1] [sat -1] [s 1.007])
  (square [b 1])
  (paper [r (rr -3 3)]
         [h (rr -7 7)]
         [x (rr -0.07 0.07)]
         [s 0.99]))

(start-shape stack)


