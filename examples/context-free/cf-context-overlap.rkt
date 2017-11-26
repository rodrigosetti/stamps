#lang s-exp stamps/lang

;; Original version by Nom at https://contextfreeart.org/gallery2/index.html#design/979
;; Translated to Racket-Stamps by Eric Clack.

;; Creative Commons licensed: Creative Commons Attribution 3.0 Unported
;; https://creativecommons.org/licenses/by/3.0/


(define-shape tres
  ((loop ([i 3])
         (d2 [y .3] [r (* i 120)]))))

(define-shape d2
  ((loop ([i 30])
         (tough [s .4] [y .5] [r (* i 12)]))))

(define-shape tough
  (d [s .2 1.1])
  (step [y .7] [s .6]))  

(define-shape step
  (d [s .3])
  (d2 [y .15] [s .3]))

(define-shape d
  (square)
  (circle [b 1]))

  
(background '(0 0 1))
(maximum-render-cycles 100000)
(start-shape tres)


