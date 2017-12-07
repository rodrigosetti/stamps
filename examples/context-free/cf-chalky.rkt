#lang s-exp stamps/lang

;; Original version by Chris at https://contextfreeart.org/gallery/view.php?id=256
;; Translated to Racket-Stamps by Eric Clack.

;; Creative Commons licensed: Creative Commons Attribution 3.0 Unported
;; https://creativecommons.org/licenses/by/3.0/

(define-shape shape
 (shape2 [alpha -1])
 (shape2 [flip 90]
         [alpha -1]
         [x 5]
         [b 1])
 (shape2 [alpha -1]
         [y -5]
         [b 1])
 (shape2 [flip 90]
         [alpha -1]
         [x 5]
         [y -5]))

(define-shape shape2
  [1 => (square)
        (shape2 [alpha 0.0001]
                [r 10]
                [x 1]
                [s 0.9995])]
  [1 => (square)
        (shape2 [alpha 0.0001]
                [r 9]
                [x 1]
                [s 0.9995])])

(background '(120 1 0.5))
 
(start-shape shape)


