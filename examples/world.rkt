#lang s-exp stamps/lang

(define-shape world
  ((loop ([i 360])
         (building [r i]
                   [y 90]))))

(define-shape building
  [3 => (square)
        (building [y 1])]
  [1 => (square)])

(start-shape world)
