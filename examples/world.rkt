#lang s-exp stamps/lang

(define-shape world
  ; i is 0 to 359
  ((loop ([i 360])
         ; draw a building 90 units out from centre at angle i
         (building [r i]
                   [y 90]))))

(define-shape building
  ; draw a random height building dictated by probability:
  ; 3 in 4: extra squares; 1 in 4 final square
  [3 => (square)
        (building [y 1])]
  [1 => (square)])

(start-shape world)
