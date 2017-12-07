#lang s-exp stamps/lang

; Stamps Tutorial
; Adjustment - Shear 

(define-shape sheared-square
  (square)
  (sheared-square [shear .02 .1]
                  [scale .9]
                  [brightness .1]))

(define-shape sheared-circle
  (circle)
  (sheared-circle [shear .02 .1]
                  [scale .9]
                  [brightness .1]))

(define-shape outer
  (sheared-square)
  (sheared-circle [translate 1 0]))

(maximum-render-cycles 100)
(start-shape outer)