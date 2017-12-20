#lang s-exp stamps/lang

; Stamps Tutorial
; Z-order example 

; When overlapping shapes you can determine which appear
; over others using the z-order adjustment:

(define-shape artwork
  (circle [z-order 1])
  (triangle [t 0.3 0]
            [hue 60]
            [sat 1]
            [b 1]
            [z-order 2])
  (square [t -0.3 0]
          [hue 120]
          [sat 1]
          [b 1]))

; The higher the z-order the higher the shape appears
; in the artwork. Try changing the values above to change
; their order.

(maximum-render-cycles 100)
(start-shape artwork)