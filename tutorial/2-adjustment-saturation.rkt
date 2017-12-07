#lang s-exp stamps/lang

; racket-stamps Tutorial
; 2 - Adjustment - saturation

; Saturation is a value between 0 (no color) to 1 (full color)
; When the adjustment is a single value it represents a percentage
; to increase or decrease towards 1 or 0. 

(define-shape ramp-up
  (circle)
  (ramp-up [saturation   0.3] ; 30% more saturation
           [scale        0.8] ; 80% smaller
           [translate  1 0  ]))

; Saturation starts at zero in the first shape.

(define-shape outer
  (ramp-up
   [hue        0] ; red
   [brightness 0.8]))

; Only draw a few render cycles, this gives 5 shapes
(maximum-render-cycles 10)

(start-shape outer)