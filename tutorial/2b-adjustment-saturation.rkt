#lang s-exp stamps/lang

; racket-stamps Tutorial
; 2b - Adjustment - saturation

; Just like in the previous example, but this time we reduce the
; saturation from full towards no color.

(define-shape ramp-down
  (circle)
  (ramp-down [saturation   -0.3] ; 30% less saturation
             [scale         0.8] ; 80% smaller
             [translate   1 0  ]))

; So the outer shape must have full saturation.

(define-shape outer
  (ramp-down [saturation 1] ; full saturation
             [hue        0] ; red
             [brightness 1]))

; Only draw a few render cycles, this gives 4 shapes
(maximum-render-cycles 10)

(start-shape outer)