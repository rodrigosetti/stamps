#lang s-exp stamps/lang

; racket-stamps Tutorial
; bounding

; By default stamps will calculate the atwork bounding so that all of the
; generated shapes are visible.

; When you render, stamps will output you the bounding box calculated.
; To see this, run the following...

(define-shape scene
  (circles [h 45] [sat 1] [b 1]))

(define-shape circles
  (circle)
  (circles [s .95]
           [t 1 1]
           [h 15]))

(maximum-render-cycles 100)
(start-shape scene)

; You'll see this output above the artwork
; rendering...recording paths...drawing paths in bounding box (-17.50 -17.50 0.50 0.50)...
; 50 shapes, 163 ms

; You can define your own bounding, e.g. if you want to crop your atwork.
; Uncoment the following command to see the effect:
;(bounding '(-5 -5 1 1))
; The parameters are (x1, y1, x2, y2)