#lang s-exp stamps/lang

; racket-stamps Tutorial
; 1b - Shapes - composition

; Any shape you define with `(define-shape` is just like any other
; shape, you can compose these too, including recursively,
; and you can apply adjustmenst too:

(define-shape circles
  (circle)
  (circle [translate 1 0])
  (circle [translate 1 1])
  (circle [translate 0 1]))

(define-shape outer
  (circles)
  (outer [translate 3 0]
         [rotate -5]
         [scale .95]))

(start-shape outer)

