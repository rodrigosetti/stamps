#lang s-exp stamps/lang

; racket-stamps Tutorial
; 1b - Shapes - composition

; You can compose several shapes to make something more interesting
; Use this form with a list of shapes in the body:
; (define-shape <name> ...)

(define-shape two-shapes
  (circle)
  (triangle))

(start-shape two-shapes)

; You'll notice that the triangle is over the top of the circle,
; we can fix that with a `translate` adjustment, go ahead and
; change the triangle line to this:
; (triangle [translate 1 0])

; You'll notice that the triangle is now to the left of the
; circle and that the art has been scaled to fit the canvas.

; Read on to see more interesting examples of composition...