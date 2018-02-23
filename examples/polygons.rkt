#lang s-exp stamps/lang

;; Various polygons overlapped

(define-shape polygons
  ((polygon 8))
  ((polygon 7))
  ((polygon 6))
  ((polygon 5))
  ((polygon 4))
  ((polygon 3))
  )

(define-shape scene
  (polygons [alpha -.7]
            [b .3]))

(start-shape scene)