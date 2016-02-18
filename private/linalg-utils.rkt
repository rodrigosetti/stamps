#lang racket/base

;; Linear algebra combinators

(require math/matrix)

(provide (all-defined-out))

(define (rotation-matrix theta)
  (matrix [[(cos theta) (- (sin theta)) 0]
           [(sin theta) (cos theta)     0]
           [0           0               1]]))

(define (scaling-matrix sx sy)
  (matrix [[sx 0  0]
           [0  sy 0]
           [0  0  1]]))

(define (translation-matrix dx dy)
  (matrix [[1 0 dx]
           [0 1 dy]
           [0 0 1 ]]))
