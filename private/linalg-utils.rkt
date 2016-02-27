#lang typed/racket/base

;; Linear algebra combinators

(require math/matrix
         racket/math)

(provide rotation-matrix
         scaling-matrix
         translation-matrix
         reflection-matrix
         shear-matrix)

(: rotation-matrix (-> Real (Matrix Real)))
(define (rotation-matrix deg)
  (define rad (degrees->radians deg))
  (matrix [[(cos rad)   (- (sin rad)) 0]
           [(sin rad)   (cos rad)     0]
           [0           0             1]]))

(: scaling-matrix (-> Real Real (Matrix Real)))
(define (scaling-matrix sx sy)
  (matrix [[sx 0  0]
           [0  sy 0]
           [0  0  1]]))

(: translation-matrix (-> Real Real (Matrix Real)))
(define (translation-matrix dx dy)
  (matrix [[1 0 (- dx)]
           [0 1 (- dy)]
           [0 0     1 ]]))

(: reflection-matrix (-> Real (Matrix Real)))
(define (reflection-matrix deg)
  (define rad (degrees->radians deg))
  (define x (cos rad))
  (define y (sin rad))
  (matrix [[(- (sqr x) (sqr y)) (* 2 x y)           0]
           [(* 2 x y)           (- (sqr y) (sqr x)) 0]
           [0                   0                   1]]))

(: shear-matrix (-> Real Real (Matrix Real)))
(define (shear-matrix x y)
  (matrix [[1 x 0]
           [y 1 0]
           [0 0 1]]))
