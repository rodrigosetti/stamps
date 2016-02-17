#lang racket/base

; defines the transformation data structure and combinators

(require racket/contract
         racket/vector
         math/matrix
         "random-utils.rkt")

(provide rotate
         scale
         translate
         identity
         transformation?
         transformation-geometric
         transformation-color
         combine-transformation)

;; Linear algebra combinators

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


;; transformation definition

; - geometric: matrix?
; - color: (vector/c real? real? real?) -- HSV deltas
(struct transformation (geometric color) #:transparent)

;; Transformations constructors

(define (geometric-transformation matrix)
  (transformation matrix #[0 0 0]))

(define (color-transformation hsb)
  (transformation (identity-matrix 3) hsb))

(define (identity)
  (geometric-transformation (identity-matrix 3)))

(define/contract (rotate theta)
  (-> real? transformation?)
  (geometric-transformation (rotation-matrix theta)))

(define/contract (scale sx [sy sx])
  (->* (real?) (real?) transformation?)
  (geometric-transformation (scaling-matrix sx sy)))

(define/contract (translate tx ty)
  (-> real? real? transformation?)
  (geometric-transformation (translation-matrix tx ty)))

(define (hue h)
  (color-transformation (vector h 0 0)))

(define (saturation s)
  (color-transformation (vector 0 s 0)))

(define (brightness b)
  (color-transformation (vector 0 0 b)))

;; Transformations combinators
(define/contract (combine-transformation . trans)

  (->* () ()  #:rest (listof transformation?) transformation?)

  (foldl (λ (a b) (transformation (matrix* (transformation-geometric b)
                                           (transformation-geometric a))
                                  (vector-map + (transformation-color a)
                                              (transformation-color b))))
         (identity)
         trans))


;; ------------------------------------------------------------------------

(module+ test
  ;; # Tests
  (require rackunit)

  (define (random-transformation)
    (transformation (matrix [[(random-real -1 1) (random-real -1 1) (random-real -1 1)]
                             [(random-real 0 1) (random-real -1 1) (random-real -1 1)]
                             [(random-real 0 1) (random-real -1 1) (random-real -1 1)]])
                    (vector (random-real -1 1) (random-real -1 1) (random-real -1 1))))

  ;; ## Geometric transformation tests

  ;; ### combining with identity is innocuous

  (define R (random-transformation))
  (check-equal? (combine-transformation (identity) R) R "transformation identity property (1)")
  (check-equal? (combine-transformation R (identity)) R "transformation identity property (2)")

  ;; ### Test invert operations

  (define x (random-real -100 100))
  (define y (random-real -100 100))

  (check-equal? (combine-transformation (translate x y) (translate (- x) (- y))) (identity) "translate invert property")
  (check-true (matrix= (transformation-geometric (combine-transformation (rotate x) (rotate (- x))))
                       (transformation-geometric (identity)))
              "rotate invert property")

  (when (not (or (= x 0) (= y 0)))
    (check-equal? (combine-transformation (scale x y) (scale (/ 1 x) (/ 1 y))) (identity) "rotate invert property"))

  ;;; ### Null operations

  (check-equal? (translate 0 0) (identity) "translate zero is identity")
  (check-equal? (rotate 0) (identity) "rotate zero is identity")
  (check-equal? (scale 1 1) (identity) "scale one is identity")
  )
