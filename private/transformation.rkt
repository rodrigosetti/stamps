#lang racket/base

; defines the transformation data structure and combinators

(require racket/contract
         racket/vector
         racket/function
         math/matrix
         "linalg-utils.rkt"
         "random-utils.rkt")

(provide (contract-out [rotate (-> real? transformation-promise/c)]
                       [scale (->* (real?) (real?) transformation-promise/c)]
                       [translate (-> real? real? transformation-promise/c)]
                       [hue (-> real? transformation-promise/c)]
                       [saturation (-> real? transformation-promise/c)]
                       [brightness (-> real? transformation-promise/c)]
                       [identity transformation-promise/c]
                       [combine-transformation (->* () ()  #:rest (listof transformation-promise/c) transformation?)])
         transformation?
         transformation-promise/c
         transformation-geometric
         transformation-color)

;; transformation definition

; - geometric: matrix?
; - color: (vector/c real? real? real?) -- HSV deltas
(struct transformation (geometric color) #:transparent)

;; Transformations constructors

(define transformation-promise/c
  (-> transformation?))

(define (transformation-promise matrix hsb)
  (thunk (transformation matrix hsb)))

(define (geometric-transformation-promise matrix)
  (transformation-promise matrix #[0 0 0]))

(define (color-transformation-promise hsb)
  (transformation-promise (identity-matrix 3) hsb))

(define identity
  (geometric-transformation-promise (identity-matrix 3)))

(define (rotate theta)
  (geometric-transformation-promise (rotation-matrix theta)))

(define (scale sx [sy sx])
  (geometric-transformation-promise (scaling-matrix sx sy)))

(define (translate tx ty)
  (geometric-transformation-promise (translation-matrix tx ty)))

(define (hue h)
  (color-transformation-promise (vector h 0 0)))

(define (saturation s)
  (color-transformation-promise (vector 0 s 0)))

(define (brightness b)
  (color-transformation-promise (vector 0 0 b)))

;; Transformations combinators
(define (combine-transformation . trans)
  (foldl (λ (a b) (transformation (matrix* (transformation-geometric b)
                                           (transformation-geometric a))
                                  (vector-map + (transformation-color a)
                                              (transformation-color b))))
         (identity)
         (map (λ (t) (t)) trans))) ; apply all promises

;; ------------------------------------------------------------------------

(module+ test
  ;; # Tests
  (require rackunit)

  (define (random-transformation)
    (transformation-promise (matrix [[(random-real -1 1) (random-real -1 1) (random-real -1 1)]
                                     [(random-real 0 1) (random-real -1 1) (random-real -1 1)]
                                     [(random-real 0 1) (random-real -1 1) (random-real -1 1)]])
                            (vector (random-real -1 1) (random-real -1 1) (random-real -1 1))))

  ;; ## Geometric transformation tests

  ;; ### combining with identity is innocuous

  (define R (random-transformation))
  (check-equal? (combine-transformation identity R) (R) "transformation identity property (1)")
  (check-equal? (combine-transformation R identity) (R) "transformation identity property (2)")

  ;; ### Test invert operations

  (define x (random-real -100 100))
  (define y (random-real -100 100))

  (check matrix=
         (transformation-geometric (combine-transformation (translate x y) (translate (- x) (- y))))
         (transformation-geometric (identity))
         "translate invert property")
  (check matrix=
         (transformation-geometric (combine-transformation (rotate x) (rotate (- x))))
         (transformation-geometric (identity))
         "rotate invert property")

  (when (not (or (= x 0) (= y 0)))
    (check matrix=
           (transformation-geometric (combine-transformation (scale x y) (scale (/ 1 x) (/ 1 y))))
           (transformation-geometric (identity))
           "rotate invert property"))

  ;;; ### Null operations

  (check-equal? ((translate 0 0)) (identity) "translate zero is identity")
  (check-equal? ((rotate 0)) (identity) "rotate zero is identity")
  (check-equal? ((scale 1 1)) (identity) "scale one is identity"))
