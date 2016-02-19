#lang racket/base

; defines the transformation data structure and combinators

(require (for-syntax racket/base)
         racket/contract
         racket/vector
         racket/function
         math/matrix
         "linalg-utils.rkt"
         "random-utils.rkt")

(provide (contract-out
          [identity transformation-promise/c]
          [combine-transformation (->* () ()  #:rest (listof transformation-promise/c) transformation?)])
         rotate
         scale
         translate
         hue
         saturation
         brightness
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

(define (transformation-const matrix hsb)
  (const (transformation matrix hsb)))

(define (geometric-transformation-const matrix)
  (transformation-const matrix #[0 0 0]))

(define (color-transformation-const hsb)
  (transformation-const (identity-matrix 3) hsb))

(define-syntax-rule (transformation-thunk matrix hsb)
  (thunk (transformation matrix hsb)))

(define-syntax-rule (geometric-transformation-thunk matrix)
  (transformation-thunk matrix #[0 0 0]))

(define-syntax-rule (color-transformation-thunk hsb)
  (transformation-thunk (identity-matrix 3) hsb))

(define identity
  (geometric-transformation-const (identity-matrix 3)))

(define-syntax (rotate stx)
  (syntax-case stx (..)
    [(_ v)
      #'(geometric-transformation-const (rotation-matrix v))]
    [(_ min .. max)
      #'(geometric-transformation-thunk (rotation-matrix (random-real min max)))]))

(define-syntax (scale stx)
  (syntax-case stx (..)
    [(_ x)
     #'(geometric-transformation-const (scaling-matrix x x))]
    [(_ x y)
     #'(geometric-transformation-const (scaling-matrix x y))]
    [(_ x1 .. x2)
     #'(geometric-transformation-thunk (let ([n (random-real x1 x2)]) (scaling-matrix n n)))]
    [(_ x1 .. x2 y)
     #'(geometric-transformation-thunk (scaling-matrix (random-real x1 x2) y))]
    [(_ x y1 .. y2)
     #'(geometric-transformation-thunk (scaling-matrix x (random-real y1 y2)))]
    [(_ x1 .. x2 y1 .. y2)
     #'(geometric-transformation-thunk (scaling-matrix (random-real x1 x2)
                                                       (random-real y1 y2)))]))

(define-syntax (translate stx)
  (syntax-case stx (..)
    [(_ x)
     #'(geometric-transformation-const (translation-matrix x x))]
    [(_ x y)
     #'(geometric-transformation-const (translation-matrix x y))]
    [(_ x1 .. x2)
     #'(geometric-transformation-thunk (let ([n (random-real x1 x2)]) (translation-matrix n n)))]
    [(_ x1 .. x2 y)
     #'(geometric-transformation-thunk (translation-matrix (random-real x1 x2) y))]
    [(_ x y1 .. y2)
     #'(geometric-transformation-thunk (translation-matrix x (random-real y1 y2)))]
    [(_ x1 .. x2 y1 .. y2)
     #'(geometric-transformation-thunk (translation-matrix (random-real x1 x2)
                                                       (random-real y1 y2)))]))

(define-syntax (hue stx)
  (syntax-case stx (..)
    [(_ v)
     #'(color-transformation-const (vector v 0 0))]
    [(_ min .. max)
     #'(color-transformation-thunk (vector (random-real min max) 0 0))]))

(define-syntax (saturation stx)
  (syntax-case stx (..)
    [(_ v)
     #'(color-transformation-const (vector 0 v 0))]
    [(_ min .. max)
     #'(color-transformation-thunk (vector 0 (random-real min max) 0))]))

(define-syntax (brightness stx)
  (syntax-case stx (..)
    [(_ v)
     #'(color-transformation-const (vector 0 0 v))]
    [(_ min .. max)
     #'(color-transformation-thunk (vector 0 0 (random-real min max)))]))

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
    (transformation-const (matrix [[(random-real -1 1) (random-real -1 1) (random-real -1 1)]
                                   [(random-real 0 1) (random-real -1 1) (random-real -1 1)]
                                   [(random-real 0 1) (random-real -1 1) (random-real -1 1)]])
                          (vector (random-real -1 1) (random-real -1 1) (random-real -1 1))))

  ;; ## Geometric transformation tests

  ;; ### combining with identity is innocuous

  (define R (random-transformation))
  (check-equal? (combine-transformation identity R) (R) "transformation identity property (1)")
  (check-equal? (combine-transformation R identity) (R) "transformation identity property (2)")

  ;; ### Test invert operations

  (define x (random-real -10 10))
  (define y (random-real -10 10))

  (check matrix=
         (transformation-geometric (combine-transformation (translate x y) (translate (- x) (- y))))
         (transformation-geometric (identity))
         "translate invert property")
  (check matrix=
         (transformation-geometric (combine-transformation (rotate x) (rotate (- x))))
         (transformation-geometric (identity))
         "rotate invert property")

  (define sx (random-real 1 5))
  (define sy (random-real 1 5))

  (check matrix=
          (transformation-geometric (combine-transformation (scale sx sy) (scale (/ 1 sx) (/ 1 sy))))
          (transformation-geometric (identity))
          "scale invert property")

  ;;; ### Null operations

  (check-equal? ((translate 0 0)) (identity) "translate zero is identity")
  (check-equal? ((rotate 0)) (identity) "rotate zero is identity")
  (check-equal? ((scale 1 1)) (identity) "scale one is identity"))
