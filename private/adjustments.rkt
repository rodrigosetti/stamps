#lang racket/base

; defines the adjustment data structure and combinators

(require (for-syntax racket/base)
         racket/contract
         racket/vector
         racket/function
         math/matrix
         "linalg-utils.rkt"
         "random-utils.rkt")

(provide (contract-out
          [identity adjustment-promise/c]
          [combine-adjustment (->* () ()  #:rest (listof adjustment-promise/c) adjustment?)])
         rotate
         scale
         translate
         hue
         saturation
         brightness
         adjustment?
         adjustment-promise/c
         adjustment-geometric
         adjustment-color)

;; adjustment definition

; - geometric: matrix?
; - color: (vector/c real? real? real?) -- HSV deltas
(struct adjustment (geometric color) #:transparent)

;; adjustment constructors

(define adjustment-promise/c
  (-> adjustment?))

(define (adjustment-const matrix hsb)
  (const (adjustment matrix hsb)))

(define (geometric-adjustment-const matrix)
  (adjustment-const matrix #[0 0 0]))

(define (color-adjustment-const hsb)
  (adjustment-const (identity-matrix 3) hsb))

(define-syntax-rule (adjustment-thunk matrix hsb)
  (thunk (adjustment matrix hsb)))

(define-syntax-rule (geometric-adjustment-thunk matrix)
  (adjustment-thunk matrix #[0 0 0]))

(define-syntax-rule (color-adjustment-thunk hsb)
  (adjustment-thunk (identity-matrix 3) hsb))

(define identity
  (geometric-adjustment-const (identity-matrix 3)))

(define-syntax (rotate stx)
  (syntax-case stx (..)
    [(_ v)
      #'(geometric-adjustment-const (rotation-matrix v))]
    [(_ min .. max)
      #'(geometric-adjustment-thunk (rotation-matrix (random-real min max)))]))

(define-syntax (scale stx)
  (syntax-case stx (..)
    [(_ x)
     #'(geometric-adjustment-const (scaling-matrix x x))]
    [(_ x y)
     #'(geometric-adjustment-const (scaling-matrix x y))]
    [(_ x1 .. x2)
     #'(geometric-adjustment-thunk (let ([n (random-real x1 x2)]) (scaling-matrix n n)))]
    [(_ x1 .. x2 y)
     #'(geometric-adjustment-thunk (scaling-matrix (random-real x1 x2) y))]
    [(_ x y1 .. y2)
     #'(geometric-adjustment-thunk (scaling-matrix x (random-real y1 y2)))]
    [(_ x1 .. x2 y1 .. y2)
     #'(geometric-adjustment-thunk (scaling-matrix (random-real x1 x2)
                                                       (random-real y1 y2)))]))

(define-syntax (translate stx)
  (syntax-case stx (..)
    [(_ x)
     #'(geometric-adjustment-const (translation-matrix x x))]
    [(_ x y)
     #'(geometric-adjustment-const (translation-matrix x y))]
    [(_ x1 .. x2)
     #'(geometric-adjustment-thunk (let ([n (random-real x1 x2)]) (translation-matrix n n)))]
    [(_ x1 .. x2 y)
     #'(geometric-adjustment-thunk (translation-matrix (random-real x1 x2) y))]
    [(_ x y1 .. y2)
     #'(geometric-adjustment-thunk (translation-matrix x (random-real y1 y2)))]
    [(_ x1 .. x2 y1 .. y2)
     #'(geometric-adjustment-thunk (translation-matrix (random-real x1 x2)
                                                       (random-real y1 y2)))]))

(define-syntax (hue stx)
  (syntax-case stx (..)
    [(_ v)
     #'(color-adjustment-const (vector v 0 0))]
    [(_ min .. max)
     #'(color-adjustment-thunk (vector (random-real min max) 0 0))]))

(define-syntax (saturation stx)
  (syntax-case stx (..)
    [(_ v)
     #'(color-adjustment-const (vector 0 v 0))]
    [(_ min .. max)
     #'(color-adjustment-thunk (vector 0 (random-real min max) 0))]))

(define-syntax (brightness stx)
  (syntax-case stx (..)
    [(_ v)
     #'(color-adjustment-const (vector 0 0 v))]
    [(_ min .. max)
     #'(color-adjustment-thunk (vector 0 0 (random-real min max)))]))

;; adjustments combinators
(define (combine-adjustment . trans)
  (foldl (λ (a b) (adjustment (matrix* (adjustment-geometric b)
                                       (adjustment-geometric a))
                              (vector-map + (adjustment-color a)
                                          (adjustment-color b))))
         (identity)
         (map (λ (t) (t)) trans))) ; apply all promises

;; ------------------------------------------------------------------------

(module+ test
  ;; # Tests
  (require rackunit)

  (define (random-adjustment)
    (adjustment-const (matrix [[(random-real -1 1) (random-real -1 1) (random-real -1 1)]
                               [(random-real 0 1) (random-real -1 1) (random-real -1 1)]
                               [(random-real 0 1) (random-real -1 1) (random-real -1 1)]])
                      (vector (random-real -1 1) (random-real -1 1) (random-real -1 1))))

  ;; ## Geometric adjustment tests

  ;; ### combining with identity is innocuous

  (define R (random-adjustment))
  (check-equal? (combine-adjustment identity R) (R) "adjustment identity property (1)")
  (check-equal? (combine-adjustment R identity) (R) "adjustment identity property (2)")

  ;; ### Test invert operations

  (define x (random-real -10 10))
  (define y (random-real -10 10))

  (check matrix=
         (adjustment-geometric (combine-adjustment (translate x y) (translate (- x) (- y))))
         (adjustment-geometric (identity))
         "translate invert property")
  (check matrix=
         (adjustment-geometric (combine-adjustment (rotate x) (rotate (- x))))
         (adjustment-geometric (identity))
         "rotate invert property")

  (define sx (random-real 1 5))
  (define sy (random-real 1 5))

  (check matrix=
          (adjustment-geometric (combine-adjustment (scale sx sy) (scale (/ 1 sx) (/ 1 sy))))
          (adjustment-geometric (identity))
          "scale invert property")

  ;;; ### Null operations

  (check-equal? ((translate 0 0)) (identity) "translate zero is identity")
  (check-equal? ((rotate 0)) (identity) "rotate zero is identity")
  (check-equal? ((scale 1 1)) (identity) "scale one is identity"))
