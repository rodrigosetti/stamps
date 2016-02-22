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
         adjustment-hue
         adjustment-saturation
         adjustment-brightness)

;; adjustment definition

; - geometric: matrix?
; - hue: real?
; - saturation: real?
; - brightness: real?
(struct adjustment (geometric hue saturation brightness) #:transparent)

;; adjustment constructors

(define adjustment-promise/c
  (-> adjustment?))

(define (adjustment-const [matrix (identity-matrix 3)] [h 0] [s 0] [b 0])
  (const (adjustment matrix h s b)))

(define (geometric-adjustment-const [matrix (identity-matrix 3)])
  (adjustment-const matrix))

(define (color-adjustment-const [h 0] [s 0] [b 0])
  (adjustment-const (identity-matrix 3) h s b))

(define-syntax-rule (adjustment-thunk matrix h s b)
  (thunk (adjustment matrix h s b)))

(define-syntax-rule (geometric-adjustment-thunk matrix)
  (adjustment-thunk matrix 0 0 0))

(define-syntax-rule (color-adjustment-thunk h s b)
  (adjustment-thunk (identity-matrix 3) h s b))

(define identity
  (geometric-adjustment-const))

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
                              ;; TODO: implement correct color adjustment logic
                              ;; see http://www.contextfreeart.org/mediawiki/index.php/Shape_adjustment#Color_Adjustments
                              (+ (adjustment-hue a) (adjustment-hue b))
                              (+ (adjustment-saturation a) (adjustment-saturation b))
                              (+ (adjustment-brightness a) (adjustment-brightness b))))
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
                      (random-real -360 360)
                      (random-real -1 1)
                      (random-real -1 1)))

  (define epsilon 1e-10)

  (define (check-adjustment-=? adj1 adj2 message)
    (check (λ (x y) (< (matrix-relative-error (adjustment-geometric x) (adjustment-geometric y)) epsilon))
           adj1 adj1
           (string-append message " : geometric should be equal"))
    (check-= (adjustment-hue adj1) (adjustment-hue adj2) epsilon (string-append message " : hue should be equal"))
    (check-= (adjustment-saturation adj1) (adjustment-saturation adj2) epsilon (string-append message " : saturation should be equal"))
    (check-= (adjustment-brightness adj1) (adjustment-brightness adj2) epsilon (string-append message " : brightness should be equal")))

  ;; ## Geometric adjustment tests

  ;; ### combining with identity is innocuous

  (define R (random-adjustment))
  (check-equal? (combine-adjustment identity R) (R) "adjustment identity property (1)")
  (check-equal? (combine-adjustment R identity) (R) "adjustment identity property (2)")

  ;; ### Test invert operations

  (define x (random-real -10 10))
  (define y (random-real -10 10))

  (check-adjustment-=? (combine-adjustment (translate x y) (translate (- x) (- y)))
                       (identity)
                       "translate invert property")

  (check-adjustment-=? (combine-adjustment (rotate x) (rotate (- x)))
                       (identity)
                       "rotate invert property")

  (define sx (random-real 1 5))
  (define sy (random-real 1 5))

  (check-adjustment-=? (combine-adjustment (scale sx sy) (scale (/ 1 sx) (/ 1 sy)))
                       (identity)
                       "scale invert property")

  ;;; ### Null operations

  (check-equal? ((translate 0 0)) (identity) "translate zero is identity")
  (check-equal? ((rotate 0)) (identity) "rotate zero is identity")
  (check-equal? ((scale 1 1)) (identity) "scale one is identity"))
