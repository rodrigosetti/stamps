#lang racket/base

; defines the adjustment data structure and combinators

(require (for-syntax racket/base)
         racket/contract
         racket/function
         math/matrix
         racket/match
         "linalg-utils.rkt")

(provide (contract-out
          [identity adjustment-delta-promise/c]
          [combine-adjustment (->* () ()  #:rest (listof adjustment-delta-promise/c) adjustment?)])
         rotate
         scale
         translate
         hue
         saturation
         brightness
         adjustment?
         adjustment-delta-promise/c
         adjustment-geometric
         adjustment-hue
         adjustment-saturation
         adjustment-brightness
         adjustment-alpha)

;; adjustment definition

(struct geometric-delta (matrix) #:transparent)
(struct color-delta (hue saturation brightness alpha) #:transparent)
(struct target-color-delta color-delta (target-hue target-saturation target-brightness target-alpha) #:transparent)

(struct adjustment (geometric hue saturation brightness alpha) #:transparent)

;; adjustment constructors

(define adjustment-delta-promise/c
  (-> (or/c geometric-delta? color-delta? target-color-delta?)))

(define identity
  (const (geometric-delta (identity-matrix 3))))

(define-syntax (rotate stx)
  (syntax-case stx (..)
    [(_ v)
     #'(const (geometric-delta (rotation-matrix v)))]))

(define-syntax (scale stx)
  (syntax-case stx (..)
    [(_ x)
     #'(thunk (geometric-delta (scaling-matrix x x)))]
    [(_ x y)
     #'(const (geometric-delta (scaling-matrix x y)))]))

  (define-syntax (translate stx)
    (syntax-case stx (..)
      [(_ x)
       #'(thunk (geometric-delta (translation-matrix x x)))]
      [(_ x y)
       #'(thunk (geometric-delta (translation-matrix x y)))]))

(define-syntax (hue stx)
  (syntax-case stx (..)
    [(_ v)
     #'(thunk (color-delta v 0 0 0))]
    [(_ v t)
     #'(thunk (target-color-delta v 0 0 0  t 0 0 0))]))

(define-syntax (saturation stx)
  (syntax-case stx (..)
    [(_ v)
     #'(thunk (color-delta 0 v 0 0))]
    [(_ v t)
     #'(thunk (color-delta 0 v 0 0  0 t 0 0))]))

(define-syntax (brightness stx)
  (syntax-case stx (..)
    [(_ v)
     #'(thunk (color-delta 0 0 v 0))]
    [(_ v t)
     #'(thunk (color-delta 0 0 v 0  0 0 t 0))]))

(define-syntax (alpha stx)
  (syntax-case stx (..)
    [(_ v)
     #'(thunk (color-delta 0 0 0 v))]
    [(_ v t)
     #'(thunk (color-delta 0 0 0 v  0 0 0 t))]))

; if target is undefined, return val changed % towards 0 or 1, depending if % is negative or positive (respectively).
; if target is defined, return val changed % towards target if % is positive, otherwise return val changed to 0 or 1,
; whichever is closest.
(define (change-% val % [target #f])
  (define real-target (if (< % 0)
                          (if (eq? #f target)
                              0
                              (if (> 1/2 (- 1 val))
                                  1
                                  0))
                          (if (eq? #f target)
                              1
                              target)))

  (- val (* (abs %) (- val real-target))))


;; adjustments combinators
(define (combine-adjustment . trans)
  (foldl (λ (delta adj)
           (define matrix (adjustment-geometric adj))
           (define hue (adjustment-hue adj))
           (define saturation (adjustment-saturation adj))
           (define brightness (adjustment-brightness adj))
           (define alpha (adjustment-alpha adj))

           (match delta
             [(geometric-delta m) (adjustment (matrix* matrix m) hue saturation brightness alpha)]
             [(color-delta h s b a) (adjustment matrix
                                                (change-% hue h)
                                                (change-% saturation s)
                                                (change-% brightness b)
                                                (change-% alpha a))]
             [(target-color-delta h s b a th ts tb ta) (adjustment matrix
                                                                   (change-% hue h th)
                                                                   (change-% saturation s ts)
                                                                   (change-% brightness b tb)
                                                                   (change-% alpha ta))]))

         ; identity adjustment:
         ; identity transformation, black, 0-saturation, 0-brightness, 1-alpha
         (adjustment (identity-matrix 3) 0 0 0 1)

         (map (λ (t) (t)) trans))) ; apply all promises to get the deltas

;; ------------------------------------------------------------------------

(module+ test
  ;; # Tests
  (require rackunit
           "random-utils.rkt")


  (define (random-geometric-delta)
    (const (geometric-delta (matrix [[(random-real -1 1) (random-real -1 1) (random-real -1 1)]
                                     [(random-real 0 1) (random-real -1 1) (random-real -1 1)]
                                     [(random-real 0 1) (random-real -1 1) (random-real -1 1)]]))))

  (define epsilon 1e-10)

  (define (check-adjustment-=? adj1 adj2 message)
    (check (λ (x y) (< (matrix-relative-error (adjustment-geometric x) (adjustment-geometric y)) epsilon))
           adj1 adj1
           (string-append message " : geometric should be equal"))
    (check-= (adjustment-hue adj1) (adjustment-hue adj2) epsilon (string-append message " : hue should be equal"))
    (check-= (adjustment-saturation adj1) (adjustment-saturation adj2) epsilon (string-append message " : saturation should be equal"))
    (check-= (adjustment-brightness adj1) (adjustment-brightness adj2) epsilon (string-append message " : brightness should be equal")))


  (test-case "combining with identity is innocuous"
    (define R (random-geometric-delta))
    (check-equal? (combine-adjustment identity R) (combine-adjustment R) "adjustment identity property (1)")
    (check-equal? (combine-adjustment R identity) (combine-adjustment R) "adjustment identity property (2)"))

  (test-case "invert operations"
    (define x (random-real -10 10))
    (define y (random-real -10 10))

    (check-adjustment-=? (combine-adjustment (translate x y) (translate (- x) (- y)))
                         (combine-adjustment)
                         "translate invert property")

    (check-adjustment-=? (combine-adjustment (rotate x) (rotate (- x)))
                         (combine-adjustment)
                         "rotate invert property")

    (define sx (random-real 1 5))
    (define sy (random-real 1 5))

    (check-adjustment-=? (combine-adjustment (scale sx sy) (scale (/ 1 sx) (/ 1 sy)))
                         (combine-adjustment)
                         "scale invert property"))

  (test-case "null operations"

    (check-equal? ((translate 0 0)) (identity) "translate zero is identity")
    (check-equal? ((rotate 0)) (identity) "rotate zero is identity")
    (check-equal? ((scale 1 1)) (identity) "scale one is identity"))


  (test-case "change-%"

    (check-= 0.5 (change-% 1 -0.5) epsilon)
    (check-= 0.5 (change-% 0 0.5) epsilon)
    (check-= 0.75 (change-% 0.5 0.5) epsilon)
    (check-= 0.25 (change-% 0.5 -0.5) epsilon)
    (check-= 1 (change-% 1 0.5) epsilon)
    (check-= 0 (change-% 0 -0.5) epsilon)

    (check-= 0.65 (change-% 0.5 0.5 0.8) epsilon)
    (check-= 0.8 (change-% 0.8 0.5 0.8) epsilon)
    (check-= 0.9 (change-% 0.8 -0.5 0.8) epsilon)
    (check-= 0.6 (change-% 0.4 0.5 0.8) epsilon)
    (check-= 0.2 (change-% 0.4 -0.5 0.8) epsilon)
    (check-= 1 (change-% 1 -0.5 20) epsilon)
    (check-= 0 (change-% 0 -0.5 20) epsilon)))
