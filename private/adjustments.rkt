#lang racket/base

; defines the adjustment data structure and combinators

(module core typed/racket/base

  (require racket/function
           math/matrix
           racket/match
           "common.rkt")

  (provide identity
           combine-adjustment
           geometric-delta
           target-color-delta
           color-delta
           identity-delta
           change-to-target ; debug
           change-%
           change-hue
           change-hue-target
           AdjustmentDelta
           (struct-out adjustment))

  ;; adjustment definition

  (struct geometric-delta ([matrix : (Matrix Real)]) #:transparent)

  (struct color-delta ([hue        : Real]
                       [saturation : Real]
                       [brightness : Real]
                       [alpha      : Real])
    #:transparent)

  (struct target-color-delta color-delta ([target-hue        : Real]
                                          [target-saturation : Real]
                                          [target-brightness : Real]
                                          [target-alpha      : Real])
    #:transparent)

  (define-type AdjustmentDelta (U geometric-delta
                                  color-delta
                                  target-color-delta))

  (struct adjustment ([geometric  : (Matrix Real)]
                      [hue        : Real]
                      [saturation : Real]
                      [brightness : Real]
                      [alpha      : Real])
    #:transparent)

  ;; adjustment constructors

  ; identity adjustment:
  ; identity transformation, black, 0-saturation, 0-brightness, 1-alpha
  (: identity adjustment)
  (define identity
    (adjustment (identity-matrix 3) 0 0 0 1))

  (: identity-delta (-> geometric-delta))
  (define identity-delta
    (const (geometric-delta (identity-matrix 3))))

  (: change-to-target (-> Real Real Real Real))
  (define (change-to-target val % target)
    ; change val by % towards target
    ; % is always treated as positive
    (- val (* (abs %)
              (- val target))))

  ; if target is undefined, return val changed % towards 0 or 1, depending if % is negative or positive (respectively).
  ; if target is defined, return val changed % towards target if % is positive, otherwise return val changed to 0 or 1,
  ; whichever is closest.
  (: change-% (case-> (-> Real Real Real) (-> Real Real Real Real)))
  (define change-%
    (case-lambda
      ([val %]
       (change-to-target val % (if (< % 0) 0 1)))
      ([val % target]
       (change-to-target val % (if (< % 0)
                                   (if (> 1/2 (- 1 val))
                                       1
                                       0)
                                   target)))))


  ; Change hue modulo 360
  (: change-hue (-> Real Real Real))
  (define (change-hue current delta)
    (define new-hue (float-modulo (+ current delta) 360))
    (+ new-hue
       (if (< new-hue 0) 360 0)))

  ; Change hue modulo 360 with target
  (: change-hue-target (-> Real Real Real Real))
  (define (change-hue-target current % target)
    (define-values (left right)
      (if (< target current)
        (values (- current target)
                (+ (- 360 current) target))
        (values (+ (- 360 target) current)
                (- target current))))
    (define delta (* % (if (< % 0) left right)))
    (change-hue current delta))

  ;; adjustments combinators
  (: combine-adjustment (->* (adjustment) () #:rest (-> AdjustmentDelta) adjustment))
  (define (combine-adjustment adj . deltas)
    (: folder (-> AdjustmentDelta adjustment adjustment))
    (define (folder delta adj)
      (define matrix (adjustment-geometric adj))
      (define hue (adjustment-hue adj))
      (define saturation (adjustment-saturation adj))
      (define brightness (adjustment-brightness adj))
      (define alpha (adjustment-alpha adj))

      (match delta
        [(geometric-delta m) (adjustment (matrix* matrix m) hue saturation brightness alpha)]
        [(color-delta h s b a) (adjustment matrix
                                           (change-hue hue h)
                                           (change-% saturation s)
                                           (change-% brightness b)
                                           (change-% alpha a))]
        [(target-color-delta h s b a th ts tb ta) (adjustment matrix
                                                              (change-hue-target hue h th)
                                                              (change-% saturation s ts)
                                                              (change-% brightness b tb)
                                                              (change-% alpha ta))]))
    (: eval-adj-delta-promise (-> (-> AdjustmentDelta) AdjustmentDelta))
    (define (eval-adj-delta-promise adj-delta-promise)
      (adj-delta-promise))

    (foldl folder
           adj
           (map eval-adj-delta-promise deltas))) ; apply all promises to get the deltas
  )

(require (for-syntax racket/base)
         'core
         (only-in racket/function const thunk)
         racket/contract
         math/matrix
         racket/match
         "linalg-utils.rkt")

(provide identity
         combine-adjustment
         AdjustmentDelta
         (struct-out adjustment)
         rotate
         scale
         translate
         flip
         hue
         saturation
         brightness
         alpha)

(define-syntax-rule (rotate angle)
  (const (geometric-delta (rotation-matrix angle))))

(define-syntax-rule (flip angle)
  (const (geometric-delta (reflection-matrix angle))))

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
     #'(thunk (target-color-delta 0 v 0 0  0 t 0 0))]))

(define-syntax (brightness stx)
  (syntax-case stx (..)
    [(_ v)
     #'(thunk (color-delta 0 0 v 0))]
    [(_ v t)
     #'(thunk (target-color-delta 0 0 v 0  0 0 t 0))]))

(define-syntax (alpha stx)
  (syntax-case stx (..)
    [(_ v)
     #'(thunk (color-delta 0 0 0 v))]
    [(_ v t)
     #'(thunk (target-color-delta 0 0 0 v  0 0 0 t))]))

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

  (check-adjustment-=? (combine-adjustment identity)
                       identity
                       "combine adjustment with no arguments")

  
  (test-case "combining with identity is innocuous"
    (define R (random-geometric-delta))
    (check-equal? (combine-adjustment identity identity-delta R)
                  (combine-adjustment identity R)
                  "adjustment identity property (1)")
    (check-equal? (combine-adjustment identity R identity-delta)
                  (combine-adjustment identity R)
                  "adjustment identity property (2)"))


  (test-case "invert operations"
    (define x (random-real -10 10))
    (define y (random-real -10 10))

    (check-adjustment-=? (combine-adjustment identity (translate x y) (translate (- x) (- y)))
                         identity
                         "translate invert property")

    (check-adjustment-=? (combine-adjustment identity (rotate x) (rotate (- x)))
                         identity
                         "rotate invert property")

    (define sx (random-real 1 5))
    (define sy (random-real 1 5))

    (check-adjustment-=? (combine-adjustment identity (scale sx sy) (scale (/ 1 sx) (/ 1 sy)))
                         identity
                         "scale invert property"))


  (test-case "null operations"

    (check-equal? ((translate 0 0)) (identity-delta) "translate zero is identity")
    (check-equal? ((rotate 0)) (identity-delta) "rotate zero is identity")
    (check-equal? ((scale 1 1)) (identity-delta) "scale one is identity"))


  (test-case "change-hue and change-hue-target"

    (check-= 300 (change-hue 200  100) epsilon)
    (check-= 140 (change-hue 300  200) epsilon)
    (check-= 200 (change-hue 300 -100) epsilon)
    (check-= 260 (change-hue 100 -200) epsilon)

    (check-= 100 (change-hue-target 200 1  100) epsilon)
    (check-= 200 (change-hue-target 300 1  200) epsilon)
    (check-= 260 (change-hue-target 300 1 -100) epsilon)
    (check-= 160 (change-hue-target 100 1 -200) epsilon)

    (check-= 100 (change-hue-target 200 -1  100) epsilon)
    (check-= 200 (change-hue-target 300 -1  200) epsilon)
    (check-= 260 (change-hue-target 300 -1 -100) epsilon)
    (check-= 160 (change-hue-target 100 -1 -200) epsilon)

    (check-= 330 (change-hue-target 200 1/2  100) epsilon)
    (check-=  70 (change-hue-target 300 1/2  200) epsilon)

    (check-= 150 (change-hue-target 200 -1/2  100) epsilon)
    (check-= 250 (change-hue-target 300 -1/2  200) epsilon))


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
    (check-= 0 (change-% 0 -0.5 20) epsilon))


  (test-case
   "combine-adjustment"

   ; Simple cases - starting with sat=0 and single adj
   (check-= 0.5
            (adjustment-saturation
             (combine-adjustment identity (saturation 0.5)))
            epsilon)
   (check-= 1
            (adjustment-saturation
             (combine-adjustment identity (saturation 1)))
            epsilon)

  ; Double adjustments - increasing
  (check-= 0.75
            (adjustment-saturation
             (combine-adjustment identity
                                 (saturation 0.5)
                                 (saturation 0.5)))
            epsilon)

  ; Double adjustments - descreasing
  (check-= 0.5
            (adjustment-saturation
             (combine-adjustment identity
                                 (saturation 1)
                                 (saturation -0.5)))
            epsilon)

  ))
