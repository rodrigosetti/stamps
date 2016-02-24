#lang racket/base

(require (for-syntax racket/base)
         racket/draw
         racket/class
         racket/contract
         racket/list
         racket/math
         math/matrix
         "adjustments.rkt"
         "random-utils.rkt"
         "color-utils.rkt")

(provide (contract-out [make-square shape-constructor/c]
                       [make-circle shape-constructor/c]
                       [shape/c contract?]
                       [shape-constructor/c contract?]
                       [shape-renderer/c contract?])
         define-shape
         loop-shape)

; Contracts

(define shape-renderer/c
  (-> (is-a?/c dc<%>) (listof procedure?)))

(define shape/c
  (->* () () #:rest (listof adjustment-delta-promise/c) shape-renderer/c))

(define shape-constructor/c
  (->* () () #:rest (listof adjustment-delta-promise/c) shape/c))

; Transform a real between 0 and 1 unto a byte? (exact between 0 and 255)
(define (unit-to-byte v)
  (inexact->exact (round (* 255 v))))

; Helper to apply color adjustments
(define (apply-color-adjustments dc adj)
  (define-values (r g b) (hsb->rgb (adjustment-hue adj)
                                   (adjustment-saturation adj)
                                   (adjustment-brightness adj)))
  (define color (make-color (unit-to-byte r)
                            (unit-to-byte g)
                            (unit-to-byte b)
                            (adjustment-alpha adj)))
  (send dc set-brush color 'solid))

; Shape constructors

(define (make-square . rel-adjs) ; shape constructor
  (λ ctx-adjs ; shape
    (λ (dc) ; shape-renderer
      (define adj (apply combine-adjustment (append ctx-adjs rel-adjs)))
      (define geom (adjustment-geometric adj))
      (define a (matrix* geom (col-matrix [-0.5 -0.5 1])))
      (define b (matrix* geom (col-matrix [-0.5  0.5 1])))
      (define c (matrix* geom (col-matrix [ 0.5  0.5 1])))
      (define d (matrix* geom (col-matrix [ 0.5 -0.5 1])))
      (define points (list (cons (matrix-ref a 0 0) (matrix-ref a 1 0))
                           (cons (matrix-ref b 0 0) (matrix-ref b 1 0))
                           (cons (matrix-ref c 0 0) (matrix-ref c 1 0))
                           (cons (matrix-ref d 0 0) (matrix-ref d 1 0))))

      (apply-color-adjustments dc adj)
      (send dc draw-polygon points)
      '())))

(define (make-circle . rel-adjs) ; shape constructor
  (λ ctx-adjs ; shape
    (λ (dc) ; shape-renderer
          (define adj (apply combine-adjustment (append ctx-adjs rel-adjs)))
          (define geom (adjustment-geometric adj))
          (define orig (matrix* geom (col-matrix [0 0 1])))
          (define start (matrix* geom (col-matrix [1 0 1])))
          (define path (new dc-path%))

          (apply-color-adjustments dc adj)
          (send path move-to
                (matrix-ref start 0 0)
                (matrix-ref start 1 0))
          (for ([a (range -0.1 (* 2 pi) 0.1)])
            (define p (matrix* geom (col-matrix ((cos a) (sin a) 1))))
            (send path line-to
                  (matrix-ref p 0 0)
                  (matrix-ref p 1 0)))
          (send dc draw-path path)
          '())))

; Helper to create shape constructors

; define a shape which is a union of one or more shapes
(define-syntax-rule (union shape-list)
  (λ rel-adjs  ; shape-constructor
    (λ ctx-adjs ; shape
      (define adjs (append ctx-adjs rel-adjs))
      (λ (dc) ; shape-renderer
        ; list of shape-renderers, from list of shapes applied to adjs
        (map (λ (s) (apply s adjs)) shape-list)))))

; creates a shape-constructor that randomly selects a shape to render
; every time it renders
; (-> (listof (cons/c real? shape/c)) shape-constructor/c)
(define (prob-shape weighted-shapes)
  (λ rel-adjs  ; shape-constructor
    ; construct shapes
    ; (define weighted-shapes
    ;   (map (λ (wsc) (cons (car wsc) (apply (cdr wsc) rel-adjs)))
    ;        weighted-shape-cons))

    (λ ctx-adjs ; shape
      (define adjs (append ctx-adjs rel-adjs))

      (λ (dc) ; shape-renderer
        (define s (random-choice weighted-shapes))
        ((apply s adjs) dc)))))

; shortcut for defining a shape union constructor with arguments and bind it to name
(define-syntax (define-shape stx)
  (syntax-case stx (=>)
    [(_ name (p => shape ...) ...)
     #'(define name
         (prob-shape (list (cons p ((union (list shape ...)))) ...)))]

    [(_ (name arg ...) (p => shape ...) ...)
     #'(define (name arg ...)
         (prob-shape (list (cons p ((union (list shape ...)))) ...)))]

    [(_ (name arg ...) shape ...) #'(define (name arg ...) (union (list shape ...)))]
    [(_ name shape ...)           #'(define name (union (list shape ...)))]))

; evaluate shape union body in a for loop and then union all together
(define-syntax-rule (loop-shape (for-clause ...) shape ...)
  (union (for/list (for-clause ...)
           ((union (list shape ...))))))
