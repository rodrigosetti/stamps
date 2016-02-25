#lang racket/base

(module core typed/racket/base
  (require typed/racket/draw
           typed/racket/class
           racket/list
           racket/math
           math/matrix
           "adjustments.rkt"
           "random-utils.rkt"
           "color-utils.rkt"
           "common.rkt")

  (provide make-square
           make-circle
           prob-shape
           ShapeConstructor
           ShapeRenderer
           Shape)

  ; Transform a real between 0 and 1 unto a byte? (exact between 0 and 255)
  (: unit-to-byte (-> Real Byte))
  (define (unit-to-byte v)
    (define r (exact-round (* 255 v)))
    (assert (and (<= 0 r) (<= r 255)))
    r)

  ; Helper to apply color adjustments
  (: apply-color-adjustments (-> (Instance Dc<%>) adjustment Void))
  (define (apply-color-adjustments dc adj)
    (define-values (r g b) (hsb->rgb (adjustment-hue adj)
                                    (adjustment-saturation adj)
                                    (adjustment-brightness adj)))
    (define color (make-object color%
                              (unit-to-byte r)
                              (unit-to-byte g)
                              (unit-to-byte b)
                              (adjustment-alpha adj)))
    (send dc set-brush color 'solid))

  ; Shape constructors

  (define square-points (matrix [[-1/2 -1/2 1/2  1/2]
                                [-1/2  1/2 1/2 -1/2]
                                [   1    1   1    1]]))

  (define-type ShapeRenderer (-> (Instance Dc<%>) (Sequenceof ShapeRenderer)))
  (define-type Shape (-> adjustment ShapeRenderer))
  (define-type ShapeConstructor (->* () () #:rest (-> AdjustmentDelta) Shape))

  (: make-square ShapeConstructor)
  (define (make-square . rel-adjs) ; shape constructor
    (λ (ctx-adj) ; shape
      (λ (dc) ; shape-renderer
        (define adj (apply combine-adjustment ctx-adj rel-adjs))
        (define geom (adjustment-geometric adj))
        (define x (matrix* geom square-points))
        (define points (list (cons (matrix-ref x 0 0) (matrix-ref x 1 0))
                            (cons (matrix-ref x 0 1) (matrix-ref x 1 1))
                            (cons (matrix-ref x 0 2) (matrix-ref x 1 2))
                            (cons (matrix-ref x 0 3) (matrix-ref x 1 3))))

        (apply-color-adjustments dc adj)
        (send dc draw-polygon points)
        '())))

  (define n-circle-points 30)

  (: circle-points (Matrix Real))
  (define circle-points (build-matrix 3 n-circle-points
                                      (λ ([i : Integer] [j : Integer])
                                        (define alpha (* j (/ pi (/ n-circle-points 2))))
                                        (cond
                                          [(= i 0) (cos alpha)]
                                          [(= i 1) (sin alpha)]
                                          [else 1]))))

  (: make-circle ShapeConstructor)
  (define (make-circle . rel-adjs) ; shape constructor
    (λ (ctx-adj) ; shape
      (λ (dc) ; shape-renderer
            (define adj (apply combine-adjustment ctx-adj rel-adjs))
            (define geom (adjustment-geometric adj))
            (define points (matrix* geom circle-points))

            (apply-color-adjustments dc adj)

            (define path (new dc-path%))
            (send path move-to
                  (matrix-ref points 0 0)
                  (matrix-ref points 1 0))
            (for ([n (range 1 n-circle-points)])
                (send path line-to
                      (matrix-ref points 0 n)
                      (matrix-ref points 1 n)))
            (send path close)

            (send dc draw-path path)
            '())))

  ; creates a shape-constructor that randomly selects a shape to render
  ; every time it renders
  ; (-> (listof (cons/c real? shape/c)) shape-constructor/c)
  (: prob-shape (-> (Listof (Pairof Real Shape)) ShapeConstructor))
  (define (prob-shape weighted-shapes)
    (λ rel-adjs  ; shape-constructor
      (λ (ctx-adj) ; shape
        (λ (dc) ; shape-renderer
          (define adj (apply combine-adjustment ctx-adj rel-adjs))
          (define s (random-choice weighted-shapes))
          ((s adj) dc)))))
)

(require (for-syntax racket/base)
         'core
         racket/class
         racket/math
         math/matrix
         "adjustments.rkt")

(provide make-square
         make-circle
         define-shape
         loop-shape
         ShapeConstructor
         ShapeRenderer
         Shape)

; Helper to create shape constructors

; construct a shape which is a union of one or more shapes
(define-syntax-rule (union shape-list)
  (λ rel-adjs  ; shape-constructor
    (λ (ctx-adj) ; shape
      (λ (dc) ; shape-renderer
        (define adj (apply combine-adjustment ctx-adj rel-adjs))
        ; list of shape-renderers, from list of shapes applied to adjs
        (map (λ (s) (s adj)) shape-list)))))

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
