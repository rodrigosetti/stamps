#lang racket/base

(module core typed/racket/base
  (require typed/racket/class
           racket/list
           racket/math
           math/matrix
           "adjustments.rkt"
           "path-record.rkt"
           typed/racket/unsafe)

  (provide ShapeConstructor
           ShapeRenderer
           Shape)

  (unsafe-provide square
                  triangle
                  circle)

  ; Types

  (define-type ShapeRenderer (-> (Instance PathRecord%) (Sequenceof ShapeRenderer)))
  (define-type Shape (-> adjustment ShapeRenderer))
  (define-type ShapeConstructor (->* () () #:rest (-> AdjustmentDelta) Shape))

  ; Shape constructors

  (: make-shape-constructor (-> (Matrix Real) ShapeConstructor))
  (define (make-shape-constructor base-points)
    (λ  rel-adjs ; shape constructor
      (λ (ctx-adj) ; shape
        (λ (dc) ; shape-renderer
          (define adj (apply combine-adjustment ctx-adj rel-adjs))
          (define geom (adjustment-geometric adj))
          (define points (matrix* geom base-points))
          (send dc record-path (path points
                                    (adjustment-hue adj)
                                    (adjustment-saturation adj)
                                    (adjustment-brightness adj)
                                    (adjustment-alpha adj)))
          '()))))

  (: square ShapeConstructor)
  (define square (make-shape-constructor (matrix [[-1/2 -1/2 1/2  1/2]
                                                  [-1/2  1/2 1/2 -1/2]
                                                  [   1    1   1    1]])))

  (: triangle ShapeConstructor)
  (define triangle
    (make-shape-constructor (matrix [[-1/2  1/2    0]
                                     [(/ 1 (* 2 (sqrt 3))) (/ 1 (* 2 (sqrt 3))) (/ -1 (sqrt 3))]
                                     [   1    1    1]])))

  (define n-circle-points 30)

  (: circle-points (Matrix Real))
  (define circle-points (build-matrix 3 n-circle-points
                                      (λ ([i : Integer] [j : Integer])
                                        (define alpha (* j (/ pi (/ n-circle-points 2))))
                                        (cond
                                          [(= i 0) (/ (cos alpha) 2)]
                                          [(= i 1) (/ (sin alpha) 2)]
                                          [else 1]))))

  (: circle ShapeConstructor)
  (define circle (make-shape-constructor circle-points)))

(require (for-syntax racket/base)
         'core
         racket/class
         racket/math
         math/matrix
         "random-utils.rkt"
         "adjustments.rkt")

(provide square
         circle
         triangle
         define-shape
         loop-shape
         ShapeConstructor
         ShapeRenderer
         Shape)


; creates a shape-constructor that randomly selects a shape to render
; every time it renders
(define (prob-shape weighted-shapes)
  (λ rel-adjs  ; shape-constructor
    (λ (ctx-adj) ; shape
      (λ (dc) ; shape-renderer
        (define adj (apply combine-adjustment ctx-adj rel-adjs))
        (define s (random-choice weighted-shapes))
        ((s adj) dc)))))

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
