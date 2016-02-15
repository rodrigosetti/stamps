#lang racket/base


(require racket/draw
         racket/class
         racket/contract
         math/matrix
         racket/list
         racket/math
         data/queue)

(provide (contract-out (rotate (-> real? matrix?))
                       (scale (->* (real?) (real?) matrix?))
                       (translate (-> real? real? matrix?))
                       (combine-transformation (->* () ()  #:rest (listof matrix?) matrix?))
                       (render-shape (-> shape? (is-a?/c dc<%>) any/c))
                       (make-square shape-constructor?)
                       (make-circle shape-constructor?)
                       (maximum-render-cycles parameter?))
         define-shape
         loop-shape)


; Parameter that controls how many shapes to render
(define maximum-render-cycles (make-parameter 100))

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

; Transformations constructors

(define (identity)
  (identity-matrix 3))

(define (rotate theta)
  (rotation-matrix theta))

(define (scale sx [sy sx])
  (scaling-matrix sx sy))

(define (translate tx ty)
  (translation-matrix tx ty))

; Transformations combinators

(define (combine-transformation . trans)
  (apply matrix* trans))

; Types:
; shape-renderer    : (-> (is-a?/c dc%) (listof shape-renderer?))
; shape             : (-> matrix? shape-renderer?)
; shape-constructor : (-> matrix? shape?)

(define shape-renderer?
  (-> (is-a?/c dc<%>) (listof procedure?)))

(define shape?
  (-> matrix? shape-renderer?))

(define shape-constructor?
  (->* () (matrix?) shape?))

; Shape constructors

(define (make-square [shape-trans (identity)]) ; shape constructor
  (λ (curr-trans) ; shape
    (let* ([trans (matrix* curr-trans shape-trans)]             
           [a (matrix* trans (col-matrix [-0.5 -0.5 1]))]
           [b (matrix* trans (col-matrix [-0.5  0.5 1]))]
           [c (matrix* trans (col-matrix [ 0.5  0.5 1]))]
           [d (matrix* trans (col-matrix [ 0.5 -0.5 1]))]
           [points (list (cons (matrix-ref a 0 0) (matrix-ref a 1 0))
                         (cons (matrix-ref b 0 0) (matrix-ref b 1 0))
                         (cons (matrix-ref c 0 0) (matrix-ref c 1 0))
                         (cons (matrix-ref d 0 0) (matrix-ref d 1 0)))])
      (λ (dc) ; shape-renderer
        (send dc draw-polygon points)
        '()))))

(define (make-circle [shape-trans (identity)]) ; shape constructor
  (λ (curr-trans) ; shape
    (let* ([trans (matrix* curr-trans shape-trans)]          
           [orig (matrix* trans (col-matrix [0 0 1]))]
           [start (matrix* trans (col-matrix [1 0 1]))]
           [path (new dc-path%)])
      
      (send path move-to
            (matrix-ref start 0 0)
            (matrix-ref start 1 0))
      (for ([a (range -0.1 (* 2 pi) 0.1)])
        (let ([p (matrix* trans (col-matrix ((cos a) (sin a) 1)))])
          (send path line-to
                (matrix-ref p 0 0)
                (matrix-ref p 1 0))))
      (λ (dc) ; shape-renderer
        (send dc draw-path path)
        '()))))

; Helper to create shape constructors

; shortcut for defining a shape union constructor with arguments and bind it to name
(define-syntax-rule (define-shape (name arg ...) shape ...)
  (define (name arg ...)
    (union (list shape ...))))

; define a shape which is a union of one or more shapes
(define-syntax-rule (union shape-list)
  (λ ([trans (identity)])  ; shape-constructor
    (λ (curr-trans) ; shape
      (λ (dc) ; shape-renderer
        (let* ([t (combine-transformation curr-trans trans)] ; combine its transformation with current transformation matrix into T
               [renderers (map (λ (s) (s t)) shape-list)]) ; list of shape-renderers, from list of shapes applied to T
          renderers)))))

(define-syntax-rule (define-shape-prob (name arg ...) (prob shape) ...)
  ; TODO: implement shape selector based on chance where "prob" is the
  ; probability of picking that specific shape
  (error "not implemented"))

; evaluate shape union body in a for loop and then union all together
(define-syntax-rule (loop-shape (for-clause ...) shape ...)
  (union (for/list (for-clause ...)
           ((union (list shape ...))))))

; Render a shape in a device context
; render-shape: (-> shape? (is-a?/c dc<%>))
(define (render-shape shape dc)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush "black" 'solid)
  
  (let ([renderers-queue (make-queue)])
    (enqueue! renderers-queue (shape (identity)))
    (let render-loop ([renderer (dequeue! renderers-queue)]
                      [n 0])
      (for ([r (renderer dc)])
        (enqueue! renderers-queue r))
      (when (and (not (queue-empty? renderers-queue))
                 (<= n (maximum-render-cycles)))
        (render-loop (dequeue! renderers-queue) (+ 1 n))))))

(module+ test
  ;; Tests to be run with raco test
  (require rackunit))

