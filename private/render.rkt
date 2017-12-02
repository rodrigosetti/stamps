#lang typed/racket/base

; Rendering tooling

(require racket/class
         racket/draw
         racket/list
         "adjustments.rkt"
         "shape.rkt"
         "common.rkt"
         "path-record.rkt"
         typed/racket/unsafe)

(unsafe-require/typed data/queue
                      [ #:opaque Queue queue?]
                      [make-queue (-> Queue)]
                      [enqueue! (-> Queue ShapeRenderer Void)]
                      [dequeue! (-> Queue ShapeRenderer)]
                      [queue-empty? (-> Queue Boolean)])

(provide maximum-render-cycles
         bounding)

(unsafe-provide render-shape)

; Parameter that controls how many shapes to render
(define maximum-render-cycles (make-parameter 10000))

; The user can set the bounding, bypassing bounding calculation based
; on shape
(define bounding (make-parameter (ann '() (Listof Real))))

; Render a shape in a device context. Returns the number of shapes
; rendered
(: render-shape (-> Shape (Instance Dc<%>) Integer))
(define (render-shape shape dc)

  (define pr (new path-record%))
  
  (when (not (empty? (bounding)))
    (send pr set-bounding
          (list-ref (bounding) 0)
          (list-ref (bounding) 1)
          (list-ref (bounding) 2)
          (list-ref (bounding) 3)))

  ; Phase 1: record paths
  ; ---------------------
  (printf "recording paths...")
  (flush-output)
  (record-paths shape pr)

  ; Phase 2: replay paths
  ; ---------------------
  (define-values (min-x min-y max-x max-y) (send pr get-bounding))
  (printf "drawing paths in bounding box ~a..."
          (map (λ ([x : Real]) (real->decimal-string x 2))
               (list min-x min-y max-x max-y)))
  (flush-output)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-smoothing 'smoothed)

  (send pr replay dc)

  (send pr get-paths-count))

; Record shape's paths in a path record
(: record-paths (-> Shape (Instance PathRecord%) Void))
(define (record-paths shape pr)
  (define renderers-queue (make-queue))
  (enqueue! renderers-queue (shape identity))

  (let render-loop ([renderer (dequeue! renderers-queue)]
                    [n 0])
    (for ([r (renderer pr)])
      (enqueue! renderers-queue r))
    (when (and (not (queue-empty? renderers-queue))
               (<= n (maximum-render-cycles)))
      (render-loop (dequeue! renderers-queue) (+ 1 n)))))
