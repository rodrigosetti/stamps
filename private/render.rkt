#lang typed/racket/base

; Rendering tooling

(require racket/class
         racket/draw
         "adjustments.rkt"
         "shape.rkt"
         "common.rkt"
         typed/racket/unsafe)

(unsafe-require/typed data/queue
                      [ #:opaque Queue queue?]
                      [make-queue (-> Queue)]
                      [enqueue! (-> Queue ShapeRenderer Void)]
                      [dequeue! (-> Queue ShapeRenderer)]
                      [queue-empty? (-> Queue Boolean)])

(provide maximum-render-cycles
         render-shape)

; Parameter that controls how many shapes to render
(define maximum-render-cycles (make-parameter 100))

; Render a shape in a device context
; render-shape: (-> shape/c (is-a?/c dc<%>))
(: render-shape (-> Shape (Instance Dc<%>) Void))
(define (render-shape shape dc)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-smoothing 'smoothed)

  (define renderers-queue (make-queue))
  (enqueue! renderers-queue (shape identity))

  (let render-loop ([renderer (dequeue! renderers-queue)]
                    [n 0])
    (for ([r (renderer dc)])
      (enqueue! renderers-queue r))
    (when (and (not (queue-empty? renderers-queue))
               (<= n (maximum-render-cycles)))
      (render-loop (dequeue! renderers-queue) (+ 1 n)))))
