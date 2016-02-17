#lang racket/base

; Rendering tooling

(require data/queue
         racket/contract
         racket/class
         racket/draw
         "transformation.rkt"
         "shape.rkt")

(provide maximum-render-cycles
         render-shape)

; Parameter that controls how many shapes to render
(define maximum-render-cycles (make-parameter 100))

; Render a shape in a device context
; render-shape: (-> shape/c (is-a?/c dc<%>))
(define/contract (render-shape shape dc)

  (-> shape/c (is-a?/c dc<%>) any/c)

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
