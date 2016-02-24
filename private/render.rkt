#lang racket/base

; Rendering tooling

(require data/queue
         racket/contract
         racket/class
         racket/draw
         "shape.rkt")

(provide (contract-out [maximum-render-cycles parameter?]
                       [render-shape (-> shape/c (is-a?/c dc<%>) any/c)]))

; Parameter that controls how many shapes to render
(define maximum-render-cycles (make-parameter 100))

; Render a shape in a device context
; render-shape: (-> shape/c (is-a?/c dc<%>))
(define (render-shape shape dc)
  (send dc set-pen "black" 0 'transparent)

  (define renderers-queue (make-queue))
  (enqueue! renderers-queue (shape))

  (let render-loop ([renderer (dequeue! renderers-queue)]
                    [n 0])
    (for ([r (renderer dc)])
      (enqueue! renderers-queue r))
    (when (and (not (queue-empty? renderers-queue))
               (<= n (maximum-render-cycles)))
      (render-loop (dequeue! renderers-queue) (+ 1 n)))))
