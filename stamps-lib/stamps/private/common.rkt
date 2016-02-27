#lang typed/racket/base

(provide Dc<%>
         float-modulo)

(: float-modulo (-> Real Real Real))
(define (float-modulo p q)
  (- p (* q (truncate (/ p q)))))

(define-type Dc<%> (Class [set-brush (-> Any Symbol Void)]
                          [draw-polygon (-> (Listof (Pairof Number Number)) Void)]
                          [draw-path (-> (Object) Void)]
                          [set-pen (-> String Number Symbol Void)]
                          [get-size (-> (Values Real Real))]
                          [set-smoothing (-> Symbol Void)]))
