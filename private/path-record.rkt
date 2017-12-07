#lang typed/racket/base

(require typed/racket/class
         typed/racket/draw
         math/matrix
         racket/list
         data/queue
         "color-utils.rkt"
         "linalg-utils.rkt"
         "common.rkt"
         typed/racket/unsafe)

(unsafe-require/typed data/queue
                      [ #:opaque Queue queue?]
                      [make-queue (-> Queue)]
                      [enqueue! (-> Queue path Void)]
                      [dequeue! (-> Queue path)]
                      [queue-length (-> Queue Integer)]
                      [in-queue (-> Queue (Sequenceof path))]
                      [queue-empty? (-> Queue Boolean)])

(provide path
         PathRecord%
         path-record%)

(struct path ([points     : (Matrix Real)]
              [hue        : Real]
              [saturation : Real]
              [brightness : Real]
              [alpha      : Real])
  #:transparent)

(: path-bounding (-> path (values Real Real Real Real)))
(define (path-bounding P)

  (define mat (path-points P))
  (define N (matrix-num-cols mat))

  (: xs (Listof Real))
  (define xs (for/list ([i (range N)])
                       (matrix-ref mat 0 i)))

  (: ys (Listof Real))
  (define ys (for/list ([i (range N)])
                       (matrix-ref mat 1 i)))

  (values (apply min xs)
          (apply min ys)
          (apply max xs)
          (apply max ys)))

; Helper to apply color adjustments
(: set-brush-with-solid-color (-> (Instance Dc<%>) Real Real Real Real Void))
(define (set-brush-with-solid-color dc hue saturation brightness alpha)
  (define-values (r g b) (hsb->rgb hue saturation brightness))
  (define color (make-object color% r g b alpha))
  (send dc set-brush color 'solid))


(define-type PathRecord% (Class [get-bounding (-> (Values Real Real Real Real))]
                                [get-paths-count (-> Integer)]
                                [set-bounding (-> Real Real Real Real Void)]
                                [replay (-> (Instance Dc<%>) Void)]
                                [record-path (-> path Void)]))

(define path-record%
  (class object%

    (super-new)

    (: min-x Real)
    (: min-y Real)
    (: max-x Real)
    (: max-y Real)
    (define min-x 0)
    (define min-y 0)
    (define max-x 0)
    (define max-y 0)

    (: calc-bounding? Boolean)
    (define calc-bounding? #t)

    (define paths-queue (make-queue))

    (define/public (get-bounding) (values min-x min-y max-x max-y))

    (define/public (get-paths-count) (queue-length paths-queue))

    (: set-bounding (-> Real Real Real Real Void))
    (define/public (set-bounding x1 y1 x2 y2)
      (set! min-x x1)
      (set! min-y y1)
      (set! max-x x2)
      (set! max-y y2)
      (set! calc-bounding? #f))

    (: replay (-> (Instance Dc<%>) Void))
    (define/public (replay dc)
      (: current-hue        Real)
      (: current-saturation Real)
      (: current-brightness Real)
      (: current-alpha      Real)
      (define current-hue        -1)
      (define current-saturation -1)
      (define current-brightness -1)
      (define current-alpha      -1)

      (define-values (width height) (send dc get-size))

      ; construct a transformation that translates and scales
      ; the bounding into the (0 0 sx sy) area
      (define b-width (- max-x min-x))
      (define b-height (- max-y min-y))
      (define x-factor (/ width  b-width))
      (define y-factor (/ height b-height))
      (define factor (min x-factor y-factor))
      (define trans (matrix* (translation-matrix (/ width -2)
                                                 (/ height -2))
                             (scaling-matrix factor factor)
                             (translation-matrix (/ b-width 2)
                                                 (/ b-height 2))
                             (translation-matrix min-x
                                                 min-y)))

      (for ([P (in-queue paths-queue)])
        (define hue (path-hue P))
        (define saturation (path-saturation P))
        (define brightness (path-brightness P))
        (define alpha (path-alpha P))

        ; whether the color was modified since last path
        (when (or (not (equal? current-hue hue))
                  (not (equal? current-saturation saturation))
                  (not (equal? current-brightness brightness))
                  (not (equal? current-alpha alpha)))
          (set-brush-with-solid-color dc hue saturation brightness alpha))

        ; transform the matrix according to "trans" and
        ; build a points (pairs of reals)
        (define mat (matrix* trans (path-points P)))
        (define N (matrix-num-cols mat))

        (: points (Listof (Pairof Real Real)))
        (define points (for/list ([i (range N)])
                         (cons (matrix-ref mat 0 i)
                               (matrix-ref mat 1 i))))

        (send dc draw-polygon points)

        (set! current-hue hue)
        (set! current-saturation saturation)
        (set! current-brightness brightness)
        (set! current-alpha alpha)))

    (: record-path (-> path Void))
    (define/public (record-path P)

      (when calc-bounding?
        (define-values (small-x small-y big-x big-y)
          (path-bounding P))

        (when (> big-x max-x) (set! max-x big-x))
        (when (> big-y max-y) (set! max-y big-y))
        (when (< small-x min-x) (set! min-x small-x))
        (when (< small-y min-y) (set! min-y small-y)))
      
      (enqueue! paths-queue P))

    ))


(module+ test
  (require typed/rackunit)

  (test-case "path-record tests"

             (define pr (new path-record%))
             (define a-path (path (matrix [[ 0 -1  2]
                                           [-2  7  1]
                                           [ 1  1  1]])
                                  0 0 0 0))

             (check-eq? 0 (send pr get-paths-count))

             (send pr record-path a-path)
             (check-eq? 1 (send pr get-paths-count))
             
             (define-values (min-x min-y max-x max-y) (send pr get-bounding))
             (check-eq? -1 min-x)
             (check-eq? -2 min-y)
             (check-eq?  2 max-x)
             (check-eq?  7 max-y)
             
             (send pr record-path (path (matrix [[ 1  2  3]
                                                 [ 0 -4  1]
                                                 [ 1  1  1]])
                                        0 0 0 0))
             (check-eq? 2 (send pr get-paths-count))
             
             (set!-values (min-x min-y max-x max-y) (send pr get-bounding))
             (check-eq? -1 min-x)
             (check-eq? -4 min-y)
             (check-eq?  3 max-x)
             (check-eq?  7 max-y))

  (test-case "path function tests"

             (define a-path (path (matrix [[ 0 -1  2 -10]
                                           [-7  7  1  10]
                                           [ 1  1  1  1 ]])
                                  0 0 0 0))
             (define-values (min-x min-y max-x max-y) (path-bounding a-path))
             (check-eq? -10 min-x)
             (check-eq? -7 min-y)
             (check-eq?  2 max-x)
             (check-eq?  10 max-y))

)
