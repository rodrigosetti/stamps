#lang s-exp stamps/lang

(define-shape row
  ((loop ([i 8])
         (circle [saturation 1  ]
                 [hue        0  ]
                 [brightness 0.5]
                 [translate (* i (random-real 0.98 1))
                            (* i (random-real 0.98 1))])))
  (row [rotate    60 ]
       [translate 5 5]
       [scale     0.9])
  (row [rotate    120]
       [translate 5 5]
       [scale     0.9])
)

(define-shape init
  (row [alpha -0.7]))

(maximum-render-cycles 50000)
(start-shape init)