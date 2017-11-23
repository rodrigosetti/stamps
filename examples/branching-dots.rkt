#lang s-exp stamps/lang

; Performance testing - no probability, just loops

(define-shape row
  ((loop ([i 10])
         (circle [saturation 1  ]
                 [hue        0  ]
                 [brightness 0.5]
                 [translate (* i (random-real 0.98 1))
                            (* i (random-real 0.98 1))])))
  (row [rotate    60 ]
       [translate 5 5]
       [scale     0.8])
  (row [rotate    120]
       [translate 5 5]
       [scale     0.8])
)

(maximum-render-cycles 100000)
(start-shape row)