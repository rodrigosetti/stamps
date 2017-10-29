#lang s-exp stamps/lang

;; A grid with nested loops

(define-shape grid
  ((loop ([j 12])
         ((loop ([i 12])
                (pentagon 
                 [x (* i 0.85)]
                 [y (* j 0.85)]
                 [saturation 1         ]
                 [hue        (* i 30)  ]
                 [brightness (* j 0.08)]))))))

(start-shape grid)