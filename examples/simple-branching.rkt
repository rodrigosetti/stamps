#lang s-exp stamps/lang

(define-shape row
  [10 => 
       (circle [saturation 1  ]
               [hue        0  ]
               [brightness 0.5])
       (row [translate 0.5   0.5]
            [scale     0.9  ]
            [hue       3    ]
            [rotate    0    ])]
  [1 =>
     (row)
     (row [rotate 60 ])
     (row [rotate 120])]
)

(maximum-render-cycles 100000) 
(start-shape row)