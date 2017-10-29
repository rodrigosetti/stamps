#lang s-exp stamps/lang

(define-shape row
  (circle [saturation 1  ]
        [hue        0  ]
        [brightness 0.5])
  (row [translate   1   1]
       [scale    0.8  ]
       [hue      2    ])
)

(start-shape row)