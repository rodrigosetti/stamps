#lang s-exp stamps/lang

(define-shape shape
  (circle 
   [saturation 1  ]
   [hue        0  ]
   [brightness 1  ]
   [alpha      0.5])  
  (circle
   [translate -0.3 -0.5]
   [saturation 1  ]
   [hue        120]
   [brightness 1  ]
   [alpha      0.5])
  (circle
   [translate 0.3 -0.5]
   [saturation 1  ]
   [hue        240]
   [brightness 1  ]
   [alpha      0.5]))

(start-shape shape)