#lang s-exp stamps/lang

;; A grid with nested loops

(define hue+ 8)
(define scale* 0.4)
(define bright+ 0.2)

(define-shape shape
  (pentagon 
   [saturation 1   ]
   [hue        30  ]
   [brightness 0.5 ])

  ;; Now draw pentagons on each side
  (shape
   [translate 0.5 0    ]
   [hue       hue+     ]
   [scale     scale*   ]
   [brightness bright+ ])
  (shape
   [translate 0.15 -0.48]
   [hue       hue+     ]
   [scale     scale*   ]
   [rotate    72       ]
   [brightness bright+ ])
  (shape
   [translate 0.15 0.48]
   [hue       hue+     ]
   [scale     scale*   ]
   [rotate    72       ]
   [brightness bright+ ])
  (shape
   [translate -0.40 0.3]
   [hue       hue+     ]
   [scale     scale*   ]
   [rotate    -72      ]
   [brightness bright+ ])
  (shape
   [translate -0.40 -0.3]
   [hue       hue+     ]
   [scale     scale*   ]
   [rotate    -72      ]
   [brightness bright+ ])
  
)

(start-shape shape)