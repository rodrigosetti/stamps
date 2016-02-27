#lang s-exp stamps/lang

(define-shape sierp

  (triangle)

  (sierp [translate   0     0.288]
         [scale       0.5        ]
         [brightness  0.1        ])

  (sierp [translate  -0.25 -0.144]
         [scale       0.5        ]
         [brightness  0.1        ])

  (sierp [translate   0.25 -0.144]
         [scale       0.5        ]
         [brightness  0.1        ]))


; background is white (full bright)
(background '(0 0 1))

; make sure "sierp" is the starting shape
(start-shape sierp)
