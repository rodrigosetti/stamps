#lang s-exp stamps/lang

; This program draws an spiral of squares

; define a shape called "S", which is composed of 3 sub-shapes:
(define-shape S

  ; 1) a square
  (square)

  ; 2) another square, with half the size and fully bright (white)
  (square [s 0.5     ]
          [b 1       ])

  ; 3) the shape S (recursively), slightly  rotated, translated
  ; and very slightly smaller and brighter
  (S      [r  1.5    ]
          [t  .7   .7]
          [s  .995   ]
          [b  .002   ]))

; background is gray (half bright)
(background '(0 0 .5))

; make sure "S" is the starting shape
(start-shape S)
