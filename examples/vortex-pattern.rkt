#lang s-exp stamps/lang

; This program draws an spiral of squares

; define a shape called "S", which is composed of 3 sub-shapes:
(define-shape S

  ; 1) a square
  (square)

  ; 2) another square, with half the size and fully bright (white)
  (square [scale      0.5     ]
          [brightness 1       ])

  ; 3) the shape S (recursively), slightly  rotated, translated
  ; and very slightly smaller and brighter
  (S      [rotate      .2     ]
          [translate   .7   .7]
          [scale       .995   ]
          [brightness  .002   ]))

; background is gray (half bright)
(background '(0 0 .5))

; make sure "S" is the starting shape
(start-shape S)
