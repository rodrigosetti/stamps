#lang s-exp stamps/lang

(define-shape circles
  (circle [hue 0]
          [brightness 1]
          [saturation 1])
  (circles
   [hue 23]
   [rotate 11]
   [translate 1]
   [scale 0.995]))

(start-shape circles)