#lang s-exp stamps/lang

(define-shape start
  ((stem 8) [hue 300]))

(define-shape (stem branches)
  ((loop-shape ([i branches])
                ((branch (random-real -.1 .1))
                                               [translate .1 0]
                                               [rotate (* i (/ branches (* 2 pi)))]
                                               [scale .7]))))

(define-shape (branch turn)
  [1 => (circle (saturation .4))
        ((branch turn)
                       [translate .1 0]
                       [scale .99]
                       [rotate turn]
                       [brightness .01])]
  [  .1 => ((branch (random-real -.1 .1)))]
  [ .04 => ((stem (random-integer 1 4)))]
  [.001 => ])

(maximum-render-cycles 100000)
(start-shape start)
