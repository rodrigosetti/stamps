#lang s-exp stamps/lang

(define-shape start
  ((stem 8) [h 300]))

(define-shape (stem branches)
  ((loop-shape ([i branches])
                ((branch (random-real -.1 .1)) [x .1]
                                               [r (* i (/ branches (* 2 pi)))]
                                               [s .7]))))

(define-shape (branch turn)
  [1    => (circle        [sat .4])
           ((branch turn) [x .1  ]
                          [s .99 ]
                          [r turn]
                          [b .01 ])]
  [  .1 => ((branch (random-real   -0.1 0.1)))]
  [ .04 => ((stem   (random-integer 1   4  )))]
  [.001 => ])

(maximum-render-cycles 100000)
(start-shape start)
