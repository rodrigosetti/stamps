#lang s-exp stamps/lang

(define-shape start
  (main [h   230]
        [sat .5 ]
        [b   .1 ]))

(define-shape main
  (branch-a)
  (main [r (deg->rad 21)]
        [s 0.98         ]))

(define-shape branch-a
  [1 => (square)
        (branch-a [y   .2]
                  [r   (deg->rad 2)]
                  [h   0.01  300]
                  [sat 0.001 0.9]
                  [b   0.001   1]
                  [s   .9987    ])]

  [.005 => (square)
           (branch-b [y   .2]
                     [r   (deg->rad 2)]
                     [h   0.01  300]
                     [sat 0.001 0.9]
                     [b   0.001   1]
                     [s   .9987    ])])

(define-shape branch-b
  [1 => (square)
        (branch-b [y   .2]
                  [r   (deg->rad -2)]
                  [h   0.01  300]
                  [sat 0.001 0.9]
                  [b   0.001   1]
                  [s   .9987])  ]

  [.01 => (square)
          (branch-a [y   .2]
                    [r   (deg->rad -2)]
                    [h   0.01   300]
                    [sat 0.001  0.9]
                    [b   0.001  1  ]
                    [s   .9987     ])])

(maximum-render-cycles 1000000)
(start-shape start)
