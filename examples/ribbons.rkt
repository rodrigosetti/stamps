#lang s-exp stamps/lang

(define-shape start
  (main [h   230  ]
        [sat   0.5]
        [b     0.1]))

(define-shape main
  (branch-a)
  (main [r 21   ]
        [s  0.98]))

(define-shape branch-a
  [1 => (square)
        (branch-a [y   0.2       ]
                  [r   2         ]
                  [h   0.01   300]
                  [sat 0.001  0.9]
                  [b   0.001  1  ]
                  [s   0.9987    ])]

  [.005 => (square)
           (branch-b [y   0.2       ]
                     [r   2         ]
                     [h   0.01   300]
                     [sat 0.001  0.9]
                     [b   0.001  1  ]
                     [s   0.9987    ])])

(define-shape branch-b
  [1 => (square)
        (branch-b [y    0.2       ]
                  [r   -2         ]
                  [h    0.01   300]
                  [sat  0.001  0.9]
                  [b    0.001  1  ]
                  [s    0.9987    ])  ]

  [.01 => (square)
          (branch-a [y    0.2       ]
                    [r   -2         ]
                    [h    0.01   300]
                    [sat  0.001  0.9]
                    [b    0.001  1  ]
                    [s    0.9987    ])])

(background '(0 0 0))
(maximum-render-cycles 100000)
(start-shape start)
