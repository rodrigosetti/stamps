stamps
=========

A language for producing art implemented on [Racket](http://racket-lang.org), inspired by
[Context Free](http://contextfreeart.org).

Install
-------

Check out this repository, or download the ZIP file and extract. Start up Racket and
open the Package Manager, using the 'Do What I Mean' tab set the Package Source to
the directory you've just checked out and click Install.


Learn
-----

Some places to start:

* some [examples](examples)
* the beginnings of a [tutorial](tutorial)
* [artwork](examples/context-free) translated from the [Context Free Gallery](https://contextfreeart.org/gallery2/)


Examples
--------

### Sierpinksi

```racket
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

(start-shape sierp)
```

![Sierpinski Image](images/sierpinski.png?raw=true)


### Tree

```racket
#lang s-exp stamps/lang

(define-shape tree
  (branch)
  (branch [flip 90]))

(define-shape branch
  [98 =>
      (circle)
      (circle [scale 0.9]
              [brightness 1])
      (branch [y 0.2]
              [scale 0.99]
              [rotate 3])]
  [2 =>
     (circle)
     (circle [scale 0.9]
             [brightness 1])
     (branch [y 0.2]
             [scale 0.99]
             [flip 90])
     (branch [y 0.2]
             [scale 0.6]
             [brightness 0.2])])

(start-shape tree)
```

![Tree Image](images/branches.png?raw=true)


### Vortex

```racket
#lang s-exp stamps/lang

(define-shape S
  (square)
  (square [s 0.5     ]
          [b 1       ])
  (S      [r  .2     ]
          [t  .7   .7]
          [s  .995   ]
          [b  .002   ]))

(background '(0 0 .5))
(start-shape S)
```

![Vortex Image](images/vortex-pattern.png?raw=true)


### Anemone

```racket
#lang s-exp stamps/lang

(define-shape start
  ((stem 8) [h 300]))

(define-shape (stem branches)
  ((loop ([i branches])
         ((branch (random-real -6 6)) [x .1]
                                      [r (* i (/ branches 360))]
                                      [s .7]))))

(define-shape (branch turn)
  [1    => (circle        [sat .4])
           ((branch turn) [x .1  ]
                          [s .99 ]
                          [r turn]
                          [b .01 ])]
  [  .1 => ((branch (random-real   -6 6)))]
  [ .04 => ((stem   (random-integer 1 4)))]
  [.001 => ])

(background '(0 0 0))
(maximum-render-cycles 100000)
(start-shape start)
```

![Anemone Image](images/anemone.png?raw=true)
