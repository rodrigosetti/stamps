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


(background '(0 0 1))
(start-shape tree)
