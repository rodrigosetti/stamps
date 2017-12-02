#lang s-exp stamps/lang 

(define-shape scene
  (spiral [b 0]))

(define-shape spiral
  ((angle (ri 10 22))
   [r (ri 25 65)]
   [t 0 (ri 0 40)])
  (spiral [r 11]
          [t 15]
          [s .99]
          [b .01]))

(define-shape (angle w)
  (square [s w 2] [b 1])
  (side [t (/ w 2) 0]
        [r 90])
  (side [t (- 0 (/ w 2)) 0]
        [r 90])
  (square [s w 1]))

(define-shape side
  ((loop ([i 75])
         (square [s (expt .99 i)
                    (* 2 (expt .99 i))]
                 [t (/ i 2) 0]
                 [b 1])))
  ((loop ([i 75])
         (square [s (expt .99 i)]
                 [t (/ i 2) 0]))))
 
(background '(0 0 1))
(maximum-render-cycles 100000)
(bounding '(-20 -150 150 5))
(start-shape scene)