#lang racket/base

(require racket/contract)

(provide (contract-out (random-real (->* (real?) (real?) real?))
                       (random-choice (-> (listof (cons/c real? any/c)) any/c))))

(define random-real
  (case-lambda
    [(min max) (+ (* (- max min) (random)) min)]
    [(max) (* max (random))]))

; select a cdr value from one of the pairs with probability of the weight (car values).
(define (random-choice weighted-pairs)
  (define total (apply + (map car weighted-pairs)))
  (define theta (random-real total))
  (let loop ([x 0]
             [wps weighted-pairs])
    (define wp (car wps))
    (define xx (+ x (car wp)))
    (if (<= theta xx)
        (cdr wp) ; choice made
        (loop xx (cdr wps))))) ; continue accumulating x and moving in wps list
