#lang racket/base

; A priority queue for storing anything

; From Wikipedia: a priority queue is an abstract data type which
; is like a regular queue or stack data structure, but where
; additionally each element has a "priority" associated with it.
; In a priority queue, an element with high priority is served
; before an element with low priority. If two elements have the
; same priority, they are served according to the order that they
; were added to the queue.

(require data/heap
         racket/list
         racket/sequence)

(provide make-queue)

(define (make-queue fn)
  ; Make a queue with a function that determines sort field,
  ; an integer
  (cons 0 ; counter
        (make-heap
         (λ (p1 p2) (< (fn (cdr p1))
                       (fn (cdr p2)))))))

(define (item-count q)
  (heap-count (cdr q)))

(define  (in-queue q)
  (sequence-map cdr
                (in-heap (cdr q))))

(define  (queue-add! q a)
  (heap-add! (cdr q) (cons (car q) a)))

; ---------------------------------------------------------------

(module+ test
  (require rackunit)

  (struct person (name age) #:transparent)
  
  (define p1 (person "Fred" 15))
  (define p2 (person "Bill" 19))
  (define p3 (person "Jane" 15))
  
  (test-case "basic tests"
             (define q (make-queue (λ (p) (person-age p))))
             (queue-add! q p2)
             (check-eq? 1 (item-count q))

             (queue-add! q p1)
             (check-eq? 2 (item-count q))
             (check-eq? p1 (sequence-ref (in-queue q) 0)))

  (test-case "test items with same priority in order added"
             (define q (make-queue (λ (p) (person-age p))))
             (for ([i 100])
                  (queue-add! q (person (format "Name.~a" i) (quotient i 10))))
             (check-eq? 100 (item-count q))

             (for ([p (in-queue q)]
                   [i (in-naturals)])
                  (check-eq? (format "Name.~a" i) (person-name p)))
             
))
