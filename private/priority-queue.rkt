#lang racket/base

; A priority queue for storing anything.

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

(provide queue?
         make-queue
         item-count
         in-queue
         queue-add!)

(define (queue? q)
  (and (mpair? q)
       (integer? (mcar q))
       (heap? (mcdr q))))

(define (make-queue fn)
  ; Make a queue with a function that determines sort field,
  ; which should be a number.
  (mcons 0 ; counter
        (make-heap
         (λ (p1 p2)
           (let ([f1 (fn (cdr p1))]
                 [f2 (fn (cdr p2))])
             (if (eq? f1 f2)
                 (< (car p1) (car p2))
                 (< f1 f2)))))))

(define (item-count q)
  (heap-count (mcdr q)))

(define  (in-queue q)
  (sequence-map cdr
                (in-heap (mcdr q))))

(define  (queue-add! q a)
  (heap-add! (mcdr q) (cons (mcar q) a))
  (set-mcar! q (add1 (mcar q))))

; ---------------------------------------------------------------

(module+ test
  (require rackunit)

  (struct person (name age) #:transparent)
  
  (define p1 (person "Person1" 15))
  (define p2 (person "Person2" 19))
  (define p3 (person "Person3" 15))
  
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
                  (check-equal? (format "Name.~a" i) (person-name p)))
             
))
