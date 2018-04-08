#!/bin/racket -f
(define (update a b) (list b (+ a b)))

(define (fib n)
  (apply update (if (<= n 1) '(0 1) (fib (- n 1))))
)

(println (car (cdr (fib (- (read) 1)))))
