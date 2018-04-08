#!/bin/racket -f
(define (fib n)
  (cond
    ((<= n 2) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

; (write-line (map fib '(1 2 3 4 5)))
(println (fib (read)))
