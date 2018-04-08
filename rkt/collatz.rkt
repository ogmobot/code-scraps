#!/bin/racket -f
(define (collatz n)
  (if (= n 1)
    '(1)
    (cons
      n
      (collatz
        (if (even? n)
          (/ n 2)
          (+ (* n 3) 1)
        )
      )
    )
  )
)

(println (collatz (read)))
