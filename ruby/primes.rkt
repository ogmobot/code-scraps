#!/usr/bin/racket -f
(define (twoup n)
  (if (integer? n)
    (if (<= n 2)
      '(2)
      (append (twoup (- n 1)) (list n))
    )
    (twoup (round n))
  )
)

(define (prime? n)
  (if (= n 2)
    #t
    (andmap
      (lambda (x) (> (remainder n x) 0))
      (twoup (- n 1))
    )
  )
)

(for-each
  println
  (filter prime? (twoup (read)))
)
