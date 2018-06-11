#!/bin/racket -f

(define (palindrome? s)
  (if (< (length s) 2)
    #t
    (and
      (equal? (first s) (last s))
      (palindrome? (drop-right (cdr s) 1))
    )
  )
)

(println (palindrome? (string->list (symbol->string (read)))))
