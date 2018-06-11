#!/bin/racket -f

(define (symbol->list s)
  (string->list (symbol->string s))
)

(define (number-of c l)
  (count (lambda (x) (eq? x c)) l)
)

(define (ser-num-odd)
  (printf "Last digit of serial number: ")
  (eq? 1 (remainder (read) 2))
)

(printf "Enter wires.\n")
(let ([wirelist (symbol->list (read))])
  (printf
    (case (length wirelist)
      [(3)
        (cond
          [
            (eq? (number-of #\r wirelist) 0)
            "Cut the second wire.\n"
          ]
          [
            (eq? (last wirelist) #\w)
            "Cut the last wire.\n"
          ]
          [
            (> (number-of #\u wirelist) 1)
            "Cut the last blue wire.\n"
          ]
          [
            else
            "Cut the last wire.\n"
          ]
        )
      ]
      [(4)
        (cond
          [
            (and (> (number-of #\r wirelist) 1) (ser-num-odd))
            "Cut the last red wire.\n"
          ]
          [
            (and (eq? (last wirelist) #\y) (eq? (number-of #\r wirelist) 0))
            "Cut the first wire.\n"
          ]
          [
            (eq? (number-of #\u wirelist) 1)
            "Cut the first wire.\n"
          ]
          [
            (> (number-of #\y wirelist) 1)
            "Cut the last wire.\n"
          ]
          [
            else
            "Cut the second wire.\n"
          ]
        )
      ]
      [(5) ;TODO
        (cond
          [
            (eq? (number-of #\r wirelist) 0)
            "Cut the second wire."
          ]
          [
            (eq? (last wirelist) #\w)
            "Cut the last wire."
          ]
          [
            (> (number-of #\u wirelist) 1)
            "Cut the last blue wire."
          ]
          [
            else
            "Cut the last wire."
          ]
        )
      ]
      [(6) ;TODO
        (cond
          [
            (eq? (number-of #\r wirelist) 0)
            "Cut the second wire."
          ]
          [
            (eq? (last wirelist) #\w)
            "Cut the last wire."
          ]
          [
            (> (number-of #\u wirelist) 1)
            "Cut the last blue wire."
          ]
          [
            else
            "Cut the last wire."
          ]
        )
      ]
    )
  )
)
