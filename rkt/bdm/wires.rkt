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
      [(5)
        (cond
          [
            (and (eq? (last wirelist) #\b) (ser-num-odd))
            "Cut the fourth wire.\n"
          ]
          [
            (and (eq? (number-of #\r wirelist) 1) (> (number-of #\y wirelist) 1))
            "Cut the first wire.\n"
          ]
          [
            (eq? (number-of #\b wirelist) 0)
            "Cut the second wire.\n"
          ]
          [
            else
            "Cut the first wire.\n"
          ]
        )
      ]
      [(6)
        (cond
          [
            (and (eq? (number-of #\y wirelist) 0) (ser-num-odd))
            "Cut the third wire.\n"
          ]
          [
            (and (eq? (number-of #\y wirelist) 1) (> (number-of #\w wirelist) 1))
            "Cut the fourth wire.\n"
          ]
          [
            (eq? (number-of #\r wirelist) 0)
            "Cut the last wire.\n"
          ]
          [
            else
            "Cut the fourth wire.\n"
          ]
        )
      ]
    )
  )
)
