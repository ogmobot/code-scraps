import shoddylisp

try:
    shoddylisp.global_env.update({
        shoddylisp.Symbol("ext-rpn"):
            shoddylisp.Lisped_list(input("RPN: ").split())
    })
    shoddylisp.eval_string("""((lambda ()
    ; Terms are tuples of (coefficient symbol power)
    (set! ->term (lambda (s)
        ; there will be at most one pronumeral.
        (if
            (and 
                (or (> (lower s) (quote "a")) (= (lower s) (quote "a")))
                (or (< (lower s) (quote "z")) (= (lower s) (quote "z"))))
            ; pronumeral
            (tuple (->int 1) s (->int 1))
            ; number
            (tuple (->int s) (quote "") (->int 0)))))
    ; Operations always operate on lists of terms.
    (set! operations (dict-new))
    (dict-set operations (quote "+") (lambda (a b)
        ; 'a and 'b are both lists of terms.
        (set! group-like-terms (lambda (term term-list)
            (if
                (contains
                    (map (lambda (t) (index t 2)) term-list)
                    (index term 2))
                (map
                    (lambda (t)
                        (tuple
                            (if (= (index t 2) (index term 2))
                                (+ (index t 0) (index term 0))
                                (index t 0))
                            (index t 1)
                            (index t 2)))
                    term-list)
                (cons term term-list))))
        (filter
            (lambda (t) (not (= 0 (index t 0))))
            (foldl group-like-terms (append a b) (quote ())))))
    (dict-set operations (quote "-") (lambda (a b)
        ((dict-get operations (quote "+"))
            a
            (map (lambda (t)
                (tuple (- 0 (index t 0)) (index t 1) (index t 2)))
                b))))
    (dict-set operations (quote "*") (lambda (a b)
        (set! multiply-terms (lambda (t1 t2)
            (tuple
                (* (index t1 0) (index t2 0))
                (if (= (index t1 1) (quote ""))
                    (index t2 1)
                    (index t1 1))
                (+ (index t1 2) (index t2 2)))))
        ((dict-get operations (quote "+"))
            (cons (tuple (->int 0) (quote "") (->int 0)) (quote ()))
            ; ^ add 0 to result to group like terms ^
            (foldl
                append
                (map
                    (lambda (left-term)
                        (map
                            (lambda (right-term)
                                (multiply-terms left-term right-term))
                            b))
                    a)
                (quote ())))))
    (dict-set operations (quote "^") (lambda (a b)
        ; It's guaranteed that 'b is a singleton list with (b0, "", 0)
        (foldl
            (dict-get operations (quote "*"))
            (map (lambda (unused) a) (range 0 (index (car b) 0)))
            (cons (tuple (->int 1) (quote "") (->int 0)) (quote ())))))
    (set! eval-rpn (lambda (rpn stack)
        (if (= 0 (length rpn))
            stack
            (eval-rpn
                (cdr rpn)
                (if (contains operations (car rpn))
                    (cons
                        ((dict-get operations (car rpn))
                            (car (cdr stack)) (car stack))
                        (cdr (cdr stack)))
                    (cons (cons (->term (car rpn)) (quote ())) stack))))))
    (set! term->string (lambda (term)
        (+
            (if (and (= (index term 0) (- 0 1)) (> (index term 2) 0))
                (quote "-")
                (quote ""))
            (+
                (if
                    (and
                        (> (index term 2) 0)
                        (or
                            (= (index term 0) 1)
                            (= (index term 0) (- 0 1))))
                    (quote "")
                    (->str (index term 0)))
                (+
                    (index term 1)
                    (if (> (index term 2) 1)
                        (+ (quote "^") (->str (index term 2)))
                        (quote "")))))))
    (set! expr->string (lambda (term-list)
        (if (= (length term-list) 0)
            (quote "0")
            (str-replace
                (delimit
                    (map term->string
                        (qsort
                            (lambda (t1 t2) (> (index t1 2) (index t2 2)))
                            term-list))
                    (quote " + "))
                (quote "+ -")
                (quote "- ")))))
    (print (expr->string (car (eval-rpn ext-rpn (quote ())))))
    ))""")
except Exception as e:
    print(f"An error occurred: {e}")
    shoddylisp.repl()