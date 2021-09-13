import shoddylisp

shoddylisp.global_env.update({
    shoddylisp.Symbol("<<1"):   (lambda a: a << 1),
    shoddylisp.Symbol(">>1"):   (lambda a: a >> 1),
    shoddylisp.Symbol("&1"):  (lambda a: a & 1),
    shoddylisp.Symbol("|"):    (lambda a, b: a | b),
})

shoddylisp.eval_string("""
(defun ->bin-list (n)
    ;; convert an integer into binary digits (least sig. fig. first)
    (if (= n 0)
        nil
        (cons (&1 n) (->bin-list (>>1 n)))))

(defun <-bin-list (seq)
    ;; convert a list of binary digits into an integer
    (if seq
        (| (car seq) (<<1 (<-bin-list (cdr seq))))
        0))

(defun +bin3 (a b c)
    ;; note - result is (sum carry)
    (cond
        ((and (= a 0) (= b 0) (= c 0)) (quote (0 0)))
        ((and (= a 0) (= b 0) (= c 1)) (quote (1 0)))
        ((and (= a 0) (= b 1) (= c 0)) (quote (1 0)))
        ((and (= a 0) (= b 1) (= c 1)) (quote (0 1)))
        ((and (= a 1) (= b 0) (= c 0)) (quote (1 0)))
        ((and (= a 1) (= b 0) (= c 1)) (quote (0 1)))
        ((and (= a 1) (= b 1) (= c 0)) (quote (0 1)))
        ((and (= a 1) (= b 1) (= c 1)) (quote (1 1)))))

(defun sum-lists-carry (list-a list-b carry)
    (let ((sum-carry (+bin3 (if list-a (car list-a) 0)
                            (if list-b (car list-b) 0)
                            carry)))
        (cons
            (car sum-carry)
            (if (and (null? list-a) (null? list-b))
                (cdr sum-carry)
                (sum-lists-carry
                    (if list-a (cdr list-a) nil)
                    (if list-b (cdr list-b) nil)
                    (cadr sum-carry))))))

(defun +. (int-a int-b)
   (<-bin-list
        (sum-lists-carry
            (->bin-list int-a)
            (->bin-list int-b)
            0)))

(defun *. (int-a int-b)
    (defun ethiopian-multiply (list-a list-b total)
        ;; list-a shrinks, list-b grows
        (if (null? list-a)
            total
            (ethiopian-multiply
                (cdr list-a)
                (cons 0 list-b)
                (if (= 1 (car list-a))
                    (sum-lists-carry total list-b 0)
                    total))))
    (<-bin-list
        (ethiopian-multiply (->bin-list int-a) (->bin-list int-b) nil)))
""")

if __name__=="__main__":
    shoddylisp.repl()
