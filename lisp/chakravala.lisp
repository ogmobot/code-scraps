(defun solve-pell (N)
    (defun chakravala (a b k)
        ;; returns minimal (x . y) where x^2 - N y^2 = 1
        (if (= k 1)
            (cons a b)
            (loop
                for m from 1
                when (= 0 (mod (+ a (* b m)) k))
                return (chakravala
                    (/ (+ (* a m) (* N b)) (abs k))
                    (/ (+ a (* b m)) (abs k))
                    (/ (- (* m m) N) k)))))
    (chakravala 1 1 (- 1 N)))

