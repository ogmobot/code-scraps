(defun load-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read stream nil)
            while line
            collect line)))

(defun sum-of-first-n (numbers n)
    (if (= n 0)
        0
        (+ (car numbers) (sum-of-first-n (cdr numbers) (- n 1)))))

(defparameter *primes-squared* (mapcar (lambda (n) (* n n)) (load-file "primes.txt")))

(loop for n = 1 then (+ n 1)
    while (< n (length *primes-squared*))
    do (if (= 0 (mod (sum-of-first-n *primes-squared* n) n))
        (format t "~8a~8a~%" n (sum-of-first-n *primes-squared* n))
        nil))
