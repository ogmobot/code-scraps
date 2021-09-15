; expr should be e.g. '(/ (+ (sqrt 23) 4) 7)
(defun ipart (expr)
    (floor (eval expr)))

(defun get-parts (expr)
    ; expr is '(/ (+ (sqrt a) b) c)
    (let ((a (cadr (cadr (cadr expr))))
          (b (caddr (cadr expr)))
          (c (caddr expr)))
        (list a b c)))
    
(defun reciprocal-fpart (expr)
    (let* ((parts (get-parts expr))
           (a (car parts))
           (b (cadr parts))
           (c (caddr parts))
           (c* (* c (ipart expr))))
        `(/
            (+ (sqrt ,a) ,(- c* b))
            ,(/ (- a (* (- b c*) (- b c*))) c))))
            

; represent continued fractions as lists:
; [4; (1, 3, 1, 8)] would be
; (4 1 3 1 8 'loop)

; root should be in the form (sqrt 23)
(defun build-cf (root)
    (let ((seen (make-hash-table :test 'equal)))
        (defun build-cf* (expr)
            (let* ((parts (get-parts expr))
                    ; use (b . c) as key for hash table
                   (hash-key (cons (cadr parts) (caddr parts))))
                (cond
                    ((gethash hash-key seen) '(loop))
                    ;((=
                        ;(sqrt (car parts))
                        ;(-
                            ;(* (caddr parts) (ipart expr))
                            ;(cadr parts))) (cons (ipart expr) nil))
                    (t (progn
                        (setf (gethash hash-key seen) t)
                        (cons
                            (ipart expr)
                            (build-cf* (reciprocal-fpart expr))))))))
        (build-cf* `(/ (+ ,root 0) 1))))

(defun perfect-square (val)
    (= val (* (isqrt val) (isqrt val))))

(format t "~a~%"
    (length
        (remove-if
            (lambda (seq) (evenp (length seq)))
            (mapcar
                (lambda (val) (build-cf `(sqrt ,val)))
                (loop for i from 2 to 10000
                    unless (perfect-square i)
                    collect i)))))
