(require :uiop)
(ql:quickload 'cl-heap)

(defun load-lines (filename)
    (with-open-file (handle filename)
        (loop for line = (read-line handle nil)
            while line
            collect line)))

(defun get-grid (grid coord)
    ;; coord is (row . column)
    (if (and (>= (car coord) 0) (>= (cdr coord) 0))
        (nth (cdr coord) (nth (car coord) grid))))

(defun path-cost (path grid)
    (apply #'+ (mapcar (lambda (c) (get-grid grid c)) path)))

(let (
    (matrix
        (mapcar
            (lambda (text)
                (mapcar
                    #'parse-integer
                    (uiop:split-string text :separator ",")))
            (load-lines "p083_matrix.txt")))
    (directions
        ;; p081
        ;(list (cons 0 1) (cons 1 0)))
        ;; p082
        ;(list (cons 0 1) (cons 1 0) (cons -1 0)))
        (list (cons 0 1) (cons 1 0) (cons -1 0) (cons 0 -1)))
    (seen (make-hash-table :test #'equal))
    (paths (make-instance 'cl-heap:priority-queue)) ; each path is a list of cons pairs
    (reached-target
        (lambda (coord)
            (equal coord (cons 79 79)))))
            ;; p082
            ;(equal (cdr coord) 79))))

    (cl-heap:enqueue paths (list (cons 0 0)) (get-grid matrix (cons 0 0)))
    ;; p082
    ;(mapcar
        ;(lambda (row)
            ;(cl-heap:enqueue paths (list (cons row 0)) (get-grid matrix (cons row 0))))
        ;(loop for i from 0 to 79 collect i))
    
    (defun push-paths ()
        (let ((path (cl-heap:dequeue paths)))
            (cond
                ((gethash (car path) seen) nil)
                ((funcall reached-target (car path))
                    (mapcar (lambda (c) (get-grid matrix c)) path))
                (t
                    (progn
                    ;(format t "~a => ~a~%"
                        ;(mapcar (lambda (c) (get-grid matrix c)) path)
                        ;(path-cost path matrix))
                    (setf (gethash (car path) seen) t)
                    (mapcar
                        (lambda (direction)
                            (let ((new-coord 
                                    (cons
                                        (+ (car direction) (caar path))
                                        (+ (cdr direction) (cdar path)))))
                                (if (get-grid matrix new-coord)
                                    (cl-heap:enqueue
                                        paths
                                        (cons new-coord path)
                                        (path-cost (cons new-coord path) matrix)))))
                        directions)
                    nil)))))
    (defun find-best-path ()
        (let ((result (push-paths)))
            (cond
                (result result)
                ((null paths) nil)
                (t (find-best-path)))))
    (let ((best-path (find-best-path)))
        (format t "~a => ~a~%" best-path (apply #'+ best-path))))
