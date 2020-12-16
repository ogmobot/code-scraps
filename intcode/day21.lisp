(load #p"intcode.lisp")

(let ((rom (file->numeric-list #p"input21.txt")))
    (let ((cpu (make-cpu rom)))
        (cpu-run-ascii cpu))
    nil)
