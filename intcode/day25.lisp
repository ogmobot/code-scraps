(load #p"intcode.lisp")

(let ((rom (file->numeric-list #p"input25.txt")))
    (let ((cpu (make-cpu rom)))
        (cpu-run-ascii cpu))
    nil)
