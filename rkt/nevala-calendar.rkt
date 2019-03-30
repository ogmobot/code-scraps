#!/usr/bin/racket -f
;; Sub this from real date to get Nev. date,
;; assuming Aug 1 == Ile 0
(define +offset+ 213)

(define (zip a b)
    (if (or (= 0 (length a)) (= 0 (length b)))
        '()
        (cons `(,(car a) ,(car b)) (zip (cdr a) (cdr b)))))

;; Access with (month-name #<object>) or (month-days #<object>)
(struct month (name days) #:transparent)
(struct calendar (months first-day) #:transparent)
(struct date (mm dd) #:transparent)

(define seasons (map (lambda (s) (month s 61))
    '("ile" "ahn" "nik" "syv" "wek" "are")))
(define nev-calendar (calendar seasons 0))

(define months (map (lambda (p) (month (car p) (cadr p))) (zip
    '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
    '(  31    29    31    30    31    30    31    31    30    31    30    31))))
(define real-calendar (calendar months 1))

(define (date->days cal dat)
    (+
        (for/sum ((m (calendar-months cal))
                   #:break (string=? (date-mm dat) (month-name m)))
                   (month-days m))
        (date-dd dat)
        1
        (- (calendar-first-day cal))))

(define (days-in-a-month cal m)
    (build-list
        (month-days m)
        (lambda (x) (date (month-name m) (+ x (calendar-first-day cal))))))

(define (days-in-a-year cal)
    (apply
        append
        (map (lambda (m) (days-in-a-month cal m)) (calendar-months cal))))

(define (days->date cal days)
    (let ((ds (days-in-a-year cal)))
        (list-ref ds (modulo (- days 1) (length ds)))))

;; (convert-date real-calendar nev-calendar 213 (date "jan" 1))
(define (convert-date cal1 cal2 offset dat)
    (days->date
        cal2
        (-
            (date->days cal1 dat)
            offset)))

(define (convert-simple dat)
    (eval
        (append
            '(convert-date)
            (if (member dat (days-in-a-year real-calendar))
                '(real-calendar nev-calendar 213)
                '(nev-calendar real-calendar 153))
            `(,dat))))

(define (make-date s)
    (let ((parts (string-split s)))
        (date (car parts) (string->number (cadr parts)))))

(define (main)
    (display "?")
    (let ((s (read-line)))
        (if (> (string-length s) 0)
            (begin
                (let ((d (convert-simple (make-date s))))
                    (printf " => ~a ~s\n" (date-mm d) (date-dd d)))
                (main))
            #f)))

(main)
