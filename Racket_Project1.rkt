#lang racket
; Question 1
(define (lowest-exponent base bound)
  (let loop ((exponent 0))
    (if (>= (expt base exponent) bound)
        exponent
        (loop (+ exponent 1)))))

; Question 2
(define (divisors n)
  (filter (lambda (x) (= (remainder n x) 0)) (range 1 (+ 1 n))))

(define (is-abundant n)
  (> (apply + (divisors n)) (* 2 n)))

(define (find-abundant limit)
  (reverse (filter is-abundant (range 1 (+ 1 limit)))))

; Question 3
(define (increasing-order? lst)
  (cond [(null? lst) true]
        [(null? (cdr lst)) true]
        [else (and (<= (car lst) (cadr lst))
                   (increasing-order? (cdr lst)))]))

; Question 4
(define (get-vowels-sc astring)
  (let ([vowels (list #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)])
    (list->string
     (filter-map (lambda (ch)
                   (if (member ch vowels)
                       (if (char-upper-case? ch)
                           (char-downcase ch)
                           (char-upcase ch))
                       #f))
                 (string->list astring)))))

; Test Cases
(lowest-exponent 3 27)
(lowest-exponent 3 28)

(find-abundant 25)

(increasing-order? (cons 3 (cons 7 (cons 9 (cons 19 empty)))))
(increasing-order? (cons -7 (cons -7 (cons 8 empty))))
(increasing-order? (cons 1 (cons 4 (cons 2 empty))))

(get-vowels-sc "A big hello world!")
(get-vowels-sc "SYSC2100")