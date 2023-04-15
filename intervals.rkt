#lang racket

(define (to-all-pairs app lst)
  (map (lambda (pair) (cond ([and (<=(car app)(cadr pair)) (<=(car pair)(cadr app))]  1)
                            (#t 0))) lst))

(define (sum-lst lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-lst (cdr lst)))))

(define (sum-lst-minus-one lst)
  (if (null? (sum-lst lst))
      0
      (- (sum-lst lst) 1)))

(define (intervals lst)
  (map (lambda (pair) (sum-lst-minus-one (to-all-pairs pair lst))) lst))
