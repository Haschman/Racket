#lang racket

(define (my-negation pred)
  (lambda (x) (not (pred x))))

(define (partition-by f s)
  (cons (filter f s) (cons (filter (my-negation f) s) null)))