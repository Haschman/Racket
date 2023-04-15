#lang racket
(define (my-del-first lst)
  (if (null? lst)
      lst
      (cdr lst)))

(define (my-del-last lst)
  (cond
    ((null? lst)         lst)
    ((null? (cdr lst))   null)
    (#t                 (cons (car lst) (my-del-last (cdr lst))))))

(define (my-append lst x)
  (if (null? lst)
      (cons x null)
      (cons (car lst) (my-append (cdr lst) x))))

(define (my-reverse lst)
  (if (null? lst)
      lst
      (my-append (my-reverse (cdr lst)) (car lst)))
  )

; Not need to check length del-last and del-first will handle that
(define (is-palindrome? lst)
  (let ((lst2 (my-del-first (my-del-last lst))))
    (equal? lst2 (my-reverse lst2))
  ))
