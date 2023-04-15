#lang racket

(define (my-length lst)
  (cond
    ((null? lst) 0)
    (#t          (+ 1 (my-length (cdr lst))))))


(define (my-first-half lst n)
  (if (= n 0)
      null
      (cons (car lst) (my-first-half (cdr lst) (- n 1)))))

(define (my-second-half lst n)
  (if (= n 0)
      lst
      (my-second-half (cdr lst) (- n 1))))

(define (my-split-half lst)
  (let ((half (floor (/ (my-length lst) 2))))
    (if (< half 1)
        lst
        (cons (my-first-half lst half) (cons (my-second-half lst half) null)))))


(define (my-merge lst1 lst2)
  (cond
    ((null? lst1)              lst2)
    ((null? lst2)              lst1)
    ((< (car lst1) (car lst2)) (cons (car lst1) (my-merge (cdr lst1) lst2)))
    (#t                        (cons (car lst2) (my-merge (cdr lst2) lst1)))))

(define (my-sort lst)
  (cond
    ((null? lst)       lst)
    ((null? (cdr lst)) lst)
    (#t                (my-merge (my-sort (car (my-split-half lst))) (my-sort (cadr (my-split-half lst)))))))


(define (my-append lst x)
  (if (null? lst)
      (cons x null)
      (cons (car lst) (my-append (cdr lst) x))))

; List has to be sorted
(define (my-remove-duplicates-in lst lst-res)
  (cond
    ((null? lst)              lst-res)
    ((null? (cdr lst))        (my-append lst-res (car lst)))
    ((= (car lst) (cadr lst)) (my-remove-duplicates-in (cdr lst) lst-res))
    (#t                       (my-remove-duplicates-in (cdr lst) (my-append lst-res (car lst))))))
(define (my-remove-duplicates lst)
  (my-remove-duplicates-in lst null))


(define (my-search x lst)
  (cond
    ((null? lst)     #f)
    ((= x (car lst)) #t)
    (#t              (my-search x (cdr lst)))))

(define (my-intersect-in lst1 lst2)
  (cond
    ((null? lst1)                lst1)
    ((null? lst2)                lst2)
    ((my-search (car lst1) lst2) (cons (car lst1) (my-intersect-in (cdr lst1) lst2)))
    (#t                          (my-intersect-in (cdr lst1) lst2))))
(define (intersect lst1 lst2)
  (my-intersect-in (my-remove-duplicates(my-sort lst1)) lst2)
  )

