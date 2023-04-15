#lang racket

(define (nth-col n matrix)
  (if (= n 0)
      (map (lambda (row) (car row)) matrix)
      (nth-col (- n 1) (map (lambda (row) (cdr row)) matrix))))

(define (mult row col)
  (if (null? (cdr row))
      (* (car row) (car col))
      (+ (* (car row)
            (car col)) (mult (cdr row) (cdr col)))))

(define (lst-len lst)
  (if(null? lst)
     0
     (+ 1 (lst-len (cdr lst)))))

(define (cnt-row-in row mat mat-width init)
  (if (= init mat-width)
      (cons (mult row (nth-col init mat)) null)
      (cons (mult row (nth-col init mat)) (cnt-row-in row mat mat-width (+ 1 init)))))
(define (cnt-row row mat)
  (let ([mat-width (- (lst-len (car mat)) 1)])
    (cnt-row-in row mat mat-width 0)))

(define (matrix-mul mat1 mat2)
  (map (lambda (row) (cnt-row row mat2)) mat1))
