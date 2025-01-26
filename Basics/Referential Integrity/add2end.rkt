#lang racket

; add an element to the end of a  list
(define add2end
  (lambda (x lis)
    (if (null? lis)
        (cons x '())
        (cons (car lis) (add2end x (cdr lis))))))

; we want referential transparency here ( add2end always behaves the same)
(define fun
  (lambda (lis)
    (append ( add2end 'x lis) (add2end 'y lis))))