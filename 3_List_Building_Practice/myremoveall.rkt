#lang racket

;remove all instances of element in a list
(define myremoveall
  (lambda (x list)
    (cond
      ((null? list) '())
      ((eq? x (car list))  (myremoveall x (cdr list)))
      (else (cons (car list) (myremoveall x (cdr list)))))))