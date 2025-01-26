#lang racket

;squares : takes a list of numbers and returns a list that contains the square of every number in the input list
(define squares
  (lambda (list)
    (cond
      ((null? list) '())
      (else
       (cons (* (car list) (car list)) (squares (cdr list)))))))