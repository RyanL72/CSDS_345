#lang racket

;sumnumbers takes a list of atoms and numbers andreturns the sum of all the numbers in the list
(define sumnumbers
  (lambda (list)
    (cond
      ((null? list) 0)
      ((integer? (car list)) (+ (car list) (sumnumbers(cdr list))))
      (else (+ 0 (sumnumbers(cdr list)))))))