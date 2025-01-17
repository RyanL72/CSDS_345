#lang racket

(define numoccurring
  (lambda ( x list)
  
    (cond
      ((null? list)
       0)
      ((eq? (car list) x)
       (+ 1 (numoccurring x (cdr list))))
      (else
       (+ 0 (numoccurring x (cdr list)))))))