#lang racket

;member but with conditional
(define condmember
  (lambda (x list)
    (cond
      ((null? list) #f)
      ((eq? x (car list)) #t)
       (else (condmember x (cdr list))))))