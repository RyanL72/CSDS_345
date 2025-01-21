#lang racket

;remove first instance of an element in a list
(define myremove
  (lambda (x list)
    (cond
      ((null? list) '())
      ((eq? x (car list)) (cdr list))
      (else (cons (car list) (myremove x (cdr list)))))))

