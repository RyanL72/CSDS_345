#lang racket

;member: returns #t if atom is in list, else false
(define member
  (lambda (x list)
    (if (null? list)
        #f
        (if (eq? (car list) x)
            #t
            (member x (cdr list))))))