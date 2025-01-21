#lang racket

;myappend : takes two lissts and returnns te result of appending the second list to the end of the first list.
(define myappend
  (lambda (x y)
    (cond
      ((null? x) '())
      ((null? (cdr x)) (cons (car x) y))
       (else (cons (car x) (myappend(cdr x) y))))))