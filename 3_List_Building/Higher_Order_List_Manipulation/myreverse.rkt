#lang racket

; helper function to add an element to the end of a list abiding by referential integrity
(define add2end
  (lambda (x lis)
    (if (null? lis)
        (cons x '())
        (cons (car lis) (add2end x (cdr lis))))))

; myreverse: takes a list of atoms and returns a list that is the reverse of theinput list
(define myreverse
  (lambda (lis)
    (cond
      ((null? lis) '())
      (else (add2end (car lis) (myreverse (cdr lis))))))) 