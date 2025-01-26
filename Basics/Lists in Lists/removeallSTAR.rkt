#lang racket

; removeall" : takes an atom ( not the emtpy list) and a list that contains sublists
;              and removes all occurrences of the given atom from the list
(define removeall*
 (lambda (x lis)
   (cond
     ((null? lis) '())
     ((list? (car lis)) (cons (removeall* x (car lis)) (removeall* x (cdr lis))))
     ((eq? x (car lis)) (removeall* x (cdr lis)))
     (else (cons (car lis) (removeall* x (cdr lis)))))))