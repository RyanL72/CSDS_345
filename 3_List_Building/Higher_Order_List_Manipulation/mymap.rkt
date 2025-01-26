#lang racket

; mymap: takes a list of atoms and function and returns a list that is the result of applying the function to each element of the input list.
(define mymap
  (lambda (lis function)
    (if (null? lis) '() 
    (cons (function (car lis)) (mymap (cdr lis) function)))))