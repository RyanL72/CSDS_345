#lang racket

;myreplace: takes two atoms and a list of atoms, and replaces the first occurence of the first atom in the list with the second atom
(define myreplace
  (lambda (x y list)
    (cond
      ((null? list) '())
      ((eq? (car list) x) (cons y (cdr list)))
      (else (cons (car list) (myreplace x y (cdr list)))))))