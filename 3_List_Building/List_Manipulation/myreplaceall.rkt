#lang racket

;myreplaceall takes two atoms and a list of atoms, and it replaces every occurence of the first atom in the list wth the second atom.
(define myreplaceall
  (lambda (x y list)
    (cond
      ((null? list) '())
      ((eq? (car list) x) (cons y (myreplaceall x y (cdr list))))
      (else (cons (car list) (myreplaceall x y (cdr list)))))))