#lang racket

(define myreplaceall*
  (lambda (x y lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (myreplaceall* x y (car lis)) (myreplaceall* x y (cdr lis))))
      ((eq? x (car lis)) (cons y (myreplaceall* x y (cdr lis))))
      (else (cons (car lis) (myreplaceall* x y (cdr lis))))))) 