#lang racket

;multiplyby takes a number and a list of numbrs and returns a list that is the input list with each element multiplied by the input number.

(define multiplyby
  (lambda (x lis)
    (if (null? lis) '()
      (cons (* x (car lis)) (multiplyby x (cdr lis))))))

; maxnnumber takes a list of numbers that contains at least one number and returns the largest number in the list
(define maxnumber
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((> (car lis) (maxnumber (cdr lis))) (car lis))
      (else (maxnumber (cdr lis))))))

;removelast takes a list of atoms containing tat least one atom and returnns the same list minus the last atom of the list.
(define removelast
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) '())
      (else (cons (car lis) (removelast (cdr lis)))))))      

; crossmultiply takes two lists of numbers, each list represents a vector. Returns the outer product of the two vectors.
;               The outer product is a matrix ( a list of lists) and each list is the result of multiplying the second list by the corresponding value of the first list.

(define crossmultiply
  (lambda (x y)
    (cond
      ((null? x) '())
      ((null? y) '())
      ((list? x) (cons (crossmultiply (car x) y) (crossmultiply (cdr x) y)))
      ((list? y) (cons (crossmultiply x (car y)) (crossmultiply x (cdr y))))
      (else ( * x y)))))