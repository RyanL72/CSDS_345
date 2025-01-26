#lang racket


; sumnumbers* takes a list that may contain sublists, and returns the sum of all numbers in the list.
(define sumnumbers*
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((pair? (car lis)) (+(sumnumbers* (car lis)) (sumnumbers* (cdr lis))))
      ((integer? (car lis)) (+ (car lis) (sumnumbers* (cdr lis))))
      (else (sumnumbers* (cdr lis))))))