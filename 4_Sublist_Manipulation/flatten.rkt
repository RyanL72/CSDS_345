#lang racket

(define flatten
  (lambda (lis)
    (cond
      ((null? lis) '()) ; Base case: empty list is already flattened
      ((not (pair? lis)) (list lis)) ; Wrap scalars in a list
      (else (append (flatten (car lis)) (flatten (cdr lis))))))) ; Append flattened car and cdr
