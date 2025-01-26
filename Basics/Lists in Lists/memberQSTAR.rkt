#lang racket

;member*?: takes an atom ( may be the empty list and a list that contain sublists
;           return #t if the atom is in the list or any of its sublilsts
(define member*?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((pair? (car lis)) (or (member*? x (car lis)) (member*? x (cdr lis))))
      ({eq? x (car lis)} #t)
      (else (member*? x (cdr lis))))))